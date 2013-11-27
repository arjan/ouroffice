-module(ouroffice_scanner).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(POLL_INTERVAL, 30000).

-include_lib("xmerl/include/xmerl.hrl").

-record(state, {hosts=undefined, ip_to_host=[]}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get_hosts/0, macaddr/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_hosts() ->
    gen_server:call(?SERVER, get_hosts, 30000).

macaddr(Addr) ->
    {ok, Ip} = gen_server:call(?SERVER, {macaddr, Addr}, 30000),
    Ip.


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    lager:info("Scanner starting."),
    State1 = queue_scan(#state{}, 0),
    {ok, State1}.

handle_call(get_hosts, _From, State=#state{hosts=Hosts}) ->
    {reply, {ok, Hosts}, State};

handle_call({macaddr, "127.0.0.1"}, _From, State) ->
    {reply, {ok, <<"localhost">>}, State};
handle_call({macaddr, Addr0}, _From, State=#state{ip_to_host=IpToHost}) ->
    Addr = z_convert:to_binary(Addr0),
    Reply = case proplists:lookup(Addr, IpToHost) of
                {Addr, HostInfo} ->
                    {ok, proplists:get_value(mac, HostInfo)};
                none ->
                    {ok, undefined}
            end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(scan, State=#state{hosts=PrevHosts}) ->
    os:cmd("nmap -oX /tmp/ouroffice.xml -sP " ++ ouroffice:get_env(subnet, "192.168.10.0/24")),
    lager:debug("Scan done."),
    {RootElem, _} = xmerl_scan:file("/tmp/ouroffice.xml"),
    Hosts = [hostinfo(H, State#state.ip_to_host) || H <- xmerl_xpath:string("//host[status[@state=\"up\"]]", RootElem)],
    ouroffice_logic:hosts_update(PrevHosts, Hosts),
    IpToHostNew = [ {proplists:get_value(addr, I), I} || I <- Hosts],
    {noreply, queue_scan(State#state{hosts=Hosts, ip_to_host=IpToHostNew})};

handle_info(_Info, State) ->
    lager:warning("Unhandled info message: ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

queue_scan(State) ->
    queue_scan(State, ouroffice:get_env(poll_interval, ?POLL_INTERVAL)).

queue_scan(State, T) ->
    erlang:send_after(T, self(), scan),
    State.

hostinfo(HostElem, IpToHost) ->
    [AddrElem] = xmerl_xpath:string("address", HostElem),
    Addr = xml_attrib(addr, AddrElem),
               
    FallbackHostname = case xmerl_xpath:string("hostnames/hostname", HostElem) of
                           [] -> undefined;
                           [HostnameElem|_] -> xml_attrib(name, HostnameElem)
                       end,

    case proplists:lookup(Addr, IpToHost) of
        {Addr, HostInfo} ->
            HostInfo;
        none ->
            X = os:cmd(binary_to_list(iolist_to_binary(["arp -a ", Addr]))),
            {Hostname, Mac} = case string:tokens(X, " ") of
                                  [] -> {FallbackHostname, undefined};
                                  ["arp:"|_] -> {FallbackHostname, undefined};
                                  [H, _, _, M|_] -> {list_to_binary(H), list_to_binary(M)}
                              end,
            [{addr, Addr},
             {mac, Mac},
             {hostname, Hostname}]
    end.

xml_attrib(Name, #xmlElement{attributes=Attrs}) ->
    case lists:filter(fun(#xmlAttribute{name=Nm}) -> Nm =:= Name end, Attrs) of
        [] -> undefined;
        [#xmlAttribute{value=Value}|_] ->
            list_to_binary(Value)
    end.
