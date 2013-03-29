-module(ouroffice_scanner).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(POLL_INTERVAL, 30000).

-include_lib("xmerl/include/xmerl.hrl").

-record(state, {hosts=undefined}).

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
    
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    lager:info("Scanner starting."),
    State1 = queue_scan(#state{}, 0),
    {ok, State1}.

handle_call(get_hosts, _From, State=#state{hosts=Hosts}) ->
    {reply, {ok, Hosts}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(scan, State=#state{hosts=PrevHosts}) ->
    os:cmd("nmap -oX /tmp/ouroffice.xml -sP " ++ ouroffice:get_env(subnet, "192.168.10.0/24")),
    lager:debug("Scan done."),
    {RootElem, _} = xmerl_scan:file("/tmp/ouroffice.xml"),
    Hosts = [hostinfo(H) || H <- xmerl_xpath:string("//host[status[@state=\"up\"]]", RootElem)],
    ouroffice_logic:hosts_update(PrevHosts, Hosts),
    {noreply, queue_scan(State#state{hosts=Hosts})};

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

hostinfo(HostElem) ->
    [AddrElem] = xmerl_xpath:string("address", HostElem),
    Addr = xml_attrib(addr, AddrElem),
               
    FallbackHostname = case xmerl_xpath:string("hostnames/hostname", HostElem) of
                           [] -> undefined;
                           [HostnameElem|_] -> xml_attrib(name, HostnameElem)
                       end,

    X = os:cmd(binary_to_list(iolist_to_binary(["arp -a ", Addr]))),
    {Hostname, Mac} = case string:tokens(X, " ") of
                          [] -> {FallbackHostname, undefined};
                          ["arp:"|_] -> {FallbackHostname, undefined};
                          [H, _, _, M|_] -> {list_to_binary(H), list_to_binary(M)}
                      end,
    [{addr, Addr},
     {mac, Mac},
     {hostname, Hostname}].

xml_attrib(Name, #xmlElement{attributes=Attrs}) ->
    case lists:filter(fun(#xmlAttribute{name=Nm}) -> Nm =:= Name end, Attrs) of
        [] -> undefined;
        [#xmlAttribute{value=Value}|_] ->
            list_to_binary(Value)
    end.


macaddr("127.0.0.1") ->
    <<"localhost">>;
macaddr(Addr) ->
    X = os:cmd(binary_to_list(iolist_to_binary(["arp -a ", Addr]))),
    case string:tokens(X, " ") of
        [] -> undefined;
        ["arp:"|_] -> undefined;
        [_H, _, _, M|_] -> list_to_binary(M)
    end.
