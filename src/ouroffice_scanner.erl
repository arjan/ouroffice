-module(ouroffice_scanner).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(POLL_INTERVAL, 30000).

-record(state, {hosts=[], ip_to_host=[]}).

-type host_result() :: {Mac::binary(), Addr::binary()}.

-callback scan(Opts::term()) ->
    [host_result()].

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

handle_info(scan, State) ->
    Parent = self(),
    proc_lib:spawn_link(fun() -> do_scan(Parent) end),
    {noreply, State};

handle_info({scan_done, Hosts, IpToHost}, State=#state{hosts=PrevHosts}) ->
    lager:warning("scan_done: ~p", [scan_done]),
    ouroffice_logic:hosts_update(PrevHosts, Hosts),
    {noreply, queue_scan(State#state{hosts=Hosts, ip_to_host=IpToHost})};

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


do_scan(Parent) ->

    lager:debug("Starting network scan."),
    Start = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    
    Self = self(),
    {ok, ScanModules} = application:get_env(ouroffice, scanners),
    Workers = [spawn(fun() ->
                             R = Module:scan(Config),
                             Self ! {done, R}
                     end)
               || {Module, Config} <- ScanModules],
                             
    Hosts = flatten1([receive {done, Hosts} -> Hosts end || _ <- lists:seq(1, length(Workers))]),
    
    IpToHostNew = [ {proplists:get_value(addr, I), I} || I <- Hosts],

    Stop = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    lager:info("Scan done. ~p hosts seen, took ~p secs.", [length(Hosts), Stop-Start]),

    Parent ! {scan_done, Hosts, IpToHostNew},
    ok.
  

flatten1(R) ->
    flatten1(R, []).

flatten1([], Acc) ->
    Acc;
flatten1([List|Rest], Acc) ->
    flatten1(Rest, List ++ Acc).


    
