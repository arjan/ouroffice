-module(ouroffice_logic).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(USER_TIMEOUT, 120000).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, hosts_update/2]).

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

hosts_update(PrevList, List) ->
    gen_server:cast(?SERVER, {hosts_update, PrevList, List}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({hosts_update, PrevList, List}, State) ->
    lager:debug("Hosts update, ~p", [List]),
    First = PrevList =:= undefined,
    [update_host(H, First) || H <- List],
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

update_host(Host, First) ->
    LookupFun = ouroffice:get_env(mac_lookup, fun lookup_user/1),
    NotifierMod = ouroffice:get_env(notifier_module, ?MODULE),
    MacAddr = proplists:get_value(mac, Host),
    case LookupFun(MacAddr) of
        undefined ->
            nop;
        User ->
            case buffalo:queue(MacAddr, NotifierMod, user_offline, [User], ouroffice:get_env(user_timeout, ?USER_TIMEOUT)) of
                {ok, new} ->
                    NotifierMod:user_online(User, First);
                {ok, existing} ->
                    nop
            end
    end.


lookup_user(MacAddr) ->
    All = ouroffice:get_env(mac_to_user, []),
    proplists:get_value(MacAddr, All).
