-module(ouroffice_logic).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(USER_TIMEOUT, 120000).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, hosts_update/1]).

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

hosts_update(List) ->
    gen_server:cast(?SERVER, {hosts_update, List}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({hosts_update, List}, State) ->
    lager:debug("Hosts update, ~p", [List]),
    [update_host(H) || H <- List],
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


update_host(Host) ->
    All = ouroffice:get_env(hostname_to_user, []),
    Hostname = proplists:get_value(hostname, Host),
    case proplists:lookup(Hostname, All) of
        {Hostname, Username} ->
            case buffalo:queue(ouroffice_notifier, user_offline, [Username], ouroffice:get_env(user_timeout, ?USER_TIMEOUT)) of
                {ok, new} ->
                    ouroffice_notifier:user_online(Username);
                {ok, existing} ->
                    nop
            end;
        none ->
            nop
    end.
