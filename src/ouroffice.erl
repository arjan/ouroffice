-module(ouroffice).

-export([start/0, stop/0, get_env/2]).

start() ->
    start(?MODULE).

-spec start(atom()) -> ok | {error, term()}.
start(App) ->
    case application:start(App) of
        {error, {not_started, Dep}} ->
            ok = start(Dep),
            ok = start(App);
        Other ->
            Other
    end.



-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(redgage_friend).


-spec get_env(atom(), list() | integer()) -> list() | integer().
get_env(Field, Default) ->
    case application:get_env(?MODULE, Field) of
        {ok, Value} -> Value;
        _ -> Default
    end
.
