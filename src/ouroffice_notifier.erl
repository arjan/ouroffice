-module(ouroffice_notifier).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         user_online/1,
         user_online/2,
         user_offline/1,
         twitter_status/1,
         day_part/0
        ]).

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

user_online(User) ->
    user_online(User, false).

user_online(_User, true) ->
    ignore;
user_online({Username, Gender}, false) ->
    %% backward comp
    user_online([{name, Username}, {gender, Gender}], false);
user_online(User, false) ->
    gen_server:cast(?SERVER, {notify_online, User}).

user_offline({Username, _Gender}) ->
    lager:info("User offline.... ~p", [Username]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({notify_online, User}, State) ->
    Username = proplists:get_value(name, User),
    Gender = proplists:get_value(gender, User),
    Reasons = online_messages(day_part()),
    Template = lists:nth(random:uniform(length(Reasons)), Reasons),
lager:warning("Template: ~p", [Template]),
    {HeShe, GuyGirl} = case Gender of
                           undefined -> {<<"it">>, <<"person">>};
                           m -> {<<"he">>, <<"dude">>};
                           f -> {<<"she">>, <<"girl">>}
            end,
    Replace = [{username, Username}, {heshe, HeShe}, {guygirl, GuyGirl}],
lager:warning("Replace: ~p", [Replace]),
    Status = lists:foldl(fun({K, V}, Acc) ->
                                 re:replace(Acc, "{" ++ atom_to_list(K) ++ "}", V, [{return, list}])
                         end,
                         Template,
                         Replace),
lager:warning("Status: ~p", [Status]),                           
    twitter_status(Status),
    lager:info("Posted: ~s", [Status]),
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

twitter_consumer() ->
    {ouroffice:get_env(twitter_ckey, ""),
     ouroffice:get_env(twitter_csec, ""),
     hmac_sha1}.

twitter_status(Update) ->
    case oauth:post("https://api.twitter.com/1.1/statuses/update.json",
                    [{"status", Update}],
                    twitter_consumer(),
                    ouroffice:get_env(twitter_token, ""),
                    ouroffice:get_env(twitter_secret, "")) of
        {ok, {{_, 200, _}, _Header, _Body}} ->
            lager:info("Tweet update ok."),
            ok;
        R ->
            lager:warning("Twitter error: ~p", [R]),
            error
    end.


day_part() ->
    case calendar:local_time() of
        {_, {H, _, _}} when H >= 0, H < 7 ->
            night;
        {_, {H, _, _}} when H >= 7, H < 12 ->
            morning;
        {_, {H, _, _}} when H >= 12, H < 15 ->
            noon;
        {_, {H, _, _}} when H >= 15, H < 19 ->
            afternoon;
        {_, {H, _, _}} when H >= 19, H =< 23 ->
            evening
    end.

                        
online_messages(night) ->
    ["Do I see {username} sneaking in the office there? Go home, dude!"];
    
online_messages(morning) ->
    ["{username} appeared at the office. Early start!",
     "I see {username} is up early at @ouroffice... give that {guygirl} some coffee!",
     "Is {username} early for work, or did {heshe} never leave?"];
online_messages(noon) ->
    ["Hey, {username} decided to do some work here. Just in time for lunch!",
     "{username} appeared at the office. Did {heshe} bring lunch?"
    ];
online_messages(afternoon) ->
    ["Hey, {username} decided to show up. Just in time for beer!",
     "Maybe {username} is still hungover from yesterday? {heshe} only just arrived at the office...",
     "{username} appeared at the office. Better late than never.. :-)"
    ];
online_messages(evening) ->
    [
     "{username} is clearly on an interesting working schedule, {heshe} just came in!"
    ].
