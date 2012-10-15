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

user_online(Username) ->
    user_online(Username, false).

user_online(_Username, true) ->
    ignore;
user_online(Username, false) ->
    lager:info("User online!!!! ~p", [Username]),
    gen_server:cast(?SERVER, {notify_online, Username}).

user_offline(Username) ->
    lager:info("User offline.... ~p", [Username]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({notify_online, Username}, State) ->
    Reasons = online_messages(day_part()),
    Template = lists:nth(random:uniform(length(Reasons)), Reasons),
    Status = lists:flatten(io_lib:format(Template, [Username])),
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
                    [{status, Update}],
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
        {_, {H, _, _}} when H >= 0, H =< 6 ->
            night;
        {_, {H, _, _}} when H >= 7, H =< 12 ->
            morning;
        {_, {H, _, _}} when H >= 13, H =< 17 ->
            afternoon;
        {_, {H, _, _}} when H >= 18, H =< 23 ->
            evening
    end.

                        
online_messages(night) ->
    ["Do I see ~s sneaking in the office there? Go home, dude!"];
    
online_messages(morning) ->
    ["~s appeared at the office. Early start!",
     "I see ~s is up early at @ouroffice... give that guy some coffee!",
     "Is ~s early for work, or did he never leave?"];
online_messages(afternoon) ->
    ["Hey, ~s decided to do some work here. Just in time for lunch!",
     "~s appeared at the office. Better late than never.. :-)"
    ];
online_messages(evening) ->
    ["~s is clearly on an interesting working schedule, he just came in!"].
