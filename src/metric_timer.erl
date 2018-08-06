-module(metric_timer).
-export([
    start_link/1,
    tick/0
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

start_link(Interval) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Interval, []).

tick() ->
    lists:foreach(fun metric_worker:tick/1, metric_worker:all()).

%% @private
init(Interval) ->
    IntervalMillis = Interval * 1000,
    timer:apply_interval(IntervalMillis, ?MODULE, tick, []).

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, TRef) ->
    {ok, cancel} = timer:cancel(TRef),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
