-module(metric_worker).
-export([
    all/0,
    find_or_create/1,
    start_link/3,
    tick/1,
    report/2,
    average/1
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

-record(state, {name, sma}).

-spec all() -> list(pid()).
all() ->
    gproc:select({l, n}, [{
        {                 %% Head begins
            {n, l, '$1'}, %% Key
            '$2',         %% Pid
            '$3'          %% Attrs
        },                %% Head ends
        [],               %% Guard
        ['$2']            %% Result
    }]).

-spec find_or_create(MetricName :: binary()) -> pid().
find_or_create(MetricName) ->
    case gproc:where({n, l, MetricName}) of
        undefined ->
            {ok, NewWorker} = supervisor:start_child(metric_workers_sup, [MetricName]),
            true = gproc:reg_other({n, l, MetricName}, NewWorker),
            NewWorker;
        ExistingWorker ->
            ExistingWorker
    end.

-spec start_link(SmoothInterval :: integer(), Interval :: integer(), Name :: binary()) -> {ok, pid()}.
start_link(SmoothInterval, Interval, Name) ->
    gen_server:start_link(?MODULE, {SmoothInterval, Interval, Name}, []).

-spec tick(Worker :: pid()) -> ok.
tick(Worker) ->
    gen_server:call(Worker, tick).

-spec report(Worker :: pid(), Value :: float()) -> ok.
report(Worker, Value) ->
    gen_server:call(Worker, {report, Value}).

-spec average(Worker :: pid()) -> undefined | float().
average(Worker) ->
    gen_server:call(Worker, average).

%% @private
init({SmoothInterval, Interval, Name}) ->
    N = SmoothInterval div Interval,
    SMA = metric_sma:new(N, Interval),
    {ok, #state{name = Name, sma = SMA}}.

%% @private
handle_call({report, Value}, _From, #state{sma = SMA} = State) ->
    {reply, ok, State#state{sma = metric_sma:update(SMA, Value)}};
handle_call(average, _From, #state{sma = SMA} = State) ->
    {reply, metric_sma:rate(SMA), State};
handle_call(tick, _From, #state{sma = SMA} = State) ->
    {reply, ok, State#state{sma = metric_sma:tick(SMA)}};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
