-module(metric_app).
-export([
    report/2,
    average/1
]).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    {ok, SmoothInterval} = application:get_env(smooth_interval_seconds),
    {ok, Interval} = application:get_env(interval_seconds),
    metric_sup:start_link(SmoothInterval, Interval).

stop(_State) ->
    ok.

-spec report(MetricName :: binary(), MetricValue :: float()) -> ok.
report(MetricName, MetricValue) ->
    Worker = metric_worker:find_or_create(MetricName),
    metric_worker:report(Worker, MetricValue).

-spec average(MetricName :: binary()) -> float().
average(MetricName) ->
    Worker = metric_worker:find_or_create(MetricName),
    metric_worker:average(Worker).
