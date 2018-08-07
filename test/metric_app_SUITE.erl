-module(metric_app_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([
    all/0
]).

-export([
    benchmark/1
]).

all() ->
    [benchmark].

benchmark(_Config) ->
    ok = application:start(gproc),
    {ok, _} = metric_workers_sup:start_link(3, 1),
    {Time, Value} = timer:tc(fun work/1, [10000]),
    ?assertEqual(2.0, Value),
    ?assert(Time < 500000),
    true = exit(whereis(metric_workers_sup), normal),
    ok = application:stop(gproc).

work(N) ->
    Metrics = lists:map(fun(X) ->
        iolist_to_binary(["Metric", integer_to_binary(X)])
    end, lists:seq(1, N)),
    lists:foreach(fun(MetricName) -> metric_app:report(MetricName, 1.5) end, Metrics),
    metric_timer:tick(),
    lists:foreach(fun(MetricName) -> metric_app:report(MetricName, 2.0) end, Metrics),
    metric_timer:tick(),
    lists:foreach(fun(MetricName) -> metric_app:report(MetricName, 2.5) end, Metrics),
    metric_timer:tick(),
    metric_app:average(<<"Metric999">>).
