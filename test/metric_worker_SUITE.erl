-module(metric_worker_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    groups/0,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    all_test/1,
    find_or_create_test/1,
    sma_calculation_test/1
]).

all() ->
    [
        {group, finders},
        {group, worker}
    ].

groups() ->
    [{finders,
        [shuffle, sequence], [
            all_test,
            find_or_create_test
        ]
    },{worker,
        [shuffle, sequence], [
            sma_calculation_test
        ]
    }].

init_per_group(finders, Config) ->
    ok = application:start(gproc),
    {ok, _} = metric_workers_sup:start_link(5, 1),
    true = unlink(whereis(gproc)),
    true = unlink(whereis(metric_workers_sup)),
    Config;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(finders, _Config) ->
    ok = application:stop(gproc),
    true = exit(whereis(metric_workers_sup), normal),
    ok;
end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all_test(_Config) ->
    One = metric_worker:find_or_create(<<"One">>),
    Two = metric_worker:find_or_create(<<"Two">>),
    Three = metric_worker:find_or_create(<<"Three">>),
    ?assertEqual([One, Two, Three], lists:sort(metric_worker:all())),
    ok = terminate_children([One, Two, Three]).

find_or_create_test(_Config) ->
    RequstsMeter1 = metric_worker:find_or_create(<<"requests">>),
    RequstsMeter2 = metric_worker:find_or_create(<<"requests">>),
    MessagesMeter = metric_worker:find_or_create(<<"messages">>),
    ?assertEqual(RequstsMeter1, RequstsMeter2),
    ?assert(MessagesMeter =/= RequstsMeter1),
    ok = terminate_children([RequstsMeter1, MessagesMeter]).

sma_calculation_test(_Config) ->
    {ok, Worker} = metric_worker:start_link(5, 1, <<"metric">>),
    lists:foreach(fun (X) ->
        ok = metric_worker:report(Worker, X),
        ok = metric_worker:tick(Worker)
    end, [11, 12, 13, 14, 15]),
    ?assertEqual(13.0, metric_worker:average(Worker)),
    exit(Worker, normal).

terminate_children(Children) ->
    lists:foreach(fun (Pid) ->
        ok = supervisor:terminate_child(metric_workers_sup, Pid)
    end, Children).
