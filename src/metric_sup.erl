-module(metric_sup).
-export([start_link/2]).

-behaviour(supervisor).
-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link(SmoothInterval :: integer(), Interval :: integer()) -> {ok, pid()}.
start_link(SmoothInterval, Interval) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {SmoothInterval, Interval}).

%% @private
init({SmoothInterval, Interval}) ->
    SupFlags = #{
        strategy  => one_for_one,
        intensity => 0,
        period    => 1
    },
    WorkersSupChildSpec = #{
        id      => metric_workers_sup,
        start   => {metric_workers_sup, start_link, [SmoothInterval, Interval]},
        restart => permanent,
        type    => supervisor
    },
    TimerChildSpec = #{
        id      => metric_timer,
        start   => {metric_timer, start_link, [Interval]},
        restart => permanent
    },
    {ok, {SupFlags, [WorkersSupChildSpec, TimerChildSpec]}}.
