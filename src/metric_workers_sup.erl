-module(metric_workers_sup).
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
        strategy  => simple_one_for_one,
        intensity => 0,
        period    => 1
    },
    ChildSpec = #{
      id       => metric_worker,
      start    => {metric_worker, start_link, [SmoothInterval, Interval]},
      restart  => temporary
    },
    {ok, {SupFlags, [ChildSpec]}}.
