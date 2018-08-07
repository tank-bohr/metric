%% @doc Simple Moving Average
%%
%% [https://en.wikipedia.org/wiki/Moving_average#Simple_moving_average]
%%
%% This module implements SMA calculation logic. The same way as folsom does for EMA
%% See [https://github.com/boundary/folsom/blob/8914823067c623d2839ecd6d17785ba94ad004c8/src/folsom_ewma.erl]
%%
%% see the description of the algorithm here
%% [https://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:moving_averages#simple_moving_average_calculation]
%%
%% NOTE: For the first N values SMA is unavailable.
%%
%% @end
-module(metric_sma).

-export([
    new/2,
    rate/1,
    update/2,
    tick/1
]).

-export_type([
    sma/0
]).

-record(sma, {
    n           :: integer(),
    rate        :: undefined | float(),
    total = 0.0 :: float(),
    window = [] :: list(float()),
    interval    :: integer()
}).

-opaque sma() :: #sma{}.

-spec new(N :: integer(), Interval :: integer()) -> sma().
%% @doc Creates new SMA struct
%%
%% `N' - Smooth. the amount of data on the basis of which the average is calculated
%%
%% `Interval' - Time interval (in seconds) the timer ticks. Each tick we dump the next value.
%% @see tick/1
%%
%% @end
new(N, Interval) ->
    #sma{n = N, interval = Interval}.

-spec rate(sma()) -> Rate :: float().
rate(#sma{rate = Rate}) ->
    Rate.

-spec update(SMA :: sma(), Value :: float()) -> sma().
update(#sma{total = Total} = SMA, Value) ->
    SMA#sma{total = Total + Value}.

-spec tick(sma()) -> sma().
%% @doc Calculates the next SMA
%%
%% Here the calculation occurs
%%
%% - Dump a new value
%%
%% - Drop the oldest one out
%%
%% - Calculate a new SMA
%%
%% - Reset a total counter
%%
%% @end
tick(#sma{n = N} = SMA) ->
    InstantRate = instant_rate(SMA),
    Window = update_window(SMA, InstantRate),
    Rate = if
        length(Window) < N ->
            undefined;
        true ->
            lists:sum(Window) / N
    end,
    SMA#sma{rate = Rate, total = 0.0, window = Window}.

%% @private
instant_rate(#sma{total = Total, interval = Interval}) ->
    Total / Interval.

%% @private
update_window(#sma{n = N} = SMA, InstantRate) ->
    Window = [InstantRate | SMA#sma.window],
    if
        length(Window) > N ->
            lists:droplast(Window);
        true ->
            Window
    end.
