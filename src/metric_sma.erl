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
new(N, Interval) ->
    #sma{n = N, interval = Interval}.

-spec rate(sma()) -> Rate :: float().
rate(#sma{rate = Rate}) ->
    Rate.

-spec update(SMA :: sma(), Value :: float()) -> sma().
update(#sma{total = Total} = SMA, Value) ->
    SMA#sma{total = Total + Value}.

-spec tick(sma()) -> sma().
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
