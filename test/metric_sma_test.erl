-module(metric_sma_test).
-include_lib("eunit/include/eunit.hrl").

rate_test_() ->
    Input = [5.3, 6.7, 7.9, 7.1, 5.2, 4.1, 3.5, 5.4, 7.3, 9.4, 8.0, 6.6, 7.9, 9.2, 7.6],
    {SMAs, _} = lists:mapfoldl(fun(X, Acc) ->
        SMA = metric_sma:tick(metric_sma:update(Acc, X)),
        {SMA, SMA}
    end, metric_sma:new(4, 1), Input),
    Expexcted = ["undefined", "undefined", "undefined",
                 "6.7500", "6.7250", "6.0750", "4.9750", "4.5500", "5.0750",
                 "6.4000", "7.5250", "7.8250", "7.9750", "7.9250", "7.8250"],
    L = lists:zip(Expexcted, SMAs),
    [?_assertEqual(Exp, to_s(metric_sma:rate(SMA))) || {Exp, SMA} <- L].

to_s(undefined) ->
    "undefined";
to_s(Number) ->
    float_to_list(Number, [{decimals, 4}]).
