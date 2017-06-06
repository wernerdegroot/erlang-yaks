-module(herd).

-export(
    [ test/0
    ]).

herd(Yaks, Stock, Time) ->
  {herd, Yaks, Stock, Time}.

yaks({herd, Yaks, _, _}) -> Yaks.
stock({herd, _, Stock, _}) -> Stock.
time({herd, _, _, Time}) -> Time.

withYaks(Yaks, {herd, _, Stock, Time}) -> herd(Yaks, Stock, Time).
withYaks(Yaks) -> fun(Herd) -> withYaks(Yaks, Herd) end.
withStock(Stock, {herd, Yaks, _, Time}) -> herd(Yaks, Stock, Time).
withStock(Stock) -> fun(Herd) -> withStock(Stock, Herd) end.
withTime(Time, {herd, Yaks, Stock, _}) -> herd(Yaks, Stock, Time).

advanceOneDay(Herd) ->
  F = funct:compose([fun advanceTime/1, fun ageHerd/1, fun shaveHerd/1, fun milkHerd/1]),
  F(Herd).

advanceDays(0, Herd) -> Herd;
advanceDays(N, Herd) -> advanceDays(N - 1, advanceOneDay(Herd)).

advanceTime(Herd) ->
  Time = time(Herd),
  AdvancedTime = time:advanceOneDay(Time),
  withTime(AdvancedTime, Herd).

ageHerd(Herd) ->
  Yaks = yaks(Herd),
  YaksDayOlder = lists:map(fun yak:advanceOneDay/1, Yaks),
  withYaks(YaksDayOlder, Herd).

milkHerd(Herd) ->
  stockProducingAction(fun yak:milk/1, Herd).

shaveHerd(Herd) ->
  stockProducingAction(fun yak:shave/1, Herd).

stockProducingAction(Action, Herd) ->
  AccumulateStock = fun(Yak, {Stock, Yaks}) ->
    {ProducedStock, ShavedYak} = Action(Yak),
    {stock:add(Stock, ProducedStock), [ShavedYak | Yaks]}
  end,
  Yaks = yaks(Herd),
  Stock = stock(Herd),
  {ProducedStock, YaksAfterShaving} = lists:foldr(
      AccumulateStock,
      {stock:empty(), []},
      Yaks),
  F = funct:compose(
      [ withStock(stock:add(Stock, ProducedStock))
      , withYaks(YaksAfterShaving)
      ]),
  F(Herd). 

test() ->
  Time = time:fromDays(0),
  Yaks =
    [ yak:yakOfAge("Betty-1", female, time:fromYears(4), Time)
    , yak:yakOfAge("Betty-2", female, time:fromYears(8), Time)
    , yak:yakOfAge("Betty-3", female, time:fromYears(9.5), Time)
    ],
  Herd = herd(Yaks, stock:empty(), time:fromDays(0)),
  advanceDays(14, Herd).
