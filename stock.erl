-module(stock).

-export(
    [ stock/2
    , litersMilk/1
    , skinsOfWool/1
    , empty/0
    , add/2
    ]).

stock(LitersMilk, SkinsOfWool) -> {stock, LitersMilk, SkinsOfWool}.

litersMilk({stock, LitersMilk, _}) -> LitersMilk.
skinsOfWool({stock, _, SkinsOfWool}) -> SkinsOfWool.

empty() -> stock(0.0, 0.0).

add(Left, Right) -> stock(
  litersMilk(Left) + litersMilk(Right),
  skinsOfWool(Left) + skinsOfWool(Right)).

