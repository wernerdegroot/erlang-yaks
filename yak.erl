-module(yak).

-export(
    [ newBorn/3
    , yakOfAge/4
    , name/1
    , age/1
    , sex/1
    , advanceOneDay/1
    , advanceDays/2
    , milk/1
    , shave/1
    ]).

-define(AGE_IN_YEARS_ADULT, 1).
-define(AGE_IN_YEARS_DEAD, 10).

newBorn(Name, Sex, Now) ->
  {yak, {young_yak, Name, time:fromDays(0), Sex, option:none(), Now}}.

yakOfAge(Name, Sex, DesiredAge, Now) ->
  NewBorn = newBorn(Name, Sex, Now),
  advanceDays(time:inDays(DesiredAge), NewBorn).

name({yak, {_, Name, _, _, _, _}}) -> Name.
age({yak, {_, _, Age, _, _, _}}) -> Age.
sex({yak, {_, _, _, Sex, _, _}}) -> Sex.

advanceOneDay({yak, {_, Name, Age, Sex, TimeLastShaved, Time}}) ->
  NewAge = time:advanceOneDay(Age),
  NewTime = time:advanceOneDay(Time),
  NewType = typeOfYak(NewAge),
  {yak, {NewType, Name, NewAge, Sex, TimeLastShaved, NewTime}}.

advanceDays(0, Yak) -> Yak;
advanceDays(N, Yak) -> advanceDays(N - 1, advanceOneDay(Yak)).

typeOfYak(Age) ->
  case time:inYears(Age) of
    X when X >= ?AGE_IN_YEARS_DEAD -> dead_yak;
    X when X >= ?AGE_IN_YEARS_ADULT -> adult_yak;
    _ -> young_yak
  end.

milk(Yak = {yak, {dead_yak, _, _, _, _, _}}) ->
  {stock:empty(), Yak};
milk(Yak) ->
  Age = age(Yak),
  ProducedMilk = 50 - time:inDays(Age) * 0.03,
  ProducedStock = stock:stock(ProducedMilk, 0.0),
  {ProducedStock, Yak}.

shave(Yak = {yak, {adult_yak, Name, Age, Sex, TimeLastShaved, Time}}) ->
  DaysFromLastShaveToShaveAgain = ceil(8.0 + time:inDays(Age) * 0.01),
  TimeFromLastShaveToShaveAgain = time:fromDays(DaysFromLastShaveToShaveAgain),
  TimeShaveAgain = option:fold
    ( Time 
    , fun(T) -> time:plus(T, TimeFromLastShaveToShaveAgain) end
    , TimeLastShaved
    ),
  case time:lte(TimeShaveAgain, Time) of
    true ->
      ProducedStock = stock:stock(0.0, 1),
      ShavedYak = {yak, {adult_yak, Name, Age, Sex, option:some(Time), Time}},
      {ProducedStock, ShavedYak};
    false ->
      {stock:empty(), Yak}
    end;
shave(Yak) ->
  {stock:empty(), Yak}.

ceil(X) -> trunc(X + 1).
  
