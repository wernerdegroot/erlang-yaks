-module(time).

-export(
    [ fromDays/1
    , fromYears/1
    , inYears/1
    , inDays/1
    , advanceOneDay/1
    , advanceDays/2
    , gt/2
    , lt/2
    , gte/2
    , lte/2
    , plus/2
    ]).

-define(NUMBER_OF_DAYS_IN_YEAR, 100).

fromDays(NumberOfDays) ->
  {days, NumberOfDays}.

fromYears(NumberOfYears) ->
  fromDays(trunc(NumberOfYears * ?NUMBER_OF_DAYS_IN_YEAR)).

inYears({days, NumberOfDays}) ->
  NumberOfDays / ?NUMBER_OF_DAYS_IN_YEAR.

inDays({days, NumberOfDays}) ->
  NumberOfDays.

advanceOneDay({days, NumberOfDays}) ->
  {days, NumberOfDays + 1}.

advanceDays(ToAdvance, {days, NumberOfDays}) ->
  {days, NumberOfDays + ToAdvance}.

gt({days, NumberOfDaysLeft}, {days, NumberOfDaysRight}) ->
  NumberOfDaysLeft > NumberOfDaysRight.

lt({days, NumberOfDaysLeft}, {days, NumberOfDaysRight}) ->
  NumberOfDaysLeft < NumberOfDaysRight.

gte(TimeLeft, TimeRight) ->
  not lt(TimeLeft, TimeRight).

lte(TimeLeft, TimeRight) ->
  not gt(TimeLeft, TimeRight).

plus({days, DaysLeft}, {days, DaysRight}) ->
  {days, DaysLeft + DaysRight}.