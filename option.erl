-module(option).

-export(
    [ some/1
    , none/0
    , fmap/2
    , getOrElse/2
    , fold/3
    ]).

some(Value) -> {option, {some, Value}}.
none() -> {option, none}.

fmap(F, {option, {some, Value}}) -> some(F(Value));
fmap(_, {option, none}) -> none().

getOrElse(_, {option, {some, Value}}) -> Value;
getOrElse(Default, {option, none}) -> Default.

fold(Default, F, Option) -> getOrElse(Default, fmap(F, Option)).