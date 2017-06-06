-module(funct).

-export(
  [ compose/2
  , identity/1
  , compose/1]).

compose(F, G) ->
  fun(X) -> F(G(X)) end.

identity(X) -> X.

compose(Fs) ->
  lists:foldr(fun compose/2, fun identity/1, Fs).