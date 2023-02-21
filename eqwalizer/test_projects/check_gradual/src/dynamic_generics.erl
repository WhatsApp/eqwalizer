%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(dynamic_generics).

-compile([export_all, nowarn_export_all]).

%% used later as
%% a producer of dynamic values
dyn_val() -> erlang:error(dynamic).

dyn_fun(_X) -> erlang:error(dynamic).

dyn_fun(_X, _Y) -> erlang:error(dynamic).

-spec inc(number()) -> number().
inc(I) -> I + 1.

-spec add(number(), number())
-> number().
add(X, Y) -> X + Y.

-spec case01() -> [number()].
case01() ->
  Ns = dyn_val(),
  lists:map(fun inc/1, Ns).

-spec case02_neg() -> ok.
case02_neg() ->
  Ns = dyn_val(),
  lists:map(fun inc/1, Ns).

-spec case03() -> [number()].
case03() ->
  Ns = [0, 1, 2],
  lists:map(fun dyn_fun/1, Ns).

-spec case04_neg() -> ok.
case04_neg() ->
  Ns = [0, 1, 2],
  lists:map(fun dyn_fun/1, Ns).

-spec case05() -> number().
case05() ->
  Ns = dyn_val(),
  lists:foldl(fun add/2, 0, Ns).

-spec case05_neg() -> ok.
case05_neg() ->
  Ns = dyn_val(),
  lists:foldl(fun add/2, 0, Ns).

-spec case06() -> number().
case06() ->
  Ns = [0, 1, 2],
  Acc = dyn_val(),
  lists:foldl(fun add/2, Acc, Ns).

-spec case07() -> none().
case07() ->
  Ns = [0, 1, 2],
  Acc = dyn_val(),
  lists:foldl(fun add/2, Acc, Ns).

-spec case08_neg() -> ok.
case08_neg() ->
  Ns = [0, 1, 2],
  Acc = dyn_val(),
  Res = lists:foldl(fun add/2, Acc, Ns),
  {Res}.

-spec case09_neg() -> ok.
case09_neg() ->
  Ns = [0, 1, 2],
  Acc = "string",
  lists:foldl(fun dyn_fun/2, Acc, Ns).

-spec fapply(fun((T) -> U), T) -> U.
fapply(F, X) ->
  ZZ = F(X),
  ZZ.

-spec case10_neg() -> ok.
case10_neg() ->
  fapply(fun inc/1, dyn_val()).

-spec case11_neg(X) -> X.
case11_neg(X) ->
  Res = fapply(fun dyn_fun/1, X),
  {Res}.

-spec named1() -> [integer()].
named1() ->
  lists:map(
    fun
      Fib(N) when N < 2 ->
        1;
      Fib(N) ->
        Fib(N - 2) + Fib(N - 1)
    end,
    [1, 2, 3]
  ).

-spec named2_neg() -> [integer()].
named2_neg() ->
  lists:map(
    fun
      Fib(N) when N < 2
        -> 1;
      Fib(3) ->
        three;
      Fib(N) ->
        Fib(N - 2) + Fib(N - 1)
    end,
    [1, 2, 3]
  ).

-spec named3_neg() -> [integer()].
named3_neg() ->
  lists:map(
    fun
      Fib(N) when N < 2
        -> 1;
      Fib(N) ->
        Fib(undefined) + Fib(N - 1)
    end,
    [1, 2, 3]
  ).

-spec step(
    number(),
    none | {some, number()}
) -> none | {some, number()}.
step(X, none) when X > 100 -> {some, X};
step(_, none) -> none;
step(X, {some, Acc}) when Acc == 1000 ->
  step(X, none);
step(X, {some, Acc}) ->
  {some, X + Acc}.

-spec game1([number()])
      -> none | {some, number()}.
game1(L) ->
  lists:foldl(fun step/2, {some, 0}, L).

-spec game2([number()])
      -> none | {some, number()}.
game2(L) ->
  lists:foldl(
    fun
      Step(X, none) when X > 100 ->
        {some, X};
      Step(_, none) ->
        none;
      Step(X, {some, Acc})
        when Acc == 1000 ->
        Step(X, none);
      Step(X, {some, Acc}) ->
        {some, X + Acc}
    end,
    {some, 0},
    L
  ).

-spec either({T, T}, T) -> T.
either({_, _}, A) -> A.

-spec unify_dyn_atom(eqwalizer:dynamic(), undef) -> ok.
unify_dyn_atom(D, A) -> eqwalizer:reveal_type(either(D, A)).
