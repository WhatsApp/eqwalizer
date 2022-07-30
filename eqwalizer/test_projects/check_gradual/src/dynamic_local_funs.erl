%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(dynamic_local_funs).

-compile([export_all, nowarn_export_all]).

-spec sum([{number(), number()}])
      -> [number()].
sum(L) ->
  F = fun({N1, N2}) -> N1 + N2 end,
  lists:map(F, L).

-spec fold1
([{number(), number()}]) -> number().
fold1(L) ->
  F = fun({N1, N2}, Acc) ->
    N1 + N2 + Acc
  end,
  lists:foldl(F, 0, L).

-spec fold2_neg
([{number(), number()}]) -> atom().
fold2_neg(L) ->
  F = fun({N1, N2}, Acc) ->
    N1 + N2 + Acc
  end,
  lists:foldl(F, 0, L).

-spec fold3_neg
([{number(), number()}]) -> number().
fold3_neg(L) ->
  F = fun({N1, N2}, Acc) ->
    N1 + N2 + binary_to_atom(Acc)
  end,
  lists:foldl(F, 0, L).

-spec test() -> fun((a) -> b).
test() ->
  X = fun(a) -> b end,
  X.

-spec test_arity_neg() -> fun((a) -> b).
test_arity_neg() ->
  X = fun(a, _) -> b end,
  X.