%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(funs_uncommon).
-compile([export_all, nowarn_export_all]).

% Uncommon uses of lambdas

-spec test_01(number()) -> number().
test_01(N) ->
  F = case N of
        0 -> fun (_) -> 1 end;
        _ -> fun (X) -> X + 1 end
      end,
  F(1).

-record(rectup, {field :: {fun((a) -> a), atom()}}).

-spec test_02() -> #rectup{}.
test_02() ->
  #rectup{
    field = {fun(X) -> X end, an_atom}
  }.

-record(rec, {field :: fun((a) -> a)}).

-spec test_03() -> #rec{}.
test_03() ->
  #rec{
    field = fun(X) -> X end
  }.

-spec test_04(#rec{}) -> #rec{}.
test_04(R) ->
  R#rec{
    field = {fun(X) -> X end, an_atom}
  }.

-spec test_05() -> #{key := fun((a) -> a)}.
test_05() ->
  #{key => fun(X) -> X end}.

-spec test_06(
   #{key := fun((a) -> a)}
) -> #{key := fun((a) -> a)}.
test_06(M) ->
  M#{key => fun(X) -> X end}.

-spec test_07() -> {fun((a) -> a)}.
test_07() ->
  {fun(X) -> X end}.

-spec test_08() -> {fun((a) -> a)}.
test_08() ->
  Res = {fun(X) -> X end},
  Res.

-spec test_09() -> b.
test_09() ->
  {F, _} = {fun(X) -> X end, a},
  F(b).

-spec test_10() -> [fun((a) -> a)].
test_10() ->
  [fun(X) -> X end, fun(X) -> X end].

-spec test_11() -> [fun((a, b) -> a)].
test_11() ->
  Res = [fun(X, _) -> X end],
  Res.

-spec test_12(a) -> fun((b, c) ->
                      {a, b, c}).
test_12(a) ->
  fun(b, c) -> {a, b, c} end.

-spec test_13() ->
  [fun((number()) -> number())].
test_13() ->
  [
    fun(X) -> X * Y end
    || Y <- [1, 2, 3]
  ].

-spec test14() -> fun((a) -> a).
test14() ->
  try
    erlang:display(ok)
  of
    ok ->
      fun(X) -> X end
  catch
    error ->
      fun(X) -> X end
  end.

-spec test15() -> ok.
test15() ->
  fun(X) -> X end, fun(X) -> X end,
  ok.

-spec test16() -> number().
test16() ->
  F = fun(X) -> X end,
  _ = F(2),
  F(3).

-spec test17() -> number().
test17() ->
  F = fun(X) -> X end,
  _ = F(an_atom),
  F(3).
