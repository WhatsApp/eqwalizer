%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(dyn_remote_funs).
-compile([nowarn_export_all, export_all]).


-spec test_01(atom()) -> term().
test_01(F) ->
    lists:F(fun(X) -> X end, [3]).

-spec test_02(atom()) -> term().
test_02(F) ->
    Res = lists:F(
        fun(X) -> X end,
        [3]
    ),
    Res.

-spec test_03(atom()) -> term().
test_03(L) ->
    L:map(fun(X) -> X end, [3]).

-spec test_04(atom()) -> term().
test_04(L) ->
    Res = L:map(
        fun(X) -> X end,
        [3]
    ),
    Res.

-spec test_04(atom(), atom()) -> term().
test_04(L, F) ->
    Res = L:F(
        fun(X) -> X end,
        [3]
    ),
    Res.

-spec test_05(atom(), number()) -> fun().
test_05(F, N) ->
    fun lists:F/N.

-spec test_06(atom(), atom(), number())
        -> fun().
test_06(M, F, N) ->
    fun M:F/N.

-spec test_07(atom(), atom(), number())
        -> fun().
test_07(M, F, N) ->
    Res = fun M:F/N,
    Res.

-spec test_08(atom(), atom(), number())
        -> fun((none()) -> term()).
test_08(M, F, N) ->
    Res = fun M:F/N,
    Res.

-spec test_09(atom(), atom(), number())
        -> fun((none()) -> term()).
test_09(M, F, N) ->
    Res = fun M:F/N,
  case true of
    true when is_function(Res, 2)
        -> Res;
    false ->
        throw(err)
  end.

-spec test_10_pos(atom(), atom()) -> fun().
test_10_pos(M, F) ->
    fun M:F/2.

-spec test_11_neg(atom(), atom()) -> a.
test_11_neg(M, F) ->
    fun M:F/2.

-spec test_12_pos(
    atom(), atom(), atom()) -> fun().
test_12_pos(M, F, A) ->
fun M:F/A.

-spec test_13_neg(
    atom(), atom(), atom()) -> a.
test_13_neg(M, F, A) ->
fun M:F/A.
