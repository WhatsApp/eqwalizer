%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(approx).

-export([
    use_unspecced0/0,
    use_unspecced1/1,
    report_later_error/0,
    test_trans_invalid1_neg/1,
    test_trans_invalid2_neg/1,
    test_trans_invalid3_neg/1, test_opaque_as_fun_neg/1
]).

unspecced0() ->
    ok.

unspecced1(foo) ->
    bar.

-spec use_unspecced0() ->
    {none(), foo}.
use_unspecced0() ->
    X = unspecced0(),
    {X, foo}.

-spec use_unspecced1(any()) -> [atom()].
use_unspecced1(Arg) ->
    unspecced1(Arg).

-spec report_later_error() -> ok.
report_later_error() ->
    X = unspecced1(bar),
    {X, X}.

-type trans_invalid() ::
generics_with_unions:type_var_from_nowhere().

-spec test_trans_invalid1_neg(
    trans_invalid()
) -> nok.
test_trans_invalid1_neg(X) ->
  X().

-spec test_trans_invalid2_neg(
    trans_invalid()
) -> nok.
test_trans_invalid2_neg(#{k := Z}) ->
  Z.

-spec test_trans_invalid3_neg(
    trans_invalid()
) -> nok.
test_trans_invalid3_neg([Z]) ->
  Z.

-spec test_opaque_as_fun_neg(
    misc:o() | fun((any()) -> none())) ->
    pid().
test_opaque_as_fun_neg(X) -> X(ok).

-spec union_list_1(
    string(), eqwalizer:dynamic()
) -> anything.
union_list_1(S, Dyn) ->
  [_H, Dyn2] = [S, Dyn],
  Dyn2.

-spec dyn_union_1(
    none(), eqwalizer:dynamic()
) -> ok.
dyn_union_1(N, D) ->
  Res = case D of
          1 -> N;
          2 -> D
        end,
  eqwalizer:reveal_type(Res),
  Res.

-spec dyn_union_2(
    none(), eqwalizer:dynamic()
) -> ok.
dyn_union_2(N, D) ->
  Res = case D of
          2 -> D;
          1 -> N
        end,
  eqwalizer:reveal_type(Res),
  Res.
