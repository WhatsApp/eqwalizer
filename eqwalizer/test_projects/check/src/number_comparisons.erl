%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(number_comparisons).
-compile([export_all, nowarn_export_all]).

-spec test_1_neg(term()) ->
  pos_integer().
test_1_neg(X) when X >= 1 -> X.

-spec test_2(term()) ->
  pos_integer().
test_2(X)
  when is_integer(X), X >= 1 ->
  X.

-spec test_2_rev(term()) ->
  pos_integer().
test_2_rev(X)
  when is_integer(X), 1 =< X ->
  X.

-spec test_3(term()) ->
  pos_integer().
test_3(X)
  when is_integer(X) and (X >= 1) ->
  X.

-spec test_4(term()) ->
  pos_integer().
test_4(X)
  when is_integer(X) andalso X >= 1 ->
  X.

-spec test_4_rev(term()) ->
  pos_integer().
test_4_rev(X)
  when is_integer(X) andalso 1 =< X ->
  X.

-spec test_5(term()) ->
  pos_integer().
test_5(X)
  when is_integer(X), X >= 0 ->
  X.

-spec test_6(term()) ->
  non_neg_integer().
test_6(X)
  when is_integer(X), X >= 0 ->
  X.

-spec test_6_rev(term()) ->
  non_neg_integer().
test_6_rev(X)
  when is_integer(X), 0 =< X ->
  X.

-spec test_7(term()) ->
  pos_integer().
test_7(X)
  when is_integer(X), X > 0 ->
  X.

-spec test_7_rev(term()) ->
  pos_integer().
test_7_rev(X)
  when is_integer(X), 0 < X ->
  X.

-spec test_8(term()) ->
  pos_integer().
test_8(X)
  when is_integer(X), X > 0 ->
  X.

-spec test_8_rev(term()) ->
  pos_integer().
test_8_rev(X)
  when is_integer(X), 0 < X ->
  X.

-spec test_9(term()) ->
  non_neg_integer().
test_9(X)
  when is_integer(X), X > -1 ->
  X.

-spec test_9_rev(term()) ->
  non_neg_integer().
test_9_rev(X)
  when is_integer(X), -1 < X ->
  X.

-spec test_10(term()) ->
  non_neg_integer().
test_10(X)
  when is_integer(X), X =< -1 ->
  X.

-spec test_10_rev(term()) ->
  non_neg_integer().
test_10_rev(X)
  when is_integer(X), -1 >= X ->
  X.

-spec test_11(term()) ->
  neg_integer().
test_11(X)
  when is_integer(X), X < 0 ->
  X.

-spec test_11_rev(term()) ->
  neg_integer().
test_11_rev(X)
  when is_integer(X), 0 > X ->
  X.

-spec test_12(non_neg_integer()) ->
  pos_integer().
test_12(X)
  when X /= 0 ->
  X.

-spec test_12_rev(non_neg_integer()) ->
  pos_integer().
test_12_rev(X)
  when 0 /= X ->
  X.

-spec test_13(non_neg_integer()) ->
  pos_integer().
test_13(X)
  when X =/= 0 ->
  X.

-spec test_13_rev(non_neg_integer()) ->
  pos_integer().
test_13_rev(X)
  when 0 =/= X ->
  X.

-spec test_14(integer()) ->
  pos_integer().
test_14(X)
  when X == 1 ->
  X.

-spec test_15(integer()) ->
  pos_integer().
test_15(X)
  when X == 999999 ->
  X.

-spec test_16(integer()) ->
  non_neg_integer().
test_16(X)
  when X == 0 ->
  X.

-spec test_17(term()) ->
  pos_integer().
test_17(X)
  when X =:= 1 ->
  X.

-spec test_18(term()) ->
  non_neg_integer().
test_18(X)
  when X =:= 0 ->
  X.

-spec test_19(integer()) ->
  neg_integer().
test_19(X)
  when -1 >= X ->
  X.

-spec test_20(integer()) ->
  neg_integer().
test_20(X)
  when X =< -1 ->
  X.

-spec test_21(
    pos_integer(),
    pos_integer()
) -> pos_integer().
test_21(X, Y) ->
  X div Y.

-spec test_22(
    pos_integer(),
    pos_integer()
) -> pos_integer().
test_22(X, Y) ->
  X bsl Y.

-spec test_23() -> pos_integer().
test_23() ->
  2 rem 2.

-spec test_24(
    pos_integer(),
    pos_integer()
) -> non_neg_integer().
test_24(X, Y) ->
  X rem Y.

-spec test_25(
    pos_integer(),
    pos_integer()
) -> non_neg_integer().
test_25(X, Y) ->
  X band Y.

-spec test_26(
    pos_integer(),
    integer()
) -> non_neg_integer().
test_26(X, Y) ->
  X band Y.

-spec test_28(
    pos_integer(),
    integer()
) -> integer().
test_28(X, Y) ->
  X band Y.

-spec test_29(
    pos_integer(),
    integer()
) -> integer().
test_29(X, Y) ->
  X div Y.

-spec test_30(a | pos_integer())
      -> pos_integer().
test_30(X) when is_integer(X) ->
  X.

-spec test_31()
      -> pos_integer().
test_31() ->
  1 bsl 20.

-spec test_32_wip_neg(neg_integer()) ->
  none().
test_32_wip_neg(X) when X > 0 ->
  X.

-spec test_34(term()) -> pos_integer().
test_34(X)
  when X > 0 andalso is_integer(X) ->
  X.

-spec test_35(term()) ->
  pos_integer().
test_35(X)
  when X > 0 andalso is_number(X) ->
  X.
