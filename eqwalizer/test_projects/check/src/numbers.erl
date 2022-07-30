%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(numbers).
-compile([export_all, nowarn_export_all]).

-spec test_1() -> integer().
test_1() ->
  2.

-spec test_2_wip_neg() -> integer().
test_2_wip_neg() ->
  2.0.

-spec test_3_wip_neg() -> integer().
test_3_wip_neg() ->
  X = 2.0,
  X.

-spec test_4() -> number().
test_4() ->
  X = 2,
  X.

-spec test_5(term()) -> number().
test_5(X) when is_number(X) ->
  X.

-spec test_6(term()) -> integer().
test_6(X) when is_integer(X) ->
  X.

-spec test_7_wip_neg(term()) -> integer().
test_7_wip_neg(X) when is_float(X) ->
  X.

-spec test_8(term(), float()) ->
  fun().
test_8(X, Float)
  when is_function(X, Float) ->
  X.

-spec test_9(
    module(),
    atom(),
    integer()
) -> mfa().
test_9(M, F, A) ->
  {M, F, A}.

-spec test_10_wip_neg(
    module(),
    atom(),
    number()
) -> mfa().
test_10_wip_neg(M, F, A) ->
  {M, F, A}.

-spec test_11_wip_neg(term())
      -> integer().
test_11_wip_neg(X=2.0) ->
  X.

-spec test_12_wip_neg(term(), number()) ->
  integer().
test_12_wip_neg(X, X)  ->
  X.

-spec test_13(term()) -> integer().
test_13(X=2) ->
  X.

-spec test_14_wip_neg() -> iolist().
test_14_wip_neg() ->
  [2.0].

-spec test_15() -> iolist().
test_15() ->
  [2].

-spec id(T) -> T.
id(X) ->
  X.

-spec test_16_wip_neg() -> integer().
test_16_wip_neg() ->
  id(2.0).

-spec test_17() -> integer().
test_17() ->
  id(2).

-spec test_18() -> integer().
test_18() ->
  2 + 2.

-spec test_19_wip_neg() -> integer().
test_19_wip_neg() ->
  2 + 2.0.

-spec test_20_wip_neg() -> integer().
test_20_wip_neg() ->
  2.0 + 2.

-spec test_21_wip_neg() -> integer().
test_21_wip_neg() ->
  2 / 2.

-spec test_22() -> integer().
test_22() ->
  2 + 2.

-spec test_23_wip_neg() -> integer().
test_23_wip_neg() ->
  +2.0.

-spec test_24_wip_neg(a) -> number().
test_24_wip_neg(A) ->
  A / 2.

-spec test_25_wip_neg(a) -> number().
test_25_wip_neg(A) ->
  2 / A.

-spec test_26_wip_neg(a) -> number().
test_26_wip_neg(A) ->
  2 * A.

-spec test_27_wip_neg(a) -> number().
test_27_wip_neg(A) ->
  A * 2.

-spec test_27b_wip_neg(a) -> number().
test_27b_wip_neg(A) ->
  X = A * 2,
  X.

-spec test_28_wip_neg(a) -> number().
test_28_wip_neg(A) ->
  2 div A.

-spec test_28b_wip_neg(a) -> number().
test_28b_wip_neg(A) ->
  X = (2 div A),
  X.

-spec test_29_wip_neg(a) -> number().
test_29_wip_neg(A) ->
  A div 2.

-spec test_29b_wip_neg(a) -> number().
test_29b_wip_neg(A) ->
  X = A div 2,
  X.

-spec test_30_wip_neg() -> a.
test_30_wip_neg() ->
  1 div 2.

-spec test_31() -> integer().
test_31() ->
  X = 1 div 2,
  X.

-spec test_32() -> integer().
test_32() ->
  X = bnot 1,
  X.

-spec test_float_lit() -> float().
test_float_lit() ->
  1.0.

-spec test_bnot_1() -> integer().
test_bnot_1() ->
  bnot 0.

-spec test_bnot_2_neg() -> pid().
test_bnot_2_neg() ->
  bnot 0.

-spec test_33(pos_integer())
      -> non_neg_integer().
test_33(X) -> X.

-spec test_33b(pos_integer())
      -> non_neg_integer().
test_33b(X) ->
  Res = X,
  Res.

-spec test_34(pos_integer())
      -> integer().
test_34(X) -> X.

-spec test_34b(pos_integer())
      -> integer().
test_34b(X) ->
  Res = X,
  Res.

-spec test_35(pos_integer())
      -> number().
test_35(X) -> X.

-spec test_35b(pos_integer())
      -> number().
test_35b(X) ->
  Res = X,
  Res.

-spec test_36(non_neg_integer())
      -> integer().
test_36(X) -> X.

-spec test_36b(non_neg_integer())
      -> integer().
test_36b(X) ->
  Res = X,
  Res.

-spec test_37(non_neg_integer())
      -> number().
test_37(X) -> X.

-spec test_38(neg_integer())
      -> integer().
test_38(X) -> X.

-spec test_38b(neg_integer())
      -> integer().
test_38b(X) ->
  Res = X,
  Res.

-spec test_39(neg_integer())
      -> number().
test_39(X) -> X.

-spec test_39b(neg_integer())
      -> number().
test_39b(X) ->
  Res = X,
  Res.

-spec test_40_wip_neg(non_neg_integer())
      -> pos_integer().
test_40_wip_neg(X) -> X.

-spec test_40_negb(non_neg_integer())
      -> pos_integer().
test_40_negb(X) ->
  Res = X,
  Res.

-spec test_41_wip_neg(integer())
      -> pos_integer().
test_41_wip_neg(X) -> X.

-spec test_41_negb(integer())
      -> pos_integer().
test_41_negb(X) ->
  Res = X,
  Res.

-spec test_42_wip_neg(number())
      -> pos_integer().
test_42_wip_neg(X) -> X.

-spec test_42_negb(number())
      -> pos_integer().
test_42_negb(X) ->
  Res = X,
  Res.

-spec test_43_wip_neg(integer())
      -> non_neg_integer().
test_43_wip_neg(X) -> X.

-spec test_43_negb(integer())
      -> non_neg_integer().
test_43_negb(X) ->
  Res = X,
  Res.

-spec test_44_wip_neg(number())
      -> non_neg_integer().
test_44_wip_neg(X) -> X.

-spec test_44_negb(number())
      -> non_neg_integer().
test_44_negb(X) ->
  Res = X,
  Res.

-spec test_45_wip_neg(integer())
      -> neg_integer().
test_45_wip_neg(X) -> X.

-spec test_45_negb(integer())
      -> neg_integer().
test_45_negb(X) ->
  Res = X,
  Res.

-spec test_46_wip_neg(number())
      -> neg_integer().
test_46_wip_neg(X) ->
  X.

-spec test_46_negb(number())
      -> neg_integer().
test_46_negb(X) ->
  Res = X,
  Res.

-spec test_47() -> non_neg_integer().
test_47() ->
  0.


-spec test_47b() -> non_neg_integer().
test_47b() ->
  Res = 0,
  Res.

-spec test_48() -> pos_integer().
test_48() ->
  +1.

-spec test_48b() -> pos_integer().
test_48b() ->
  Res = +1,
  Res.

-spec test_49() -> neg_integer().
test_49() ->
  -1.

-spec test_49b() -> neg_integer().
test_49b() ->
  Res = -1,
  Res.

-spec test_50_neg(term())
      -> number().
test_50_neg(X) ->
  (+X).

-spec test_50b_neg(term()) -> nok.
test_50b_neg(X) ->
  _ = (+X),
  nok.

-spec test_51_neg(term()) -> nok.
test_51_neg(X) ->
  _ = (-X),
  nok.

-spec test_51b_neg(term()) -> nok.
test_51b_neg(X) ->
  _ = (-X),
  nok.

-spec test_52_wip_neg(pos_integer()) ->
  pos_integer().
test_52_wip_neg(X) ->
  (-X).

-spec test_53(pos_integer()) ->
  neg_integer().
test_53(X) ->
  (-X).

-spec test_54(neg_integer()) ->
  pos_integer().
test_54(X) ->
  (-X).

-spec test_56_wip_neg(integer()) ->
  neg_integer().
test_56_wip_neg(X) ->
  (-X).

-spec test_57_wip_neg(non_neg_integer())
      -> neg_integer().
test_57_wip_neg(X) ->
  (-X).

-spec test_58(pos_integer()) ->
  neg_integer().
test_58(X) ->
  bnot X.

-spec test_58b(pos_integer()) ->
  neg_integer().
test_58b(X) ->
  Res = bnot X,
  Res.

-spec test_59(neg_integer()) ->
  integer().
test_59(X) ->
  bnot X.

-spec test_59b(neg_integer()) ->
  integer().
test_59b(X) ->
  Res = bnot X,
  Res.

-spec test_59_wip_neg(neg_integer()) ->
  pos_integer().
test_59_wip_neg(X) ->
  bnot X.

-spec test_59b_wip_neg(neg_integer()) ->
  pos_integer().
test_59b_wip_neg(X) ->
  Res = bnot X,
  Res.

-spec test_60_neg(term()) ->
  integer().
test_60_neg(X) ->
  bnot X.

-spec test_60b_neg(term()) ->
  nok.
test_60b_neg(X) ->
  _ = bnot X,
  nok.

-spec test_61() -> +1..2.
test_61() -> 1.

-spec test_62() -> -1..+2.
test_62() -> -1.

-spec test_63() -> +1..+2.
test_63() -> 1.

-spec test_64() -> -2..-1.
test_64() -> -1.

-spec test_65(neg_integer(),
    neg_integer()) -> neg_integer().
test_65(N1, N2) -> N1 + N2.

-spec test_66(neg_integer(),
    pos_integer()) -> neg_integer().
test_66(N1, N2) -> N1 - N2.

-spec test_66_wip_neg(pos_integer(),
    neg_integer()) -> pos_integer().
test_66_wip_neg(N1, N2) -> N1 + N2.

-spec test_67_wip_neg(pos_integer(),
    neg_integer()) -> neg_integer().
test_67_wip_neg(N1, N2) -> N1 + N2.

-spec test_68(pos_integer(),
    neg_integer()) -> neg_integer().
test_68(N1, N2) -> N1 * N2.

-spec test_69(neg_integer(),
    neg_integer()) -> pos_integer().
test_69(N1, N2) -> N1 * N2.

-spec test_70(integer(),
    neg_integer()) -> integer().
test_70(N1, N2) -> N1 * N2.

-spec test_71(integer(),
    neg_integer()) -> integer().
test_71(N1, N2) -> N1 div N2.

-spec test_72(
    non_neg_integer() | pos_integer(),
    non_neg_integer() | pos_integer())
      -> non_neg_integer().
test_72(N1, N2) -> N1 div N2.

-spec test_78_neg(
    a | number(),
    number()
) -> number().
test_78_neg(U, N) ->
  U * N.

-spec test_79_wip_neg(
    float() | integer(),
    integer()
) -> number().
test_79_wip_neg(U, N) ->
  U div N.

-spec test_80(
    pos_integer() | neg_integer(),
    neg_integer()
) -> integer().
test_80(U, N) ->
  U div N.

-spec test_81(
    pos_integer(),
    pos_integer() | non_neg_integer()
) -> non_neg_integer().
test_81(U, N) ->
  U div N div (N + U).

-spec test_82(
    pos_integer(),
    number() | non_neg_integer()
) -> number().
test_82(U, N) ->
  U + N / (N + U).

-spec test_83_wip_neg(
    non_neg_integer() | pos_integer(),
    pos_integer() | integer()
) -> non_neg_integer().
test_83_wip_neg(U1, U2) ->
  U1 + U2 / (U2 + U1).

-spec test_84(
    non_neg_integer() | pos_integer(),
    non_neg_integer() | pos_integer()
) -> non_neg_integer().
test_84(U1, U2) ->
  U1 div U2 / (U2 bsl U1).

-spec test_85(
    none()
) -> integer().
test_85(X) ->
  X div X.

-spec test_86(
    none()
) -> number().
test_86(X) ->
  X * X.

-spec test_87_wip_neg(
    a | b,
    c | d
) -> non_neg_integer().
test_87_wip_neg(U1, U2) ->
  U1 div U2 / (U2 bsl U1).

-spec test_88_wip_neg(
    a | b,
    c | d
) -> non_neg_integer().
test_88_wip_neg(U1, U2) ->
  U1 * U2 / (U2 + U1).

-spec test_89(
    neg_integer() | pos_integer()
) -> integer().
test_89(X) ->
  X * X.

-spec test_90(
    integer() | float()
) -> integer().
test_90(X) ->
  X * X.

-spec test_91_neg(
    none()
) -> none().
test_91_neg(X) ->
  -X.

-spec test_92_neg(
    none()
) -> none().
test_92_neg(X) ->
  bnot X.

-spec test_93_neg(
    none()
) -> none().
test_93_neg(X) ->
  5 * X.

-spec test_94(
    non_neg_integer(),
    non_neg_integer()
) -> non_neg_integer().
test_94(X, Y) ->
  X * Y.

-spec test_95(
    neg_integer(),
    non_neg_integer()
) -> integer().
test_95(X, Y) ->
  X * Y.

-spec test_96(
    integer(),
    integer()
) -> integer().
test_96(X, Y) ->
  X - Y.

-spec test_97_wip_neg(
    integer(),
    number()
) -> integer().
test_97_wip_neg(X, Y) ->
  X - Y.

-spec test_98(term()) -> integer().
test_98(X) when bnot X < self() ->
  X.

-spec test_99(term()) -> integer().
test_99(X) when +X < self() ->
  X.

-spec test_100(term()) -> integer().
test_100(X) when -X < self() ->
  X.

-spec test_101(term()) -> number().
test_101(X) when X / 2 < self() ->
  X.

-spec test_102_neg(integer()) ->
  {
    number(),
    integer(),
    integer(),
    number(),
    ok
  }.
test_102_neg(Int) ->
  {
    1,
    0,
    -1,
    Int,
    error
  }.

-spec test_103_wip_neg(-1..20) ->
  neg_integer().
test_103_wip_neg(X) -> X.

-spec test_104_neg() -> pid().
test_104_neg() ->
  1.0.
