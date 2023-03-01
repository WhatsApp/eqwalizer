%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(dynamic_calls).

-compile([export_all, nowarn_export_all]).

-record(rec, {
    method :: fun((atom()) -> pid())
}).

-spec test_01_pos(fun((a) -> b), a) -> b.
test_01_pos(F, X) -> F(X).

-spec test_02_neg(
    fun((a) -> b),
    a
) -> pid().
test_02_neg(F, X) ->
    F(X).

-spec test_03_pos(
    #rec{},
    atom()
) -> pid().
test_03_pos(
    #rec{method = Method},
    Atom
) ->
    Method(Atom).

-spec test_04_neg(
    #rec{}
) -> number().
test_04_neg(Rec) ->
    (Rec#rec.method)(atom).

-spec test_05_pos(#rec{}) -> pid().
test_05_pos(Rec) ->
    Meth = Rec#rec.method,
    Res = Meth(atom),
    Res.

-spec test_06_neg(fun((a) -> b)) -> term().
test_06_neg(F) -> F(1, 2).

-spec test_07_pos() -> ok.
test_07_pos() ->
    F = fun ret_ok/0,
    F().

-spec test_07_neg() -> pid().
test_07_neg() ->
    F = fun ret_ok/0,
    F().

-spec test_08_neg() -> term().
test_08_neg() ->
    F = fun ret_ok/0,
    F(1).

-spec test_09_neg() -> term().
test_09_neg() ->
    F = fun unspecced/0,
    F().

-spec test_10_neg() -> ok.
test_10_neg() ->
    (fun ret_ok/0)(1, 2).

-spec test_11_neg(
    fun((a1 | a2) -> r1 | r2)
    | fun((a2 | a3) -> r2 | r3)
) -> ok.
test_11_neg(FUnion) ->
    (FUnion)(false).

% 'e' tests exercise elaboration instead of checking
-spec test_11e_neg(
    fun((a1 | a2) -> r1 | r2)
    | fun((a2 | a3) -> r2 | r3)
) -> ok.
test_11e_neg(FUnion) ->
    Res = (FUnion)(false),
    Res.

-spec test_12_pos(
    fun((a1 | a2) -> r1 | r2)
    | fun((a2 | a3) -> r2 | r3)
) -> r1.
test_12_pos(FUnion) ->
    (FUnion)(a2).

-spec test_12e_pos(
    fun((a1 | a2) -> r1 | r2)
    | fun((a2 | a3) -> r2 | r3)
) -> r1.
test_12e_pos(FUnion) ->
    Res = (FUnion)(a2),
    Res.

-spec test_13_neg(
    fun((a) -> b) | z
) -> b.
test_13_neg(F) ->
    (F)(a).

-spec test_13e_neg(
    fun((a) -> b) | z
) -> b.
test_13e_neg(F) ->
    Res = (F)(a),
    Res.

-spec test_14_pos(
    fun((a1) -> r1 | r2) |
    (fun((a1 | a2) -> r1 | r2) |
      fun((a1 | a3) -> r2))
    ) -> r1 | r2.
test_14_pos(FUnion) ->
    FUnion(a1).

-spec test_14e_pos(
    fun((a1) -> r1 | r2) |
    (fun((a1 | a2) -> r1 | r2) |
      fun((a1 | a3) -> r2))
    ) -> r1 | r2.
test_14e_pos(FUnion) ->
    Res = FUnion(a1),
    Res.

-spec test_15_neg(
    fun((a) -> b) | fun(() -> b)
) -> b.
test_15_neg(F) ->
    (F)(a).

-spec test_15e_neg(
    fun((a) -> b) | fun(() -> b)
) -> b.
test_15e_neg(F) ->
    Res = (F)(a),
    Res.

-spec ret_ok() -> ok.
ret_ok() -> ok.

unspecced() -> ok.
