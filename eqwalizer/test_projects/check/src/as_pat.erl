%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(as_pat).

-compile([export_all, nowarn_export_all]).

-record(box_a, {a :: atom()}).
-record(box_b, {b :: binary()}).
-record(box_n, {n :: number()}).

-type abn() :: atom() | binary() | number().

-type box() :: #box_a{}
             | #box_b{}
             | #box_n{}.

-spec box_a(atom()) -> #box_a{}.
box_a(A) -> #box_a{a = A}.

-spec unbox_a(#box_a{}) -> atom().
unbox_a(#box_a{a = A}) -> A.

-spec box_n(number()) -> #box_n{}.
box_n(N) -> #box_n{n = N}.

-spec unbox_n(#box_n{}) -> number().
unbox_n(#box_n{n = N}) -> N.

-spec box_b(binary()) -> #box_b{}.
box_b(B) -> #box_b{b = B}.

-spec unbox_b(#box_b{}) -> binary().
unbox_b({b, B}) -> B.

-spec unbox(box()) -> abn().
unbox(#box_a{a = A}) -> A;
unbox(#box_b{b = B}) -> B;
unbox(#box_n{n = N}) -> N.

-spec unboxL(box()) -> abn().
unboxL(BA = #box_a{}) -> unbox_a(BA);
unboxL(BB = #box_b{}) -> unbox_b(BB);
unboxL(BN = #box_n{}) -> unbox_n(BN).

-spec unboxL_neg(box()) -> abn().
unboxL_neg(BA = #box_a{}) -> unbox_a(BA);
unboxL_neg(BB = #box_b{}) -> unbox_b(BB);
unboxL_neg(BN = #box_n{}) -> unbox_a(BN).

-spec unboxR(box()) -> abn().
unboxR(#box_a{} = BA) -> unbox_a(BA);
unboxR(#box_b{} = BB) -> unbox_b(BB);
unboxR(#box_n{} = BN) -> unbox_n(BN).

-spec unboxR_neg(box()) -> abn().
unboxR_neg(#box_a{} = BA) -> unbox_a(BA);
unboxR_neg(#box_b{} = BB) -> unbox_b(BB);
unboxR_neg(#box_n{} = BN) -> unbox_b(BN).
