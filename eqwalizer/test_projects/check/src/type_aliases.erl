%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(type_aliases).
-compile([export_all, nowarn_export_all]).

-type w_unbound_var() :: {_A, _B}.
-type trans_unbound_var()
    :: w_unbound_var().

-spec uses_ty_w_unbound_var(_A, _B) ->
    w_unbound_var().
uses_ty_w_unbound_var(A, B) -> {A, B}.

-spec uses_trans_unbound_var(_A, _B) ->
    trans_unbound_var().
uses_trans_unbound_var(A, B) -> {A, B}.

-type w_unbound_var2()
    :: _A.

-spec uses_ty_w_unbound_var2() ->
    w_unbound_var2().
uses_ty_w_unbound_var2() -> 1.

-type repeated(T, T) :: T.

-spec test_repeated1_neg(a, b) ->
    repeated(a, b).
test_repeated1_neg(b, a) -> a.

-spec test_repeated2_neg(a, b) ->
    repeated(a, b).
test_repeated2_neg(a, b) -> b.

%% see T102518744
-spec test_use_bad_type
(undefined | w_unbound_var()) ->
    w_unbound_var() | undefined.
test_use_bad_type(X) ->
    X.

-spec ttt(T, T) -> T.
ttt(X, X) -> X.

-spec test_use_bad_type_gen(
    w_unbound_var(),
    undefined | w_unbound_var()) ->
        term().
test_use_bad_type_gen(X, Union) ->
    ttt(X, Union).

-type tup_w_unbound(T)
:: {T, _A}.

-spec unpack_tup_w_unbound(
    tup_w_unbound(T)
) -> T.
unpack_tup_w_unbound({X, _}) ->
    X.

-spec test_use_unpack_tup_w_unbound(
    tup_w_unbound(a)) -> a.
test_use_unpack_tup_w_unbound(X) ->
    unpack_tup_w_unbound(X).

-spec test_use_tup_w_unbound_cov(
    tup_w_unbound(a)) ->
    tup_w_unbound(a | b) .
test_use_tup_w_unbound_cov(X) ->
    X.

-spec test_use_tup_w_unbound_contra_neg(
    tup_w_unbound(a | b)) ->
    tup_w_unbound(a) .
test_use_tup_w_unbound_contra_neg(X) ->
    X.

-spec test_use_tup_w_unbound_contra_neg2(
    tup_w_unbound(a | b)) ->
    tup_w_unbound(a) | w_unbound_var().
test_use_tup_w_unbound_contra_neg2(X) ->
    X.

-type loop() :: loop().

-record(rec1, {l :: loop() | undefined}).
-record(rec2, {l :: undefined | loop()}).

-spec same(A, A) -> A.
same(X, X) -> X.

-spec id(A) -> A.
id(X) -> X.

-spec convert(any(), any()) -> none().
convert(#rec1{l = L1}, #rec2{l = L2}) ->
    same(id(L1), L2),
    L2.

-type bad_inside() ::
    opaque:opair(a, x:y()).

-spec use_bad_inside_neg(
   bad_inside()
) -> bad_inside().
use_bad_inside_neg(X) -> X.