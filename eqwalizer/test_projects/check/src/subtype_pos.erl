%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(subtype_pos).

-compile([export_all, nowarn_export_all]).

-type ab() :: a | b.
-type pair_diff_elems() ::
    {a, b} | {b, a}.
-type pair_ab() ::
    {ab(), ab()}.

-spec f01({A, A}) -> term().
f01(X) -> X.

-spec f02(atom()) -> term().
f02(X) -> X.

-spec f03 (fun((term()) -> atom())) ->
    fun((atom()) -> term()).
f03(F) -> F.

-spec f04(a | b) -> (a | b | c).
f04(X) -> X.

-spec f05(none() | none()) -> a | b.
f05(X) -> X.

-spec f06({a, b} | {b, a}) ->
           {a | b, a | b}.
f06(X) -> X.

-spec f07(pair_diff_elems()) ->
    pair_ab().
f07(X) -> X.

-spec f08(none()) -> {none(), none()}.
f08(X) -> X.

-spec map01(#{}) -> map().
map01(M) -> M.

-spec map02(#{a := atom()})
    -> #{a => atom()}.
map02(M) -> M.

-spec map03(#{a := atom(), b => atom()})
        -> #{atom() => atom()}.
map03(M) -> M.

-spec map04(#{
    a := a | b,
    b => a | b
}) -> #{a | b => a | b}.
map04(M) -> M.

-spec map05(#{
    a := a | b,
    b => a | b
}) -> #{a | b => a | b | c}.
map05(M) -> M.

-spec map06(#{atom() => integer()})
    -> #{term() => integer()}.
map06(M) -> M.

-spec map07(#{atom() => integer()})
        -> #{atom() => term()}.
map07(M) -> M.

-spec map08(#{})
        -> #{atom() => term()}.
map08(M) -> M.

-spec ty_var_sub_any_1(T, T) -> term().
ty_var_sub_any_1(T, T) -> T.

-spec ty_var_sub_any_2(T, T) -> term().
ty_var_sub_any_2(T, T) ->
    X = T,
    X.

-spec id(T) -> T.
id(X) -> X.

-spec foralls_matter() -> unreachable.
foralls_matter() ->
    X = fun erlang:is_number/1,
    case (fun id/1) of
        X -> X
        end.

-spec iolist1([]) -> iolist().
iolist1(L) -> L.

-spec iolist2([binary()]) -> iolist().
iolist2(L) -> L.

-spec iolist3([non_neg_integer()]) -> iolist().
iolist3(L) -> L.

-spec iolist4([iolist()]) -> iolist().
iolist4(L) -> L.

-spec iolist5(
    [binary() | non_neg_integer() | iolist()]
) -> iolist().
iolist5(L) -> L.

% When EQWALIZER_STRICT_INTEGERS is false,
% all integer types are Subtype.eqv
-spec num_1(pos_integer()) ->
    neg_integer().
num_1(X) -> X.

-spec num_2(neg_integer()) ->
    pos_integer().
num_2(X) -> X.

-spec num_3(integer()) ->
    non_neg_integer().
num_3(X) -> X.

-spec nil_1([none()]) -> [].
nil_1(L) -> L.

-spec nil_2([none()]) -> [[]].
nil_2(L) -> L.

-spec nil_3([none() | none()]) -> [].
nil_3(L) -> L.
