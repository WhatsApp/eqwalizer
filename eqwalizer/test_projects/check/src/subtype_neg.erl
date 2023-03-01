%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(subtype_neg).

-compile([export_all, nowarn_export_all]).

-type ab() :: a | b.
-type pair_diff_elems() ::
    {a, b} | {b, a}.
-type pair_ab() ::
    {ab(), ab()}.

-spec f01(term()) -> {A, A}.
f01(X) -> X.

-spec f02(term()) -> atom().
f02(X) -> X.

-spec f03 (fun((atom()) -> term())) ->
           fun((term()) -> atom()).
f03(F) -> F.

-spec f04(a | b | c) -> (a | b).
f04(X) -> X.

-spec f05(a | b) -> none() | none().
f05(X) -> X.

-spec f06({a | b, a | b}) ->
    {a, b} | {b, a}.
f06(X) -> X.

-spec f07(pair_ab()) ->
    pair_diff_elems().
f07(X) -> X.

-spec f08({none(), none()}) -> none().
f08(X) -> X.

-spec map01(map()) -> #{}.
map01(M) -> M.

-spec map02(#{a => atom()})
        -> #{a := atom()}.
map02(M) -> M.

-spec map03(#{a := atom(), b := atom()})
        -> #{a => atom()}.
map03(M) -> M.

-spec map05(#{
    a | b => a | b | c
}) -> #{
    a := a | b,
    b => a | b
}.
map05(M) -> M.

-spec map06(#{term() => integer()})
        -> #{atom() => integer()}.
map06(M) -> M.

-spec map07(#{atom() => term()})
        -> #{atom() => integer()}.
map07(M) -> M.

-spec map08(#{atom() => term()})
        -> #{}.
map08(M) -> M.

-spec tuple1
    (tuple()) -> {term()}.
tuple1(X) -> X.

-spec any_fun(term()) -> fun().
any_fun(F) -> F.

-spec f10() -> {tuple(), ok}.
f10() ->
    {{}, error}.

-spec f11() -> {[pid()], ok}.
f11() ->
    {[], error}.

-spec f12() -> {iolist(), ok}.
f12() ->
    {[], error}.

-spec nil_1([a]) -> [].
nil_1(L) -> L.

-spec nil_2([a] | [none()]) -> [].
nil_2(L) -> L.
