%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(guards).

-compile([export_all, nowarn_export_all]).

-spec test01(atom() | number()) -> atom().
test01(X) when is_atom(X) -> X.

-spec test01_if
    (atom() | number()) ->
    atom().
test01_if(X) ->
    if
        is_number(X) -> undef;
        is_atom(X) -> X
    end.

-spec test02
    (atom() | number(), atom() | number())
    -> atom().
test02(X, _) when is_atom(X) -> X;
test02(_, Y) when is_atom(Y) -> Y;
test02(_, _) -> default.

-spec test02_if
    (atom() | number(), atom() | number())
        -> atom().
test02_if(X, Y) ->
    if
        is_atom(X) -> X;
        is_atom(Y) -> Y;
        true -> default
    end.


-spec test03(atom()) -> boolean() | undef.
test03(X) when is_boolean(X) -> X;
test03(_) -> undef.

-spec test04(term()) -> number() | undef.
test04(X) when is_number(X) -> X;
test04(_) -> undef.

-spec test05(term()) ->
    #{number() => number()}.
test05(X) when X =:= #{1 => b} ->
    X.

-type loop() :: loop().
-record(invalid, {field :: loop()}).

-spec test06_neg() -> ok.
test06_neg() when #invalid{} =/= 2 -> ok.

-spec redundant_guard(ok) -> ok.
redundant_guard(X) ->
    (is_atom(X) orelse error(fail)),
    X.
