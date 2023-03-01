%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(comprehensions).

-compile([export_all, nowarn_export_all]).

-spec gen_atom(term()) -> atom().
gen_atom(A) when is_atom(A) -> A;
gen_atom(_) -> not_atom.

-spec gen_number(term()) -> number().
gen_number(N) when is_number(N) -> N;
gen_number(_) -> 0.

-spec test01
    ([{atom()} | {atom(), number()}]) ->
    [atom()].
test01(L) ->
    [A || {A} <- L].

-spec test02_neg
    ([{atom()} | {atom(), number()}]) ->
    [atom()].
test02_neg(L) ->
    [N || {_, N} <- L].

-spec test03
    ([{atom()} | {atom(), number()}]) ->
    {[atom()], x}.
test03(L) ->
    Atoms = [gen_atom(X) || X <- L],
    X = x,
    {Atoms, X}.

-spec test04_neg(term()) -> [term()].
test04_neg(L) ->
    [X || X <- L].

-spec test05(binary()) -> [number()].
test05(B) ->
    [Y || <<Y>> <= B ].

-spec test06_neg(binary()) -> list(term()).
test06_neg(B) ->
    << Y || <<Y>> <= B >>.

-spec test07(list(binary())) -> binary().
test07(LB) ->
    << Y || Y <- LB >>.

-spec test08_neg(binary()) -> binary().
test08_neg(B) ->
    << Y || Y <- B >>.

-spec test09_neg(list(binary())) -> binary().
test09_neg(LB) ->
    << Y || <<Y>> <= LB >>.

-spec test10(binary()) -> [number()].
test10(LB) ->
    [ Y || <<Y>> <= LB ].

-spec test11_neg(binary()) -> [binary()].
test11_neg(LB) ->
    [ Y || <<Y>> <= LB ].

-spec test12(binary()) -> [binary()].
test12(LB) ->
    << Y || <<Y>> <= LB >>.

-spec test13_neg(binary()) -> binary().
test13_neg(LB) ->
    << Y || <<Y>> <= LB >>.

-spec test14(binary()) -> binary().
test14(LB) ->
    << <<Y>> || <<Y>> <= LB >>.

-spec test15_neg(binary()) -> binary().
test15_neg(LB) ->
    << <<Y>> ||
        <<Y>> <= LB,
        gen_atom(Y) >>.

-spec test16_neg(term()) -> [term()].
test16_neg(L) ->
    Res = [X || X <- L],
    Res.

-spec test17(binary()) -> [number()].
test17(LB) ->
    Res = [ Y || <<Y>> <= LB ],
    Res.

-spec test18_neg(boolean()) -> [boolean()].
test18_neg(L) ->
    Res = [X || X <- L, X],
    Res.

-spec test19([boolean()]) -> [binary()].
test19(L) ->
    Res = [erlang:atom_to_binary(X) ||
            X <- L, X],
    Res.

-spec test20(binary()) -> binary().
test20(LB) ->
    Res = << <<Y>> || <<Y>> <= LB >>,
    Res.

-spec test21_neg(list(binary())) -> binary().
test21_neg(LB) ->
    Res = << Y || <<Y>> <= LB >>,
    Res.

-spec test22_neg(binary()) -> binary().
test22_neg(B) ->
    Res = << Y || Y <- B >>,
    Res.

-spec test23([binary()]) -> binary().
test23(B) ->
    Res = << Y || Y <- B >>,
    Res.

-spec test24([boolean()]) -> binary().
test24(B) ->
    Res = << (erlang:atom_to_binary(Y)) ||
            Y <- B, Y >>,
    Res.

-spec num_atom(number(), atom())
    -> number().
num_atom(_, _) -> 3.

-spec test25
    ([{number()} | [atom()]]) ->
    [number()].
test25(L) ->
    [num_atom(A, B) + 1 ||
        {A} <- L, [B] <- L].

-spec test26(none()) -> list().
test26(X) -> [Y || Y <- X].

-spec test27(none()) -> list().
test27(X) ->
    Res = [Y || Y <- X],
    Res.

-spec test28(none()) -> number().
test28(X) ->
    << Y ||
        <<Y>> <= X >>.

-spec test29(none()) -> number().
test29(X) ->
    Res = << Y ||
        <<Y>> <= X >>,
    Res.

-spec test30(none()) -> binary().
test30(X) ->
    << Y ||
        <<Y>> <= X >>.

-spec test31(none()) -> binary().
test31(X) ->
    Res = << Y ||
        <<Y>> <= X >>,
    Res.

-spec test32([term()]) -> [atom()].
test32(L) ->
    Res = [X || X <- L, is_atom(X)],
    Res.

-spec test33_neg([term()]) -> [binary()].
test33_neg(L) ->
    Res = [X || X <- L, is_atom(X)],
    Res.

-spec convert_data(binary())
        -> [integer()].
convert_data(Bin) ->
    Lines =
        string:split(Bin, "\n", all),
    IntOrErrors = [
        string:to_integer(Line)
        || Line <- Lines
    ],
    [Int ||
        {Int, _Rest} <- IntOrErrors,
        is_integer(Int)
    ].

-spec convert_data2(binary())
        -> [integer()].
convert_data2(Bin) ->
    Lines =
        string:split(Bin, "\n", all),
    IntOrErrors = [
        string:to_integer(Line)
        || Line <- Lines
    ],
    Res = [Int ||
        {Int, _Rest} <- IntOrErrors,
        is_integer(Int)
    ],
    Res.

-spec test34([term()]) -> [true].
test34(L) ->
    [X || X <- L, X].

-spec test35([term()]) -> [true].
test35(L) ->
    Res = [X || X <- L, X],
    Res.

-spec test36_neg([term()]) -> [false].
test36_neg(L) ->
    [X || X <- L, X].

-spec test37_neg([term()]) -> [false].
test37_neg(L) ->
    Res = [X || X <- L, X],
    Res.

-spec test38(binary()) -> [].
test38(B) ->
    [Y || <<Y>> <= B, Y ].

-spec test39_neg(binary()) -> [atom()].
test39_neg(B) ->
    [Y || <<Y>> <= B, is_integer(Y) ].

-record(rec1, {
    b :: undefined | binary(),
    s :: undefined | string()
}).

-record(rec2, {
    b :: undefined | binary(),
    s :: undefined | string()
}).

-spec gen1() -> [binary() | undefined].
gen1() ->
    error(undefined).

-spec gen2() -> [#rec1{} | #rec2{}].
gen2() ->
    error(undefined).

-spec gen3() -> [#rec1{} | #rec2{} | binary()].
gen3() ->
    error(undefined).

-spec test40_neg() ->
    [binary()].
test40_neg() ->
    Res = [X || X <- gen1()],
    Res.

-spec test41() -> [binary()].
test41() ->
    Res = [X || X <- gen1(), is_binary(X)],
    Res.

-spec test42() -> [undefined].
test42() ->
    Res = [
        X || X <- gen1(),
        X =:= undefined
    ],
    Res.

-spec test43() -> [undefined].
test43() ->
    Res = [
        X || X <- gen1(),
        X == undefined
    ],
    Res.

-spec test44() -> [binary()].
test44() ->
    Res = [
        X || X <- gen1(),
        X =/= undefined
    ],
    Res.

-spec test45() -> [binary()].
test45() ->
    Res = [
        X || X <- gen1(),
        X /= undefined
    ],
    Res.

-spec test46() -> [#rec1{}].
test46() ->
    Res = [
        Rec || Rec <- gen2(),
        is_record(Rec, rec1)
    ],
    Res.

-spec test47() -> [#rec1{}].
test47() ->
    Res = [
        Rec || Rec <- gen2(),
        is_record(Rec, rec1, 2)
    ],
    Res.

-spec test48() -> [#rec1{} | #rec2{}].
test48() ->
    Res = [
        Rec || Rec <- gen3(),
        is_record(Rec, rec1) or
            is_record(Rec, rec2)
    ],
    Res.

-spec modules1
    (unicode:chardata())
        -> [module()].
modules1(P) ->
    AllModules = [
        list_to_atom(M)
        || {M, F, _}
            <- code:all_available(),
        is_list(F),
        string:prefix(F, P) =/= nomatch
    ],
    AllModules.

-spec modules2
    (unicode:chardata())
        -> [module()].
modules2(P) ->
    AllModules = [
        list_to_atom(M)
        || {M, F, _}
            <- code:all_available(),
        F =/= preloaded,
        F =/= cover_compiled,
        string:prefix(F, P) =/= nomatch
    ],
    AllModules.

-spec test49([
    fun(() -> atom()) |
    fun((number()) -> atom())
]) -> [atom()].
test49(Fs) ->
    Res = [
        F(0) || F <- Fs,
        is_function(F, 1)
    ],
    Res.

-spec test50([
    fun(() -> atom()) |
    fun((number()) -> atom())
]) -> [atom()].
test50(Fs) ->
    Res = [
        F() || F <- Fs,
        is_function(F, 0)
    ],
    Res.

-spec test51_neg([
    fun(() -> atom()) |
    fun((number()) -> atom())
]) -> [atom()].
test51_neg(Fs) ->
    Res = [
        F() || F <- Fs,
        is_function(F)
    ],
    Res.
