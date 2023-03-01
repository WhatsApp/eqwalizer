%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(funs2).
-compile([export_all, nowarn_export_all]).

-spec test_map1() -> [number()].
test_map1() ->
    X = lists:map(
        fun(X) -> X + 1 end,
        [1, 2, 3]),
    X.

-spec test_map2() -> [number()].
test_map2() ->
    X = lists:map(
        % works because
        % pattern can't match
        fun({X}) -> X + 1 end,
        [a, b, c]),
    X.

-spec test_foldl1_pos() -> number().
test_foldl1_pos() -> lists:foldl(
    fun(X, Sum) -> X + Sum end,
    0, [1, 2, 3]).

-spec test_foldl2_neg() -> term().
test_foldl2_neg() -> lists:foldl(
    fun(_, Acc) -> [Acc] end,
    [], [1]).

-spec test_foldl3() -> [number()].
test_foldl3() -> lists:foldl(
    fun(X, L) -> [X|L] end,
    [], [1, 2, 3]).

-spec mapdeep({fun((T) -> U)}, [T])
    -> [U].
mapdeep(_, []) -> [];
mapdeep({F}, [H|T]) ->
    [F(H) | lists:map(F, T)].

% Not accepted due to ad-hockery:
% We only special-case lambdas when
% they are direct arguments to a function
-spec test_deep_lambda1() -> number().
test_deep_lambda1() ->
    X = mapdeep(
        {fun(X) -> X + 1 end},
        [1, 2, 3]),
    X.

-spec mapboth(
    fun((T1) -> U1),
    [T1],
    fun((T2) -> U2),
    [T2]
) -> {[U1], [U2]}.
mapboth(F1, T1s, F2, T2s) ->
    {
        lists:map(F1, T1s),
        lists:map(F2, T2s)
    }.

-spec test_mapboth1() ->
    {[number()], [[a]]}.
test_mapboth1() ->
    mapboth(
        fun(X) -> X + 1 end, [1],
        fun(X) -> X ++ [a] end, [[a]]
    ).

-spec trans(
    fun((T) -> U),
    fun((U) -> V),
    T
) -> V.
trans(F1, F2, X) -> F2(F1(X)).

-spec test_trans1_wip() -> term().
test_trans1_wip() -> trans(
    fun(X) -> X end,
    fun(X) -> X end,
    1
).

-spec generic(
    fun((T) -> T),
    fun((T) -> T),
    T
) -> T.
generic({}, {}, {}) -> nok.

-spec test_err_msg() -> nok.
test_err_msg() ->
    generic(
        fun(X) -> X end,
        fun erlang:atom_to_list/1,
        fun erlang:list_to_atom/1
    ),
    nok.

-spec apply0(fun(() -> T)) -> T.
apply0(F) -> F().

-spec test_apply_nullary_lambda() -> a.
test_apply_nullary_lambda() ->
    F = fun() -> a end,
    Res = apply0(F),
    Res.

-spec test_apply_nullary_lambda2() -> a.
test_apply_nullary_lambda2() ->
    Res = apply0(fun() -> a end),
    Res.

-spec test_mapfoldl() ->
    {[{number()}], number()}.
test_mapfoldl() ->
    Res = lists:mapfoldl(
        fun (N, Sum) ->
            {{N}, Sum + N}
        end,
        0,
        [1, 2, 3]),
    % {[{1},{2},{3}],6}
    Res.

-spec test_mapfoldr() ->
    {[{number()}], number()}.
test_mapfoldr() ->
    Res = lists:mapfoldr(
        fun (N, Sum) ->
            {{N}, N + Sum}
        end,
        0,
        [1, 2, 3]),
    % {[{1},{2},{3}],6}
    Res.

-spec repeated_vars() -> number().
repeated_vars() ->
    (fun(X, X) -> X end)(2, 2).

-spec repeated_vars_neg(atom()) -> number().
repeated_vars_neg(A) ->
    (
        fun
            (X, X) -> X + A
        end
    )(2, 2).

-spec repeated_vars2() -> number().
repeated_vars2() ->
    (
        fun
            (X, X) -> X
        end
    )(2, 2),
    3.
