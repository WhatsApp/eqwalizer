%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(any_fun_type).

-compile([export_all, nowarn_export_all]).

% fun() is like f0() | f1() | f3() ...
-type f0() :: fun(() -> any()).
-type f1() :: fun((any()) -> any()).
-type f2() ::
    fun((any(), any()) -> any()).
-type f3() ::
    fun((any(), any(), any()) -> any()).

-spec f0_to_f_any_pos(f0()) -> fun().
f0_to_f_any_pos(F) -> F.

-spec f1_to_f_any_pos(f1()) -> fun().
f1_to_f_any_pos(F) -> F.

-spec fs_to_f_any_pos(
    f0() | f1() | f2() | f3()
) -> fun().
fs_to_f_any_pos(F) -> F.

-spec to_f_any_pos(
    fun((any(), any()) -> pid())
) -> fun().
to_f_any_pos(F) -> F.

-spec to_f_any_neg1(
    fun((atom()) -> pid())
) -> fun().
to_f_any_neg1(F) -> F.

-spec to_f_any_neg2(
    f0 | fun((atom()) -> pid()) | f1
) -> fun().
to_f_any_neg2(F) -> F.

-spec f_any_to_f0_neg(fun()) -> f0.
f_any_to_f0_neg(F) -> F.

-spec guard01_pos(any()) -> fun().
guard01_pos(F)
    when is_function(F, 1) -> F.

-spec guard02_pos(any()) -> f1().
guard02_pos(F)
    when is_function(F, 1) -> F.

-spec guard03_neg(any()) -> f2().
guard03_neg(F)
    when is_function(F, 1) -> F.

-spec a_to_a(none()) -> a.
a_to_a(a) -> a.

-spec unsound() -> any().
unsound() ->
    F = fun a_to_a/1,
    if
        is_function(F, 1) ->
            F(3);
        true ->
            a
    end.

-type f4(T) :: fun((...) -> T).

-type f5(T) :: fun((term()) -> T).

-type f6(T) :: fun((term(), term()) -> T).

-spec f4_to_f(f4(term())) -> fun().
f4_to_f(F) -> F.

-spec f5_to_f4(f5(term())) -> f4(term()).
f5_to_f4(F) -> F.

-spec f5_to_f4_cov(f5(a)) -> f4(a | b).
f5_to_f4_cov(F) -> F.

-spec f5_to_f4_cov_neg(f5(a | b)) -> f4(a).
f5_to_f4_cov_neg(F) -> F.

-spec f5_or_f6_to_f4(f5(atom()) | f6(number())) -> f4(atom() | number()).
f5_or_f6_to_f4(F) -> F.

-spec apply_f4_neg(f4(number())) -> boolean().
apply_f4_neg(F) -> F(a).

-spec f4_id_T(f4(T)) -> f4(T).
f4_id_T(F) -> F.

-spec apply_f4_id(f4(a)) -> f4(a | b).
apply_f4_id(F) -> f4_id_T(F).

-spec fun_to_f4_neg(fun()) -> f4(term()).
fun_to_f4_neg(F) -> F.

-spec fun2_to_f4(fun((term()) -> a)) -> f4(a | b).
fun2_to_f4(F) -> F.

-spec fun3_to_f4_neg(fun((term()) -> a | b)) -> f4(a).
fun3_to_f4_neg(F) -> F.

-spec f4_to_fun_neg(f4(a)) -> fun((term()) -> (a | b)).
f4_to_fun_neg(F) -> F.

-spec map_f1(fun((term()) -> atom()), [term()]) -> [atom()].
map_f1(F, Ts) -> lists:map(F, Ts).

-spec map_f2(fun((term()) -> atom()), [term()]) -> [atom()].
map_f2(F, Ts) -> lists:map(F, Ts).

-spec map_f3_neg(fun((...) -> atom()), [term()]) -> [atom()].
map_f3_neg(F, Ts) -> lists:map(F, Ts).

-spec map_f4_neg(fun((...) -> A), [term()]) -> [A].
map_f4_neg(F, Ts) -> lists:map(F, Ts).

-spec map_f5(fun((term()) -> atom()), [term()]) -> [atom()].
map_f5(F, Ts) -> map_f4_neg(F, Ts).
