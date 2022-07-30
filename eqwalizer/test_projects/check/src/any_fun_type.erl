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
