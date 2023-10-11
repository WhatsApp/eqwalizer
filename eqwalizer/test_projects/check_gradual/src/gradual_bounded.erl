%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(gradual_bounded).

-compile([export_all, nowarn_export_all]).

-type dyn(T) :: eqwalizer:dynamic(T).

-spec bd_unsafe_01(dyn(a | b)) -> c.
bd_unsafe_01(V) -> V.

-spec bd_unsafe_neg(dyn(a) | b) -> c.
bd_unsafe_neg(V) -> V.

-spec bd_unsafe_02(dyn(a) | b) -> b.
bd_unsafe_02(V) -> V.

-spec bd_coerce(a) -> dyn(a).
bd_coerce(V) -> V.

-spec bd_coerce_safe_neg(a) -> dyn(b).
bd_coerce_safe_neg(V) -> V.

-spec bd_map_01(dyn(#{atom() => atom()}))
    -> atom().
bd_map_01(M) -> maps:get(at, M).

-spec bd_map_02(dyn(#{atom() => atom()}))
    -> binary().
bd_map_02(M) -> maps:get(at, M).

-spec bd_map_03(dyn(not_a_map))
    -> atom().
bd_map_03(M) -> maps:get(at, M).

-spec bd_map_put_01_print
    (dyn(#{b => c}))
    -> err.
bd_map_put_01_print(M) ->
    V = maps:put(42, a, M),
    eqwalizer:reveal_type(V).

-spec bd_map_put_02_print
    (dyn(#{b => c}))
    -> err.
bd_map_put_02_print(M) ->
    V = maps:put(a, b, M),
    eqwalizer:reveal_type(V).

-record(rec, {field :: atom()}).

-spec bd_record(dyn(#rec{})) -> none().
bd_record(R) -> R#rec.field.

-spec bd_tuple_01(dyn({a, b})) -> c.
bd_tuple_01({V, _}) -> V.

-spec bd_tuple_02(dyn(none())) -> c.
bd_tuple_02({V, _}) -> V.

-spec bd_distrib_print
    (dyn({a, b})) -> err.
bd_distrib_print({a, V}) -> eqwalizer:reveal_type(V).

-spec bd_generic_fun(dyn(T), T) -> T.
bd_generic_fun(_, _) -> throw(impl).

-spec bd_generics_01()
    -> atom() | number().
bd_generics_01() ->
    bd_generic_fun(test, 42).

-spec bd_generics_02_neg()
    -> number().
bd_generics_02_neg() ->
    bd_generic_fun(test, 42).

-spec bd_generics_03(dyn(number()))
    -> atom().
bd_generics_03(N) ->
    bd_generic_fun(test, N).

-spec bd_generics_04_neg(dyn(number()))
    -> number().
bd_generics_04_neg(N) ->
    bd_generic_fun(test, N).
