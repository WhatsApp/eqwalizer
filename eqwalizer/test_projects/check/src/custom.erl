%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(custom).

-import(maps, [get/2, get/3]).
-compile([export_all, nowarn_export_all]).

-record(foo, {
    a :: ok | error,
    b :: number(),
    c :: string()
}).

% element/2 - basic examples

-spec element_2_basic_1({atom(), number(), string()}) -> atom().
element_2_basic_1(Tup) ->
    element(1, Tup).

-spec element_2_basic_2_neg({atom(), number(), string(), map()}) -> atom().
element_2_basic_2_neg(Tup) ->
    element(4, Tup).

-spec element_2_basic_3_neg({atom(), number(), string()}) -> atom().
element_2_basic_3_neg(Tup) ->
    element(42, Tup).

% element/2 - union examples

-spec element_2_union_1({atom(), number() | string()} | {number(), atom()}) -> number() | string() | atom().
element_2_union_1(Tup) ->
    element(2, Tup).

-spec element_2_union_2_neg({atom(), number() | string()} | {number(), atom()}) -> map().
element_2_union_2_neg(Tup) ->
    element(2, Tup).

-spec element_2_union_3_neg({atom(), string()} | list()) -> string().
element_2_union_3_neg(Tup) ->
    element(2, Tup).

-spec element_2_union_4_neg({c, d, e, f} | {a, b} | {b, c, d}) -> atom().
element_2_union_4_neg(Tup) ->
    element(42, Tup).

% element/2 - dynamic index examples

-spec element_2_dynindex_1_neg(pos_integer(), {atom(), number(), string()}) -> map().
element_2_dynindex_1_neg(N, Tup) ->
    element(N, Tup).

-spec element_2_dynindex_2_neg(pos_integer(), {atom(), atom()} | {atom(), atom(), number()}) -> atom().
element_2_dynindex_2_neg(N, Tup) ->
    element(N, Tup).

% element/2 - tuple() examples

-spec element_2_anytuple_1_neg(tuple()) -> atom().
element_2_anytuple_1_neg(Tup) ->
    element(1, Tup).

-spec element_2_anytuple_2_neg(tuple() | {number(), atom()}) -> atom().
element_2_anytuple_2_neg(Tup) ->
    element(1, Tup).

% element/2 - record examples

-spec element_2_record_1(#foo{}) -> foo.
element_2_record_1(Rec) ->
    element(1, Rec).

-spec element_2_record_2(#foo{}) -> ok | error.
element_2_record_2(Rec) ->
    element(2, Rec).

-spec element_2_record_3(#foo{}) -> ok.
element_2_record_3(Rec) when Rec#foo.a =/= error ->
    element(2, Rec).

-spec element_2_record_4_neg(pos_integer(), #foo{}) -> atom().
element_2_record_4_neg(N, Rec) ->
    element(N, Rec).

% element/2 - none examples

-spec element_2_none_1(none()) -> number().
element_2_none_1(Tup) ->
    element(42, Tup).

-spec element_2_none_2(pos_integer(), none()) -> number().
element_2_none_2(N, Tup) ->
    element(N, Tup).

-spec map_get_2_1(
    pid(), #{pid() => atom()}
) -> atom().
map_get_2_1(K, M) ->
    maps:get(K, M).

-spec map_get_2_2_neg(
    pid(), #{pid() => number()}
) -> atom().
map_get_2_2_neg(K, M) ->
    maps:get(K, M).

-spec map_get_2_3(
    #{a => atom(), n => number()}
) -> {atom(), number()}.
map_get_2_3(M) ->
    {maps:get(a, M), maps:get(n, M)}.

-spec map_get_2_4(
    pid(), #{pid() => atom()}
) -> atom().
map_get_2_4(K, M) ->
    Res = maps:get(K, M),
    Res.

-spec map_get_2_5_neg(
    pid(), #{pid() => number()}
) -> atom().
map_get_2_5_neg(K, M) ->
    Res = maps:get(K, M),
    Res.

-spec map_get_2_6(
    #{a => atom(), n => number()}
) -> {atom(), number()}.
map_get_2_6(M) ->
    Res = {
        get(a, M),
        get(n, M)
    },
    Res.

-spec map_get_2_6_2(
    #{a => atom(), n => number()}
) -> {atom(), number()}.
map_get_2_6_2(M) ->
    Res = {
        map_get(a, M),
        map_get(n, M)
    },
    Res.

-spec map_get_2_7_neg(
    term()
) -> term().
map_get_2_7_neg(M) ->
    get(a, M).

-spec map_get_2_8_neg(
    term()
) -> term().
map_get_2_8_neg(M) ->
    Res = maps:get(a, M),
    Res.

-spec map_get_2_9_neg(
    term()
) -> term().
map_get_2_9_neg(M) ->
    maps:get(a, M, false).

-spec map_get_2_10_neg(
    term()
) -> term().
map_get_2_10_neg(M) ->
    Res = maps:get(a, M, false),
    Res.

-spec map_get_2_11(
    pid(), #{pid() => atom()}
) -> atom().
map_get_2_11(K, M) ->
    maps:get(K, M, undefined).

-spec map_get_2_12(
    pid(), #{pid() => atom()}
) -> atom().
map_get_2_12(K, M) ->
    Res = maps:get(K, M, undefined),
    Res.

-spec map_get_2_13_neg(
    pid(), #{pid() => atom()}
) -> atom().
map_get_2_13_neg(K, M) ->
    maps:get(K, M, 0).

-spec map_get_2_14_neg(
    pid(), #{pid() => atom()}
) -> atom().
map_get_2_14_neg(K, M) ->
    Res = maps:get(K, M, 0),
    Res.

-spec map_get_3_15(
    #{a => atom(), n => number()}
) -> {atom(), number()}.
map_get_3_15(M) ->
    {
        maps:get(a, M, undefined),
        maps:get(n, M, undefined)
    }.

-spec keydelete() ->
    [{key, value}].
keydelete() ->
    Res = lists:keydelete(
        key,
        1,
        [{key, value}]
    ),
    Res.

-spec keyreplace() ->
    [{key1, value1} | {key2, value2}].
keyreplace() ->
    Res = lists:keyreplace(
        key,
        1,
        [{key1, value1}],
        {key2, value2}
    ),
    Res.

-spec keyfind_1() -> false | term().
keyfind_1() ->
    Res = lists:keyfind(a, 1, []),
    Res.

-spec keyfind_2() -> false | {c, d}.
keyfind_2() ->
    Res = lists:keyfind(a, 1, [{c, d}]),
    Res.

% We could error here by bounding
% third param <: [tuple]
-spec keyfind_3_wip_neg() -> term().
keyfind_3_wip_neg() ->
    lists:keyfind(a, 1, [#{}]).

-record(rec, {field :: number()}).

-spec keyfind_4() ->
    false | #rec{}.
keyfind_4() ->
    Res = lists:keyfind(
        a,
        #rec.field,
        [#rec{field = 4}]
    ),
    Res.

-spec keyfind_5() ->
    false | {c, d} | {e, f}.
keyfind_5() ->
    Res = lists:keyfind(
        a,
        1,
        [{c, d}, {e, f}]
    ),
    Res.

-spec keysearch_1() ->
    false | {value, term()}.
keysearch_1() ->
    Res = lists:keysearch(a, 1, []),
    Res.

-spec keysearch_2() ->
    false | {value, {c, d}}.
keysearch_2() ->
    Res = lists:keysearch(a, 1, [{c, d}]),
    Res.

-spec keysearch_3_neg() -> nok.
keysearch_3_neg() ->
    Res = lists:keysearch(a, 1, [#{}]),
    Res.

-spec keysearch_4() ->
    false | {value, #rec{}}.
keysearch_4() ->
    Res = lists:keysearch(
        a,
        #rec.field,
        [#rec{field = 4}]
    ),
    Res.

-spec keysearch_5() ->
    false | {value, {c, d} | {e, f}}.
keysearch_5() ->
    Res = lists:keysearch(
        a,
        1,
        [{c, d}, {e, f}]
    ),
    Res.


-spec keystore_1() ->
    [{a, b} | {c, d} | {replacement}].
keystore_1() ->
    Res = lists:keystore(
        a, 1,
        [{a, b}, {c, d}],
        {replacement}
    ),
    Res.

-spec maps_filter_2_1()
        -> #{number() => atom()}.
maps_filter_2_1() ->
    M = #{1 => a, 2 => b},
    F = fun erlang:'=:='/2,
    maps:filter(F, M).

-spec maps_filter_2_2()
        -> #{number() => atom()}.
maps_filter_2_2() ->
    M = #{1 => a, 2 => b},
    maps:filter(
        fun (K, V) -> K =:= V end,
        M
    ).

-spec maps_filter_2_3_neg()
        -> #{number() => atom()}.
maps_filter_2_3_neg() ->
    M = #{1 => a, 2 => b},
    maps:filter(
        fun (_, _) -> self() end,
        M
    ).

% should pass if we fully support
%  lambdas assigned to vars
-spec maps_filter_2_4_wip()
        -> #{number() => atom()}.
maps_filter_2_4_wip() ->
    M = #{1 => a, 2 => b},
    F = fun (K, V) -> K =:= V end,
    maps:filter(F, M).


-spec maps_filter_2_5_neg()
        -> #{number() => atom()}.
maps_filter_2_5_neg() ->
    M = #{1 => a, 2 => b},
    F = fun self/0,
    maps:filter(F, M).

-spec maps_filter_2_6_neg()
        -> #{number() => atom()}.
maps_filter_2_6_neg() ->
    M = #{1 => a, 2 => b},
    F = fun lists:filter/2,
    maps:filter(F, M).

-spec maps_filter_2_7(
    #{K => V}
) -> #{K => V}.
maps_filter_2_7(M) ->
    F = fun erlang:'=:='/2,
    maps:filter(F, M).

-spec maps_filter_2_8(
    #{K => V}
) -> #{K => V}.
maps_filter_2_8(M) ->
    maps:filter(fun erlang:'=:='/2, M).

-spec maps_filter_2_9_neg(
    boolean(),
    #{K => V}
) -> #{K => V}.
maps_filter_2_9_neg(B, M) ->
    X = case B of
            true -> M;
            false -> a
        end,
    maps:filter(fun erlang:'=:='/2, X).

-spec maps_filter_2_8()
        -> #{a => a, b => b}.
maps_filter_2_8() ->
    M = #{a => a, b => b},
    F = fun erlang:'=:='/2,
    maps:filter(F, M).

-spec maps_filter_2_10_neg() -> nok.
maps_filter_2_10_neg() ->
    F = fun erlang:'=:='/2,
    maps:filter(F, non_kv),
    nok.

-spec map_filter_empty(Pred) -> Map when
    Pred :: fun((Key, Value) -> boolean()),
    Map :: #{Key => Value}.
map_filter_empty(Pred) ->
    maps:filter(Pred, #{}).

-spec maps_map_2_1()
        -> #{number() => boolean()}.
maps_map_2_1() ->
    M = #{1 => a, 2 => b},
    F = fun erlang:'=:='/2,
    maps:map(F, M).

-spec maps_map_2_2()
        -> #{number() => boolean()}.
maps_map_2_2() ->
    M = #{1 => a, 2 => b},
    maps:map(
        fun (K, V) -> K =:= V end,
        M
    ).

-spec maps_map_2_3_neg()
        -> #{number() => boolean()}.
maps_map_2_3_neg() ->
    M = #{1 => a, 2 => b},
    maps:map(
        fun (_, _) -> self() end,
        M
    ).

% should pass if we fully support
%  lambdas assigned to vars
-spec maps_map_2_4_wip()
        -> #{number() => boolean()}.
maps_map_2_4_wip() ->
    M = #{1 => a, 2 => b},
    F = fun (K, V) -> K =:= V end,
    maps:map(F, M).


-spec maps_map_2_5_neg()
        -> #{number() => boolean()}.
maps_map_2_5_neg() ->
    M = #{1 => a, 2 => b},
    F = fun self/0,
    maps:map(F, M).

-spec maps_map_2_6_neg()
        -> #{number() => boolean()}.
maps_map_2_6_neg() ->
    M = #{1 => a, 2 => b},
    F = fun lists:map/2,
    maps:map(F, M).

-spec maps_map_2_7(
    #{K => term()}
) -> #{K => boolean()}.
maps_map_2_7(M) ->
    F = fun erlang:'=:='/2,
    maps:map(F, M).

% return ty of maps:map/2
% is always DictMap
-spec maps_map_2_9_neg()
        -> #{a => a, b => b}.
maps_map_2_9_neg() ->
    M = #{a => a, b => b},
    F = fun erlang:'=:='/2,
    maps:map(F, M).

-spec maps_map_2_10_neg()
        -> nok.
maps_map_2_10_neg() ->
    F = fun erlang:'=:='/2,
    maps:map(F, non_kv),
    nok.

-spec maps_fold_3_1()
        -> [number() | a | b].
maps_fold_3_1() ->
    M = #{1 => a, 2 => b},
    X = M,
    Res = maps:fold(
        fun (K, V, Acc) ->
            [K, V] ++ Acc
        end, [], X),
    Res.

-spec maps_fold_3_2_neg()
        -> [number() | a | b].
maps_fold_3_2_neg() ->
    M = #{1 => a, 2 => b},
    maps:fold(
        fun (K, V) -> [K, V] end, [], M).

-spec maps_fold_3_3_neg()
        -> [number() | a | b].
maps_fold_3_3_neg() ->
    M = #{1 => a, 2 => b},
    maps:fold(
        fun (_, _, Acc) -> [Acc] end,
        [],
        M
    ).

-spec maps_fold_3_5_neg() -> nok.
maps_fold_3_5_neg() ->
    maps:fold(
        fun (_, _, Acc) -> Acc end,
        [],
        non_kv
    ).

-spec maps_fold_3_6(none()) ->
    #{number() => number()}.
maps_fold_3_6(None) ->
    maps:fold(
        None,
        #{},
        #{1 => 1}
    ).

-spec maps_fold_3_7(none()) -> none().
maps_fold_3_7(None) ->
    maps:fold(
        None,
        None,
        None
    ).

-spec maps_fold_4_neg
    (#{atom => atom()}) -> atom().
maps_fold_4_neg(M) ->
    maps:fold(
        fun (_K, A, _Acc) -> A end,
        [],
        M
    ).

-spec folder_good(_, _, _) -> number().
folder_good(_, _, _) -> 1.

-spec folder_bad(_, _, Acc) -> [Acc].
folder_bad(_, _, Acc) -> [Acc].

-spec maps_fold_3_8() -> number().
maps_fold_3_8() ->
    maps:fold(
        fun folder_good/3,
        0,
        #{1 => a}
    ).

-spec maps_fold_3_9_neg() -> term().
maps_fold_3_9_neg() ->
    maps:fold(
        fun folder_bad/3,
        [],
        #{1 => a}
    ).

-spec maps_fold_4_1(#{atom() => {b, binary()} | {i, integer()}}) -> #{atom() => binary() | integer()}.
maps_fold_4_1(M) ->
    maps:fold(
        fun
            (K, {i, I}, Acc) ->
                maps:put(K, I, Acc);
            (K, {b, B}, Acc) ->
                maps:put(K, B, Acc)
        end,
        #{},
        M
    ).

-spec maps_fold_4_2(#{atom() => {b, binary()} | {i, integer()} | {a, atom()}}) -> #{atom() => binary() | integer() | atom()}.
maps_fold_4_2(M) ->
    maps:fold(
        fun
            (K, {i, I}, Acc) ->
                maps:put(K, I, Acc);
            (K, {b, B}, Acc) ->
                maps:put(K, B, Acc);
            (K, {a, A}, Acc) ->
                maps:put(K, A, Acc)
        end,
        #{},
        M
    ).

-spec maps_fold_4_3_neg(#{atom() => {b, binary()} | {i, integer()} | {a, atom()}}) -> [binary()] | [integer()] | [atom()].
maps_fold_4_3_neg(M) ->
    maps:fold(
        fun
            (_K, {i, I}, Acc) ->
                [I | Acc];
            (_K, {b, B}, Acc) ->
                [B | Acc];
            (_K, {a, A}, Acc) ->
                [A | Acc]
        end,
        [],
        M
    ).

-spec lists_filtermap_1() -> [number()].
lists_filtermap_1() ->
    lists:filtermap(
        fun is_function/1, [1, 2]
    ).

-spec lists_filtermap_2() ->
    [number() | a].
lists_filtermap_2() ->
    lists:filtermap(
        fun (1) -> {true, a} end,
        [1, 2, 3]
    ).

-spec lists_filtermap_2_neg() ->
    [number() | a].
lists_filtermap_2_neg() ->
    lists:filtermap(
        fun erlang:binary_to_list/1,
        [1, 2, 3]
    ).

-spec lists_filtermap_3() ->
    [a | number()].
lists_filtermap_3() ->
    lists:filtermap(
        fun (1) -> {true, a};
            (2) -> true end,
        [1, 2, 3]
    ).

-spec lists_filtermap_4_neg() -> nok.
lists_filtermap_4_neg() ->
    lists:filtermap(
        fun (1) -> {true, a};
            (2) -> true;
            (3) -> wrong_ret end,
        [1, 2, 3]
    ),
    nok.

-spec lists_filtermap_5_neg() -> nok.
lists_filtermap_5_neg() ->
    lists:filtermap(
        fun (1) -> {true, a};
            (2) -> true end,
        not_a_list
    ),
    nok.

-spec lists_filtermap_6_neg() -> nok.
lists_filtermap_6_neg() ->
    lists:filtermap(
        fun (wrong_arity) ->
            {true, a}
        end,
        [1, 2, 3]
    ),
    nok.

-spec lists_filtermap_7() ->
    nok.
lists_filtermap_7() ->
    lists:filtermap(
        fun (1) -> {true, a};
            (X) -> case X of
                       true ->
                           {true, a};
                       false ->
                           false
                   end
        end,
        [1, 2, 3]
    ),
    nok.

-spec lists_filtermap_8_neg() ->
    nok.
lists_filtermap_8_neg() ->
    lists:filtermap(
        fun (1) -> {true, a};
            (X) ->
                Res = case X of
                       true ->
                           {true, a};
                       false ->
                           wrong_ret
                   end,
                Res
        end,
        [1, 2, 3]
    ),
    nok.

-spec ab_queue() -> queue:queue(a | b).
ab_queue() ->
    throw(not_implemented).

-spec queue_filter_1() -> queue:queue(a | b).
queue_filter_1() ->
    queue:filter(
        fun is_function/1, ab_queue()
    ).

-spec queue_filter_2() ->
    queue:queue(a | b | c).
queue_filter_2() ->
    queue:filter(
        fun (a) -> [c] end,
        ab_queue()
    ).

-spec queue_filter_3() ->
    queue:queue(a | b).
queue_filter_3() ->
    queue:filter(
        fun (a) -> union([a], [b]);
            (b) -> true end,
        ab_queue()
    ).

-spec queue_filter_4_neg() -> nok.
queue_filter_4_neg() ->
    queue:filter(
        fun (a) -> [a];
            (b) -> true;
            (c) -> wrong_ret end,
        ab_queue()
    ),
    nok.

-spec queue_filter_5_neg() -> nok.
queue_filter_5_neg() ->
    queue:filter(
        fun (1) -> {true, a};
            (2) -> true end,
        not_a_queue
    ),
    nok.

-spec queue_filter_6_neg() -> nok.
queue_filter_6_neg() ->
    queue:filter(
        fun (wrong, arity) ->
            [a]
        end,
        ab_queue()
    ),
    nok.

-spec queue_filter_7_neg() ->
    nok.
queue_filter_7_neg() ->
    queue:filter(
        fun (1) -> {true, a};
            (X) -> case X of
                       true ->
                           [a];
                       false ->
                           false
                   end
        end,
        ab_queue()
    ),
    nok.

-spec queue_filter_8_neg() ->
    nok.
queue_filter_8_neg() ->
    queue:filter(
        fun (a) -> [a];
            (X) ->
                Res = case X of
                          true ->
                              [a];
                          false ->
                              wrong_ret
                      end,
                Res
        end,
        ab_queue()
    ),
    nok.

-spec queue_filter_9_neg() ->
    nok.
queue_filter_9_neg() ->
    queue:filter(
        fun list_to_atom/1,
        ab_queue()
    ),
    nok.

-spec queue_filter_10_neg() ->
    nok.
queue_filter_10_neg() ->
    queue:filter(
        fun atom_to_list/1,
        ab_queue()
    ),
    nok.

-spec queue_filter_11_neg() ->
    nok.
queue_filter_11_neg() ->
    queue:filter(
        fun atom_to_list/1,
        ab_queue()
    ),
    nok.

-spec queue_filter_12_neg(
   queue:queue(a) | queue:queue(b)
) -> ok.
queue_filter_12_neg(Q) ->
    queue:filter(
        fun atom_to_list/1,
        Q
    ),
    ok.

-spec queue_filter_13_neg(none()) -> ok.
queue_filter_13_neg(Q) ->
    queue:filter(
        fun atom_to_list/1,
        Q
    ),
    ok.

-spec keystore_2() ->
    [{foo, b} | {c, d} | {replacement}].
keystore_2() ->
    Res = lists:keystore(
        a, 1,
        [{foo, b}, {c, d}],
        {replacement}
    ),
    Res.

-spec keystore_3() ->
    [{foo,b} | {c,d} | {replacement}].
keystore_3() ->
    Res = lists:keystore(
        a, 1,
        [{foo, b}, {c, d}],
        {replacement}
    ),
    Res.

-spec keystore_4_neg() -> nok.
keystore_4_neg() ->
    % runtime error
    lists:keystore(
        a, 1,
        [{foo, b}, {c, d}],
        non_tuple
    ).

-spec keystore_5_neg() -> nok.
keystore_5_neg() ->
    % erl undefined behavior
    % succeeds at run time
    % returns [non_tuple, []],
    lists:keystore(
        a, 1,
        [non_tuple],
        {replacement}
    ).

-spec keystore_6_neg() -> nok.
keystore_6_neg() ->
    % runtime error
    lists:keystore(
        a, 1,
        non_list,
        {replacement}
    ).

-spec keystore_7(none()) ->
    [{replacement}].
keystore_7(None) ->
    lists:keystore(
        a, 1,
        None,
        {replacement}
    ).

-spec keystore_8(none()) -> [none()].
keystore_8(None) ->
    lists:keystore(
        a, 1,
        None,
        None
    ).

-type my_tup() :: {k1, v1} | {k2, v2}.

-spec keytake_3_1(term()) ->
    {value, my_tup(),[my_tup()]} | false.
keytake_3_1(X) ->
    lists:keytake(
        X,
        1,
        [{k1, v1}, {k2, v2}]
    ).

-spec keytake_3_2_neg() -> nok.
keytake_3_2_neg() ->
    lists:keytake(a, 1, non_tup),
    nok.

-spec keytake_3_3_neg() -> nok.
keytake_3_3_neg() ->
    lists:keytake(a, 1, non_list),
    nok.

-spec keytake_3_4_neg() -> nok.
keytake_3_4_neg() ->
    lists:keytake(a, non_num, []),
    nok.

-spec sum_1() -> integer().
sum_1() ->
    lists:sum([1, 2, 3]).

-spec sum_2_neg() -> integer().
sum_2_neg() ->
    lists:sum([1.0, 2, 3]).

-spec sum_3() -> number().
sum_3() ->
    lists:sum([1.0, 2, 3]).

-spec sum_4_neg() -> integer().
sum_4_neg() ->
    lists:sum([a, 1]).

-spec sum_5_neg() -> integer().
sum_5_neg() ->
    lists:sum(not_a_list).

-spec max_1() -> integer().
max_1() ->
    lists:max([1, 2, 3]).

-spec max_2_neg() -> integer().
max_2_neg() ->
    lists:max([1.0, 2, 3]).

-spec max_3() -> number().
max_3() ->
    lists:max([1.0, 2, 3]).

-spec max_4() -> a | integer().
max_4() ->
    lists:max([a, 1]).

-spec max_5_neg() -> integer().
max_5_neg() ->
    lists:max(not_a_list).

-spec min_1() -> integer().
min_1() ->
    lists:min([1, 2, 3]).

-spec min_2_neg() -> integer().
min_2_neg() ->
    lists:min([1.0, 2, 3]).

-spec min_3() -> number().
min_3() ->
    lists:min([1.0, 2, 3]).

-spec min_4() -> a | integer().
min_4() ->
    lists:min([a, 1]).

-spec min_5_neg() -> integer().
min_5_neg() ->
    lists:min(not_a_list).

-spec test_find_shape(#{k := v}) -> v.
test_find_shape(M) ->
    {ok, V} = maps:find(k, M),
    V.

-spec test_find_dict(#{pid() => v}) -> v.
test_find_dict(M) ->
    {ok, V} = maps:find(k, M),
    V.

-type plist(K, V) :: [K | {K, V}].

-spec 'test_plists_get_value/2'(
    plist(integer(), pid())) ->
        pid() | undefined.
'test_plists_get_value/2'(L) ->
    proplists:get_value(k, L).

-spec 'test_plists_get_value/2_overlap'(
    plist({k, v}, pid())) ->
    pid() | undefined | v.
'test_plists_get_value/2_overlap'(L) ->
    proplists:get_value(k, L).

-spec 'test_plists_get_value/2_neg'(
    plist(atom(), pid())) -> pid().
'test_plists_get_value/2_neg'(L) ->
    proplists:get_value(k, L).

-spec 'test_plists_get_value/3'(
    plist(reference(), pid())) ->
        pid() | integer().
'test_plists_get_value/3'(L) ->
    proplists:get_value(k, L, 3).

-spec 'test_plists_get_value/3_overlap'(
    plist({k, v1}, v2)) ->
    v1 | v2 | my_default.
'test_plists_get_value/3_overlap'(L) ->
    proplists:get_value(k, L, my_default).

-spec 'test_plists_get_value/3_neg'(
    plist(number(), v)) ->
    v.
'test_plists_get_value/3_neg'(L) ->
    proplists:get_value(k, L, my_default).

-spec 'test_plists_get_value/2_2_neg'()
        -> term().
'test_plists_get_value/2_2_neg'() ->
    proplists:get_value(k, b).

-spec 'test_plists_get_value/2_3'()
        -> default.
'test_plists_get_value/2_3'() ->
    proplists:get_value(k, []).

-spec 'test_plists_get_value/3_2'()
        -> my_default.
'test_plists_get_value/3_2'() ->
proplists:get_value(k, [], my_default).

-spec 'test_plists_get_value/3_2_neg'()
        -> term().
'test_plists_get_value/3_2_neg'() ->
    proplists:get_value(k, b, my_default).

-spec test_plists_get_bool1_neg
    (plist(a, b)) -> true.
test_plists_get_bool1_neg(L) ->
    proplists:get_bool(b, L).

-spec test_plists_get_all_values(
    plist(integer(), pid())) ->
    [pid() | default].
test_plists_get_all_values(L) ->
    proplists:get_all_values(k, L).

-spec test_plists_get_all_values2(
    plist({k, v}, pid())) ->
    [pid() | default | v].
test_plists_get_all_values2(L) ->
    proplists:get_all_values(k, L).

-spec test_plists_get_all_values3(
    plist(atom(), pid())) -> [pid()].
test_plists_get_all_values3(L) ->
    proplists:get_all_values(k, L).

-spec test_plists_get_all_values4()
        -> [].
test_plists_get_all_values4() ->
    proplists:get_all_values(k, []).

-spec test_plists_get_all_values5_neg()
        -> [].
test_plists_get_all_values5_neg() ->
    proplists:get_all_values(k, b).

-spec test_plists_get_bool2_neg()
        -> term().
test_plists_get_bool2_neg() ->
    proplists:get_bool(b, b).

-spec test_plists_get_keys1_neg
    ([a | b | {c, {d, d}}]) -> [c].
test_plists_get_keys1_neg(L) ->
    proplists:get_keys(L).

-spec test_plists_get_keys2
    ([a | b | {c, {d, d}}]) ->
        [a | b | c].
test_plists_get_keys2(L) ->
    proplists:get_keys(L).

-spec test_plists_get_keys3_neg()
        -> term().
test_plists_get_keys3_neg() ->
    proplists:get_keys(a).

-spec test_plists_get_keys4()
        -> [a].
test_plists_get_keys4() ->
    proplists:get_keys(
        [{a, b, c}]
    ).

-spec test_plists_get_values1(
    plist(reference(), a | pid())) ->
    a | pid().
test_plists_get_values1(L) ->
    proplists:get_value(k, L).

-spec test_plists_get_values2_neg() ->
    a | pid().
test_plists_get_values2_neg() ->
    proplists:get_value(k, b).

-spec test_plists_lookup1_neg()
    -> {b, true}.
test_plists_lookup1_neg() ->
proplists:lookup(self(), [a, {b, true}]).

-spec test_plists_lookup2_neg() -> term().
test_plists_lookup2_neg() ->
    proplists:lookup(a, b).

-spec test_plists_lookup3()
        -> none | {a, true} | {b, true}.
test_plists_lookup3() ->
proplists:lookup(self(), [a, {b, true}]).

-spec test_plists_lookup4()
        -> none.
test_plists_lookup4() ->
    proplists:lookup(k, []).

-spec test_plists_lookup_all1_neg()
        -> [{b, true}].
test_plists_lookup_all1_neg() ->
    proplists:lookup_all(
        self(),
        [a, {b, true}]
    ).

-spec test_plists_lookup_all2_neg()
        -> term().
test_plists_lookup_all2_neg() ->
    proplists:lookup_all(a, b).

-spec test_plists_lookup_all3()
        -> [{a, true} | {b, true}].
test_plists_lookup_all3() ->
    proplists:lookup_all(
        self(),
        [a, {b, true}]
    ).

-spec test_plists_lookup_all4()
        -> [].
test_plists_lookup_all4() ->
    proplists:lookup_all(
        self(),
        []
    ).

-spec test_plists_is_defined1() ->
    boolean().
test_plists_is_defined1() ->
    proplists:is_defined(self(), []).

-spec test_plists_is_defined2() ->
    boolean().
test_plists_is_defined2() ->
    proplists:is_defined(self(), [a]).

-spec test_plists_is_defined3() ->
    boolean().
test_plists_is_defined3() ->
    proplists:is_defined(
        self(),
        [{a, b}]
    ).

-spec test_plists_is_defined4_neg() ->
    boolean().
test_plists_is_defined4_neg() ->
    proplists:is_defined(self(), b).

-spec test_plists_delete1_neg() -> term().
test_plists_delete1_neg() ->
    proplists:delete(k, b).

-spec test_plists_delete2() -> [].
test_plists_delete2() ->
    proplists:delete(k, []).

-spec test_plists_delete3(
     plist(k1 | pid(), v1 | v2)
) -> plist(k1 | pid(), v1 | v2).
test_plists_delete3(L) ->
    proplists:delete(k, L).

-spec test_plists_split1(
    plist(a, b), [pid()]
) -> {[plist(a, b)], plist(a, b)}.
test_plists_split1(L, Ks) ->
    proplists:split(L, Ks).

-spec test_plists_split2_neg() -> term().
test_plists_split2_neg() ->
    proplists:split(b, []).

-spec test_plists_split3_neg() -> term().
test_plists_split3_neg() ->
    proplists:split([], b).

-spec test_plists_to_map1_neg()
    -> term().
test_plists_to_map1_neg() ->
    proplists:to_map(b).

-spec test_plists_to_map2_neg()
        -> term().
test_plists_to_map2_neg() ->
    proplists:to_map([{a, b, c}]).

-spec test_plists_to_map3_neg()
        -> #{a |d => b | pid()}.
test_plists_to_map3_neg() ->
    proplists:to_map(
        [{a, b}, c, {d, self()}]).

-spec test_plists_to_map4()
    -> #{a | c |d => b | true | pid()}.
test_plists_to_map4() ->
    proplists:to_map(
        [{a, b}, c, {d, self()}]).

-spec test_plists_to_map5()
        -> #{none() => none()}.
test_plists_to_map5() ->
    proplists:to_map([]).

-spec test_plists_from_map1() ->
    plist(a | pid(), b | true).
test_plists_from_map1() ->
    proplists:from_map(
        #{a => b, self() => true}
    ).

-spec test_plists_from_map2_neg() ->
    term().
test_plists_from_map2_neg() ->
    proplists:from_map(b).

-spec 'test_plists_get_value/2_4'()
        -> true | undefined.
'test_plists_get_value/2_4'() ->
    proplists:get_value(a, [a]).

-spec 'test_plists_get_value/2_5'
    (atom()) -> true | undefined.
'test_plists_get_value/2_5'(X) ->
    proplists:get_value(X, [a]).

-spec 'test_plists_get_value/3_3'()
        -> true | b.
'test_plists_get_value/3_3'() ->
    proplists:get_value(a, [a], b).

-spec app_env1_strict() -> number().
app_env1_strict() ->
    Res = application:get_env(app1),
    case Res of
        undefined -> 0;
        {ok, N} -> N
    end.

-spec app_env2_strict() -> number().
app_env2_strict() ->
    Res = application:get_env(app1, key1),
    case Res of
        undefined -> 0;
        {ok, N} -> N
    end.

-spec app_env3_strict() -> number().
app_env3_strict() ->
    A = app1,
    K = key,
    U = undefined,
    application:get_env(A, K, U).

-spec flatten1_strict() -> [atom()].
flatten1_strict() ->
    In = [a, [b, c]],
    lists:flatten(In).

-spec flatten2_strict() -> [atom()].
flatten2_strict() ->
    In = [a, [b, c]],
    Tail = [x, y, z],
    lists:flatten(In, Tail).

-spec file_consult_neg1() -> [anything].
file_consult_neg1() ->
    {ok, Res} = file:consult(some_file),
    Res.

-spec file_consult_neg2() -> nok.
file_consult_neg2() ->
    file:consult(some_file).

-spec keysort() -> [{a | b, c | d}].
keysort() ->
    lists:keysort(2, [{a, c}, {b, d}]).

-spec keysort2() -> [{a | b, c | d}].
keysort2() ->
    lists:keysort(2, []).

-spec keysort3_neg() -> none().
keysort3_neg() ->
    lists:keysort(2, [{a, c}, {b, d}]).

-spec keysort4_neg() -> none().
keysort4_neg() ->
    lists:keysort(1, [3]).

-spec gb_sets() -> gb_sets:set(number()).
gb_sets() ->
    gb_sets:add(1, gb_sets:empty()).

-spec filtermap_none() -> [none()].
filtermap_none() ->
    lists:filtermap(
        fun(_) -> false end,
        [1, 3, 3]
    ).

-spec filtermap_neg() ->
    [integer()].
filtermap_neg() ->
    lists:filtermap(
        fun(X) when X div 2 =:= 0 ->
            {true, integer_to_list(X)};
        (X) ->
            X < 10
        end,
        [1, 2, 3, 4]
    ).

-spec filtermap_ty_change() ->
    [string()].
filtermap_ty_change() ->
    lists:filtermap(
        fun(X) when X div 2 =:= 0 ->
            {true, integer_to_list(X)};
        (_) ->
            false
        end,
        [1, 2, 3, 4]
    ).

-spec min1(integer(), integer()) ->
    integer().
min1(X, Y) ->
    erlang:min(X, Y).

-spec min2(atom(), integer()) ->
    atom() | integer().
min2(X, Y) ->
    erlang:min(X, Y).

-spec min3_neg(atom(), integer()) ->
    none().
min3_neg(X, Y) ->
    erlang:min(X, Y).

-spec max1(integer(), integer()) ->
    integer().
max1(X, Y) ->
    erlang:max(X, Y).

-spec max2(atom(), integer()) ->
    atom() | integer().
max2(X, Y) ->
    erlang:max(X, Y).

-spec max3_neg(atom(), integer()) ->
    none().
max3_neg(X, Y) ->
    erlang:max(X, Y).

-spec abs1(neg_integer()) ->
    non_neg_integer().
abs1(N) ->
    abs(N).

-spec abs2(float()) -> float().
abs2(N) ->
    abs(N).

-spec abs3(number()) -> number().
abs3(N) ->
    abs(N).

-spec abs4(float()) -> non_neg_integer().
abs4(N) ->
    abs(N).

-spec abs5(integer()) ->
    non_neg_integer().
abs5(N) ->
    abs(N).

-spec abs6_neg(a) -> number().
abs6_neg(Atom) ->
    abs(Atom).

-spec seq3_1_wip_neg() ->
    [non_neg_integer()].
seq3_1_wip_neg() -> lists:seq(-1, -2, -1).

-spec seq3_2_wip_neg() -> [pos_integer()].
seq3_2_wip_neg() -> lists:seq(0, 2, 1).

-spec seq3_3() -> [pos_integer()].
seq3_3() -> lists:seq(1, 2, 1).

-spec seq3_4_wip_neg() -> [integer()].
seq3_4_wip_neg() -> lists:seq(a, 2, 1).

-spec seq3_5_neg() -> [integer()].
seq3_5_neg() -> lists:seq(1, a, 1).

-spec seq3_6_neg() -> [integer()].
seq3_6_neg() -> lists:seq(1, 2, a).

-spec seq3_7(pos_integer(),
    pos_integer()
) -> [pos_integer()].
seq3_7(X, Y) ->
    lists:seq(X, X - Y, X).

-spec seq3_7_wip_neg(non_neg_integer(),
    non_neg_integer()
) -> [non_neg_integer()].
seq3_7_wip_neg(X, Y) ->
    lists:seq(X, X - Y, -1).

-spec seq3_8_wip_neg() -> [pos_integer()].
seq3_8_wip_neg() ->
    lists:seq(0, 1, 1).

-spec seq2_1_wip_neg() -> [non_neg_integer()].
seq2_1_wip_neg() -> lists:seq(-1, -2).

-spec seq2_2_wip_neg() -> [pos_integer()].
seq2_2_wip_neg() -> lists:seq(0, 2).

-spec seq2_3() -> [pos_integer()].
seq2_3() -> lists:seq(1, 2).

-spec seq2_4_wip_neg() -> [integer()].
seq2_4_wip_neg() -> lists:seq(a, 2).

-spec seq2_5_neg() -> [integer()].
seq2_5_neg() -> lists:seq(1, a).

-spec seq2_6(pos_integer(),
    pos_integer()
) -> [pos_integer()].
seq2_6(X, Y) ->
    lists:seq(X, X - Y).

-spec seq2_7(non_neg_integer(),
    non_neg_integer()
) -> [non_neg_integer()].
seq2_7(X, Y) ->
    lists:seq(X, X - Y).

-spec seq2_8() -> [pos_integer()].
seq2_8() ->
    lists:seq($a, $z).

-spec seq2_9_wip_neg() -> [pos_integer()].
seq2_9_wip_neg() ->
    lists:seq(0, 1).

-spec system_time0() -> pos_integer().
system_time0() ->
    erlang:system_time().

-spec system_time1() -> pos_integer().
system_time1() ->
    erlang:system_time(second).

-spec keystore_neg() -> term().
keystore_neg() ->
    lists:keystore(a, 0, [], {}).

-spec keysort_neg() -> term().
keysort_neg() ->
    lists:keysort(-1, []).

-spec tc1() ->
    {integer(), result}.
tc1() ->
    timer:tc(
        fun() ->
            result
        end
    ).

-spec tc1_neg() -> pid().
tc1_neg() ->
    timer:tc(
        fun() ->
            err
        end
    ).

-spec ets_lookup_1_neg(term()) ->
    [{whatev} | {2}].
ets_lookup_1_neg(Any) ->
    ets:lookup(tab, Any).

-spec ets_lookup_2_neg(term()) -> pid().
ets_lookup_2_neg(Any) ->
    ets:lookup(tab, Any).

-spec ets_lookup_3_neg(term()) -> pid().
ets_lookup_3_neg(Any) ->
    ets:lookup("not atom", Any).

-spec ets_lookup_4_neg(term()) ->
    [tuple()].
ets_lookup_4_neg(Any) ->
    ets:lookup(tab, Any).

-spec ets_lookup_5(term()) ->
    [term()].
ets_lookup_5(Any) ->
    ets:lookup(tab, Any).

-spec ets_tab2list_1_neg(atom()) ->
    [tuple()].
ets_tab2list_1_neg(Atom) ->
    ets:tab2list(Atom).

-spec ets_tab2list_2_neg(atom()) ->
    [{whatev} | {2}].
ets_tab2list_2_neg(Atom) ->
    ets:tab2list(Atom).

-spec ets_tab2list_3_neg() -> term().
ets_tab2list_3_neg() ->
    ets:tab2list("not atom").

-spec ets_tab2list_4(atom()) ->
    [term()].
ets_tab2list_4(Atom) ->
    ets:tab2list(Atom).

-spec flatten1_strict2() ->
    [atom() | number()].
flatten1_strict2() ->
    In = [a, [b, c], [1, 2, 3]],
    lists:flatten(In).

-spec flatten2_strict2() ->
    [atom() | number()].
flatten2_strict2() ->
    In = [a, [b, c]],
    Tail = [1, 2, 3],
    lists:flatten(In, Tail).

% test flattening of aliases
-type mylist(A, B) :: [A | [A | B]].

-spec flatten1_strict3(mylist(X,Y))
    -> [X | Y].
flatten1_strict3(L) -> lists:flatten(L).

-spec flatten2_strict3(mylist(X,Y))
    -> [X | Y | number()].
flatten2_strict3(L) ->
    lists:flatten(L, [1,2,3]).

-spec flatten2_strict4(mylist(X,Y))
    -> [X | Y | [X | Y]].
flatten2_strict4(L) ->
    lists:flatten(L, L).

-spec flatten2_strict5_neg() -> term().
flatten2_strict5_neg() ->
    lists:flatten([], 1).

-type reclist(A) :: [A | reclist(A)].
-spec flatten1_strict4(reclist(A))
    -> [A].
flatten1_strict4(L) -> lists:flatten(L).

-spec flatten1_strict5_neg() -> term().
flatten1_strict5_neg() ->
    lists:flatten(3).

-type reclist2() :: [nil] | [reclist2()].
-spec flatten_reclist2
    (reclist2()) -> [nil].
flatten_reclist2(RL) ->
    lists:flatten(RL).

-type reclist3(A,B) ::
    [ {A,B} | reclist3(B,A)].
-spec flatten_reclist3_neg (reclist3(A,B))
    -> [{A,B}].
flatten_reclist3_neg(X)
    -> lists:flatten(X).

-spec union(T, U) -> T | U.
union(T, _) -> T.

-type id(T) :: T.
-type id2(T) :: id(T).
-type infinikey(T) :: [T | infinikey(T)].

-spec maps_without_1() -> #{a => ka}.
maps_without_1() ->
    maps:without([], #{a => ka}).

-spec maps_without_2(id2(a)) -> #{}.
maps_without_2(A) ->
    maps:without([A], #{a => ka}).

-spec maps_without_3() -> #{a := ka}.
maps_without_3() ->
    maps:without([z], #{a => ka}).

-spec maps_without_4(atom())
        -> #{b => atom()}.
maps_without_4(Atom) ->
    maps:without(
        [a],
        #{a => ka, b => Atom}
    ).

-spec maps_without_5_neg(
    atom(), d | e
) -> #{b => atom()}.
maps_without_5_neg(Atom, DOrE) ->
    maps:without(
        [a, c, DOrE],
        #{
            a => ka,
            b => Atom,
            c => self(),
            d => kd
        }
    ).

-spec maps_without_6_neg() -> term().
maps_without_6_neg() ->
    maps:without(non_list, #{}).

-spec maps_without_7_neg() -> term().
maps_without_7_neg() ->
    maps:without([], non_map).

-spec maps_without_8(term()) ->
    #{atom() => ka | pid()}.
maps_without_8(Any) ->
    maps:without([Any],
        #{a => ka, b => self()}
    ).

-spec maps_without_9(
    #{atom() => number()}
) -> #{atom() => number()}.
maps_without_9(D) ->
    maps:without([a], D).

-spec maps_without_10(atom()) ->
    #{atom() => y | z}.
maps_without_10(Atom) ->
    maps:without(
        [self(), w, Atom],
        #{a => y, b => z}
    ).

-spec maps_without_11
    (eqwalizer:dynamic()) ->
    #{atom() => y | z}.
maps_without_11(Dyn) ->
    maps:without(
        [Dyn],
        #{a => y, b => z}
    ).

-spec maps_without_12_neg
    (none()) ->
    wrong_ret.
maps_without_12_neg(None) ->
    maps:without(
        [a, b],
        None
    ).

-spec maps_without_13(
    infinikey(a)
) -> #{b := pid()}.
maps_without_13(Keys) ->
    maps:without(
        Keys,
        #{a => self(), b => self()}
    ).

-spec maps_without_14_neg()
        -> #{b := pid()}.
maps_without_14_neg() ->
    maps:without(
        [a | improper_tail],
        #{a => self(), b => self()}
    ).

% Some information is lost when
% the arg is not a list literal
-spec maps_without_15_neg()
    -> #{b := pid()}.
maps_without_15_neg() ->
    Keys = [a, b],
    maps:without(
        Keys,
        #{a => ka, b => self()}
    ).

-spec maps_without_16
    (#{a := atom(), c := atom()}
    | #{b := atom(), c := atom()}
) -> #{c := atom()}.
maps_without_16(M) ->
    maps:without([a, b], M).

-spec maps_without_17_neg
    (#{a := av, c := cv, d := dv}
    | #{b := bv, c := cv, e => ev}
    ) -> #{c := atom()}.
maps_without_17_neg(M) ->
    maps:without([a, b], M).

-spec maps_without_18
    (#{a := av, b := bv})
    -> #{a => av, b => bv}.
maps_without_18(M) ->
    L = [a, c, d],
    maps:without(L, M).

-spec maps_without_19
    (#{a := av, b := bv})
    -> #{a => av, b => bv}.
maps_without_19(M) ->
    maps:without([a, c, d], M).

-spec none() -> none().
none() -> error(none).

-spec maps_without_with_none(boolean()) ->
    #{id => integer()}.
maps_without_with_none(Flag) ->
    M = case Flag of
        true -> maps:without(
            [name],
            #{id => 1, name => "name"}
        );
        false -> maps:without(
            [name],
            none()
        )
    end,
    M.

-spec maps_without_opacity_opaque() ->
    nok.
maps_without_opacity_opaque() ->
    _ = maps:without(
        [sets:new()],
        #{}
    ),
    nok.

-spec custom_overloaded
    (term()) -> term().
custom_overloaded(A) when is_atom(A) ->
    atom_to_binary(A);
custom_overloaded(B) when is_binary(B) ->
    binary_to_atom(B).

-spec use_custom_overloaded1
    (atom()) -> binary().
use_custom_overloaded1(A) ->
    custom_overloaded(A).

-spec use_custom_overloaded2
    (binary()) -> atom().
use_custom_overloaded2(B) ->
    custom_overloaded(B).

-spec used_custom_overloaded3_neg
    (term()) -> term().
used_custom_overloaded3_neg(X) ->
    custom_overloaded(X).

-spec maps_find_1(
    pid(), #{pid() => atom()}
) -> atom().
maps_find_1(K, M) ->
    {ok, Val} = maps:find(K, M),
    Val.

-spec maps_find_2(
    #{a => atom(), n => number()}
) -> {atom(), number()}.
maps_find_2(M) ->
    {ok, A} = maps:find(a, M),
    {ok, N} = maps:find(n, M),
    {A, N}.

-spec maps_find_3_neg(
    #{a => atom(), n => number() | pid()}
) -> {atom(), number()}.
maps_find_3_neg(M) ->
    {ok, A} = maps:find(a, M),
    {ok, N} = maps:find(n, M),
    {A, N}.

-spec maps_find_4(
    term(), #{a => atom(), n => number()}
) -> atom() | number().
maps_find_4(K, M) ->
    {ok, A} = maps:find(K, M),
    A.

-spec maps_with_1(
    Ks :: [atom()],
    M :: #{atom() => number()}
) -> #{atom() => number()}.
maps_with_1(Ks, M) ->
    maps:with(Ks, M).

-spec maps_with_2(#{
    a := atom,
    n => number()
}) -> #{a := atom()}.
maps_with_2(M) ->
    maps:with([a], M).

-spec maps_with_3(#{
    a := atom,
    n => number()
}) -> #{n => number()}.
maps_with_3(M) ->
    maps:with([n], M).

-spec maps_with_4
    (#{a := av, b := bv})
    -> #{a => av, b => bv}.
maps_with_4(M) ->
    L = [a, c, d],
    maps:with(L, M).

-spec maps_with_5
    (#{a := av, b := bv})
    -> #{a => av, b => bv}.
maps_with_5(M) ->
    maps:with([a, c, d], M).

-spec lists_flatten_nil_1(
    [[atom()]], [atom()]
) -> [atom()].
lists_flatten_nil_1(
    L1, L2
) ->
    lists:flatten([L1, L2, []]).

-spec lists_flatten_nil_2(
    atom(), atom(), boolean()
) -> [atom()].
lists_flatten_nil_2(
    A1, A2, Flag
) ->
    lists:flatten([
        a,
        b,
        case Flag of
            true -> A1;
            false -> []
        end,
        case Flag of
            true -> A2;
            false -> []
        end
    ]).


-spec list_flatten_nil_3() -> [].
list_flatten_nil_3() ->
    lists:flatten([[], [], []]).

-spec filename_join_1_1_wip() ->
    file:filename().
filename_join_1_1_wip() ->
    filename:join(["server", "erl"]).

-spec filename_join_1_1_neg() ->
    file:filename().
filename_join_1_1_neg() ->
    filename:join(["server", <<>>]).

-spec filename_join_1_2_neg() ->
    file:filename().
filename_join_1_2_neg() ->
    filename:join([<<>>, ""]).

-spec filename_join_1_3() ->
    file:filename_all().
filename_join_1_3() ->
    filename:join([<<>>, ""]).

-spec filename_join_1_4() ->
    file:filename_all().
filename_join_1_4() ->
    filename:join(["", <<>>]).

-spec filename_join_1_5() ->
    binary().
filename_join_1_5() ->
    filename:join([<<>>, <<>>]).

-spec filename_join_1_6() ->
    file:filename_all().
filename_join_1_6() ->
    filename:join([atom, <<>>]).

-spec filename_join_1_7() ->
    file:filename_all().
filename_join_1_7() ->
    filename:join([<<>>, atom]).

-spec filename_join_1_8_neg() ->
    file:filename_all().
filename_join_1_8_neg() ->
    filename:join([<<>>, self()]).

-spec filename_join_2_1_wip() ->
    file:filename().
filename_join_2_1_wip() ->
    filename:join("server", "erl").

-spec filename_join_2_1_neg() ->
    file:filename().
filename_join_2_1_neg() ->
    filename:join("server", <<>>).

-spec filename_join_2_2_neg() ->
    file:filename().
filename_join_2_2_neg() ->
    filename:join(<<>>, "").

-spec filename_join_2_3() ->
    file:filename_all().
filename_join_2_3() ->
    filename:join(<<>>, "").

-spec filename_join_2_4() ->
    file:filename_all().
filename_join_2_4() ->
    filename:join("", <<>>).

-spec filename_join_2_6_neg() ->
    binary().
filename_join_2_6_neg() ->
    filename:join(atom, <<>>).

-spec filename_join_2_7() ->
    file:filename_all().
filename_join_2_7() ->
    filename:join(<<>>, atom).

-spec filename_join_2_8_neg() ->
    file:filename_all().
filename_join_2_8_neg() ->
    filename:join(<<>>, self()).

-spec my_filter1(atom() | char())
        -> [char()].
my_filter1(A) when is_atom(A) ->
    atom_to_list(A);
my_filter1(C) when is_number(C) ->
    [C].

-spec my_filter2(atom() | char())
        -> [atom() | char()].
my_filter2(A) when is_atom(A) ->
    atom_to_list(A);
my_filter2(C) when is_number(C) ->
    [C].

-spec queue_filter_20(
    queue:queue(atom() | char())
) -> queue:queue(atom() | char()).
queue_filter_20(Q) ->
    queue:filter(
        fun my_filter1/1,
        Q
    ).

-spec queue_filter_21_neg(
    queue:queue(atom() | char())
) -> queue:queue(char()).
queue_filter_21_neg(Q) ->
    queue:filter(
        fun my_filter1/1,
        Q
    ).

-spec queue_filter_22(
    queue:queue(atom() | char())
) -> queue:queue(atom() | char()).
queue_filter_22(Q) ->
    queue:filter(
        fun my_filter2/1,
        Q
    ).

-type state1() :: #{
    module := module(),
    count := number()
}.

-type state2() :: #{
    module => module(),
    count => number()
}.

-spec maps_put1() -> state1().
maps_put1() ->
    M1 = #{},
    M2 = maps:put(module, foo, M1),
    M3 = maps:put(count, 0, M2),
    M3.

-spec maps_put2(boolean()) -> state1().
maps_put2(B) ->
    M1 = #{},
    M2 = maps:put(module, foo, M1),
    M3 =
        case B of
            true ->
                maps:put(count, 0, M2);
            false -> M2
        end,
    M3.

-spec maps_put3(boolean()) -> state2().
maps_put3(B) ->
    M1 = #{},
    M2 = maps:put(module, foo, M1),
    M3 =
        case B of
            true ->
                maps:put(count, 0, M2);
            false -> M1
        end,
    M3.

-type a_n_map() :: #{atom() => number()}.

-spec sum_numbers1(a_n_map())
        -> number().
sum_numbers1(M) ->
    maps:fold(
        fun (_Atom, Num, Sum) ->
            Num + Sum
        end,
        0,
        M
    ).

-spec sum_numbers2_neg(a_n_map())
        -> number().
sum_numbers2_neg(M) ->
    maps:fold(
        fun (Atom, _Num, Sum) ->
            Atom + Sum
        end,
        0,
        M
    ).

-spec filter_rows
  ({Rows, FilterTs}) -> [User]
  when
  Rows :: [
    {User, undefined, integer()} |
    User
  ],
  FilterTs :: integer() | undefined,
  User :: binary().
filter_rows({Rows, FTS}) ->
  lists:filtermap(fun
    ({_, _, Ts})
        when FTS =/= undefined,
             FTS > Ts ->
      false;
    ({User, _, _}) ->
      {true, User};
    (User) ->
      {true, User}
    end, Rows).

-spec file_open1() -> pid().
file_open1() ->
    {ok, Device} =
        file:open("/file", [read]),
    Device.

-spec file_open2() -> file:fd().
file_open2() ->
    {ok, Device} =
        file:open("/file", [read, raw]),
    Device.

-spec file_open3_neg
    ([file:mode()]) -> file:fd().
file_open3_neg(Modes) ->
    {ok, Device} =
        file:open("/file", Modes),
    Device.

-spec file_open5_neg
    (file:mode()) -> file:fd().
file_open5_neg(Mode) ->
    {ok, Device} =
        file:open("/file", [Mode]),
    Device.

-spec maps_remove1(
    #{a := integer()}
) -> #{}.
maps_remove1(M) ->
    maps:remove(a, M).

-spec maps_remove2(
    #{a := integer()}
) -> #{a := integer()}.
maps_remove2(M) ->
    maps:remove(b, M).

-spec maps_remove3_neg(
    atom(),
    #{a := integer()}
) -> #{a := integer()}.
maps_remove3_neg(A, M) ->
    maps:remove(A, M).

-spec maps_remove4(
    #{a := integer(), b := atom()} |
    #{a := atom(), b := pid()}
) -> #{b := atom()} | #{b => pid()}.
maps_remove4(M) ->
    maps:remove(a, M).

-spec maps_remove5(
    #{atom() => binary()}
) -> #{atom() => binary()}.
maps_remove5(M) ->
    maps:remove(a, M).

-spec maps_filtermap_1(
    #{atom() => atom()}
) -> #{atom() => binary()}.
maps_filtermap_1(M) ->
    maps:filtermap(
        fun (_, V) ->
            {true, atom_to_binary(V)}
        end,
        M
    ).

-spec maps_filtermap_2(
    #{a := atom(), b => atom()}
) -> #{a => binary(), b => binary()}.
maps_filtermap_2(M) ->
    maps:filtermap(
        fun (_, V) ->
            {true, atom_to_binary(V)}
        end,
        M
    ).

-spec maps_filtermap_3_neg(
    #{a := atom(), b := atom(), c := atom()}
) -> #{a := atom(), b := binary()}.
maps_filtermap_3_neg(M) ->
    maps:filtermap(
        fun
            (a, V) -> true;
            (b, V) -> {true, atom_to_binary(V)};
            (c, _) -> false
        end,
        M
    ).

-spec maps_filtermap_4(
    #{atom() => atom()}
) -> #{atom() => none()}.
maps_filtermap_4(M) ->
    maps:filtermap(
        fun (_, V) -> false end,
        M
    ).

-spec maps_filtermap_5_neg(
    #{atom() => atom()}
) -> #{atom() => atom()}.
maps_filtermap_5_neg(M) ->
    maps:filtermap(
        fun (_, V) ->
            {true, atom_to_binary(V)}
        end,
        M
    ).

-spec maps_filtermap_6_neg(
    #{atom() => atom()}
) -> #{atom() => binary()}.
maps_filtermap_6_neg(M) ->
    maps:filtermap(
        fun (_, _) -> err end,
        M
    ).

-spec maps_filtermap_7_neg(
    #{atom() => binary()}
) -> #{atom() => atom()}.
maps_filtermap_7_neg(M) ->
    maps:filtermap(
        fun (_, V) -> {true, atom_to_binary(V)} end,
        M
    ).

-spec re_replace_1(iodata()) -> string().
re_replace_1(Subj) ->
    re:replace(Subj, "+", "-", [{return, list}]).

-spec re_replace_2(iodata()) -> binary().
re_replace_2(Subj) ->
    re:replace(Subj, "+", "-", [{return, binary}]).

-spec re_replace_3_neg(iodata()) -> string().
re_replace_3_neg(Subj) ->
    re:replace(Subj, "+", "-", [{return, binary}]).

-spec re_replace_4_neg(iodata()) -> binary().
re_replace_4_neg(Subj) ->
    re:replace(Subj, "+", "-", [{return, list}]).

-spec re_replace_5_neg(atom()) -> binary().
re_replace_5_neg(Subj) ->
    Res = re:replace(Subj, "+", "-", [{return, list}]),
    Res.

-spec re_replace_6_neg(iodata()) -> term().
re_replace_6_neg(Subj) ->
    Res = re:replace(Subj, "+", "-", [{return, something}]),
    Res.
