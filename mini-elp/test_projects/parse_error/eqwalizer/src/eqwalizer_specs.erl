%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(eqwalizer_specs).
-typing([eqwalizer]).
-compile([export_all, nowarn_export_all]).

%% -------- erlang --------

-spec 'erlang:abs'(number()) -> number().
'erlang:abs'(_) -> error(eqwalizer_specs).

-spec 'erlang:max'(A, B) -> A | B.
'erlang:max'(_, _) -> error(eqwalizer_specs).

-spec 'erlang:min'(A, B) -> A | B.
'erlang:min'(_, _) -> error(eqwalizer_specs).

-spec 'erlang:system_time'() -> pos_integer().
'erlang:system_time'() -> error(eqwalizer_specs).

-spec 'erlang:system_time'(erlang:time_unit()) -> pos_integer().
'erlang:system_time'(_) -> error(eqwalizer_specs).

%% -------- gb_sets --------

-spec 'gb_sets:empty'() -> gb_sets:set(none()).
'gb_sets:empty'() -> error(eqwalizer_specs).

-spec 'gb_sets:new'() -> gb_sets:set(none()).
'gb_sets:new'() -> error(eqwalizer_specs).

%% -------- lists --------

-spec 'lists:all'(fun((T) -> boolean()), [T]) -> boolean().
'lists:all'(_, _) -> error(eqwalizer_specs).

-spec 'lists:any'(fun((T) -> boolean()), [T]) -> boolean().
'lists:any'(_, _) -> error(eqwalizer_specs).

-spec 'lists:append'([[T]]) -> [T].
'lists:append'(_) -> error(eqwalizer_specs).

-spec 'lists:append'([T], [T]) -> [T].
'lists:append'(_, _) -> error(eqwalizer_specs).

-spec 'lists:delete'(T, [T]) -> [T].
'lists:delete'(_, _) -> error(eqwalizer_specs).

-spec 'lists:droplast'([T]) -> [T].
'lists:droplast'(_) -> error(eqwalizer_specs).

-spec 'lists:dropwhile'(fun((T) -> boolean()), [T]) -> [T].
'lists:dropwhile'(_, _) -> error(eqwalizer_specs).

-spec 'lists:duplicate'(non_neg_integer(), T) -> [T].
'lists:duplicate'(_, _) -> error(eqwalizer_specs).

-spec 'lists:filter'(fun((T) -> boolean()), [T]) -> [T].
'lists:filter'(_, _) -> error(eqwalizer_specs).

-spec 'lists:filtermap'(fun((T) -> boolean() | {'true', X}), [T]) -> [(T | X)].
'lists:filtermap'(_, _) -> error(eqwalizer_specs).

-spec 'lists:flatmap'(fun((A) -> [B]), [A]) -> [B].
'lists:flatmap'(_, _) -> error(eqwalizer_specs).

-spec 'lists:flatlength'([term()]) -> non_neg_integer().
'lists:flatlength'(_) -> error(eqwalizer_specs).

-spec 'lists:foldl'(fun((T, Acc) -> Acc), Acc, [T]) -> Acc.
'lists:foldl'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:foldr'(fun((T, Acc) -> Acc), Acc, [T]) -> Acc.
'lists:foldr'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:foreach'(fun((T) -> term()), [T]) -> ok.
'lists:foreach'(_, _) -> error(eqwalizer_specs).

-spec 'lists:join'(T, [T]) -> [T].
'lists:join'(_, _) -> error(eqwalizer_specs).

-spec 'lists:keydelete'(Key :: term(), N :: pos_integer(), [Tuple]) -> [Tuple].
'lists:keydelete'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:keyfind'(Key :: term(), N :: pos_integer(), [Tuple]) -> Tuple | false.
'lists:keyfind'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:keyreplace'(Key :: term(), N :: pos_integer(), [Tuple], Tuple) -> [Tuple].
'lists:keyreplace'(_, _, _, _) -> error(eqwalizer_specs).

-spec 'lists:keysearch'(Key :: term(), N :: pos_integer(), [Tuple]) -> {value, Tuple} | false.
'lists:keysearch'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:keytake'(Key :: term(), N :: pos_integer(), [Tuple]) -> {value, Tuple, [Tuple]} | false.
'lists:keytake'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:last'([T]) -> T.
'lists:last'(_) -> error(eqwalizer_specs).

-spec 'lists:map'(fun((A) -> B), [A]) -> [B].
'lists:map'(_, _) -> error(eqwalizer_specs).

-spec 'lists:mapfoldl'(fun((A, Acc) -> {B, Acc}), Acc, [A]) -> {[B], Acc}.
'lists:mapfoldl'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:mapfoldr'(fun((A, Acc) -> {B, Acc}), Acc, [A]) -> {[B], Acc}.
'lists:mapfoldr'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:max'([T]) -> T.
'lists:max'(_) -> error(eqwalizer_specs).

-spec 'lists:member'(T, [T]) -> boolean().
'lists:member'(_, _) -> error(eqwalizer_specs).

-spec 'lists:merge'([[T]]) -> [T].
'lists:merge'(_) -> error(eqwalizer_specs).

-spec 'lists:merge'([X], [Y]) -> [X | Y].
'lists:merge'(_, _) -> error(eqwalizer_specs).

-spec 'lists:merge'(fun((A, B) -> boolean()), [A], [B]) -> [A | B].
'lists:merge'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:merge3'([X], [Y], [Z]) -> [X | Y | Z].
'lists:merge3'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:min'([T]) -> T.
'lists:min'(_) -> error(eqwalizer_specs).

-spec 'lists:nth'(pos_integer(), [T]) -> T.
'lists:nth'(_, _) -> error(eqwalizer_specs).

-spec 'lists:nthtail'(pos_integer(), [T]) -> [T].
'lists:nthtail'(_, _) -> error(eqwalizer_specs).

-spec 'lists:partition'(fun((T) -> boolean()), [T]) -> {[T], [T]}.
'lists:partition'(_, _) -> error(eqwalizer_specs).

-spec 'lists:prefix'([T], [T]) -> boolean().
'lists:prefix'(_, _) -> error(eqwalizer_specs).

-spec 'lists:reverse'([T]) -> [T].
'lists:reverse'(_) -> error(eqwalizer_specs).

-spec 'lists:reverse'([T], [T]) -> [T].
'lists:reverse'(_, _) -> error(eqwalizer_specs).

-spec 'lists:rmerge'([X], [Y]) -> [X | Y].
'lists:rmerge'(_, _) -> error(eqwalizer_specs).

-spec 'lists:rmerge3'([X], [Y], [Z]) -> [X | Y | Z].
'lists:rmerge3'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:rumerge'([X], [Y]) -> [X | Y].
'lists:rumerge'(_, _) -> error(eqwalizer_specs).

-spec 'lists:rumerge'(fun((X, Y) -> boolean()), [X], [Y]) -> [(X | Y)].
'lists:rumerge'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:rumerge3'([X], [Y], [Z]) -> [X | Y | Z].
'lists:rumerge3'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:search'(fun((T) -> boolean()), [T]) -> {value, T} | false.
'lists:search'(_, _) -> error(eqwalizer_specs).

-spec 'lists:sort'([T]) -> [T].
'lists:sort'(_) -> error(eqwalizer_specs).

-spec 'lists:sort'(fun((T, T) -> boolean()), [T]) -> [T].
'lists:sort'(_, _) -> error(eqwalizer_specs).

-spec 'lists:split'(non_neg_integer(), [T]) -> {[T], [T]}.
'lists:split'(_, _) -> error(eqwalizer_specs).

-spec 'lists:splitwith'(fun((T) -> boolean()), [T]) -> {[T], [T]}.
'lists:splitwith'(_, _) -> error(eqwalizer_specs).

-spec 'lists:sublist'([T], Len :: non_neg_integer()) -> [T].
'lists:sublist'(_, _) -> error(eqwalizer_specs).

-spec 'lists:sublist'([T], Start :: pos_integer(), Len :: non_neg_integer()) -> [T].
'lists:sublist'(_, _,  _) -> error(eqwalizer_specs).

-spec 'lists:subtract'([T], [T]) -> [T].
'lists:subtract'(_, _) -> error(eqwalizer_specs).

-spec 'lists:suffix'([T], [T]) -> boolean().
'lists:suffix'(_, _) -> error(eqwalizer_specs).

-spec 'lists:takewhile'(fun((T) -> boolean()), [T]) -> [T].
'lists:takewhile'(_, _) -> error(eqwalizer_specs).

-spec 'lists:umerge'([[T]]) -> [T].
'lists:umerge'(_) -> error(eqwalizer_specs).

-spec 'lists:umerge'([A], [B]) -> [A | B].
'lists:umerge'(_, _) -> error(eqwalizer_specs).

-spec 'lists:umerge'(fun((A, B) -> boolean()), [A], [B]) -> [A | B].
'lists:umerge'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:umerge3'([A], [B], [C]) -> [A | B | C].
'lists:umerge3'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:unzip'([{A, B}]) -> {[A], [B]}.
'lists:unzip'(_) -> error(eqwalizer_specs).

-spec 'lists:unzip3'([{A, B, C}]) -> {[A], [B], [C]}.
'lists:unzip3'(_) -> error(eqwalizer_specs).

-spec 'lists:usort'([T]) -> [T].
'lists:usort'(_) -> error(eqwalizer_specs).

-spec 'lists:usort'(fun((T, T) -> boolean()), [T]) -> [T].
'lists:usort'(_, _) -> error(eqwalizer_specs).

-spec 'lists:zf'(fun((T) -> boolean() | {'true', X}), [T]) -> [(T | X)].
'lists:zf'(_, _) -> error(eqwalizer_specs).

-spec 'lists:zip'([A], [B]) -> [{A, B}].
'lists:zip'(_, _) -> error(eqwalizer_specs).

-spec 'lists:zip3'([A], [B], [C]) -> [{A, B, C}].
'lists:zip3'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:zipwith'(fun((X, Y) -> T), [X], [Y]) -> [T].
'lists:zipwith'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:zipwith3'(fun((X, Y, Z) -> T), [X], [Y], [Z]) -> [T].
'lists:zipwith3'(_, _, _, _) -> error(eqwalizer_specs).

%% -------- maps --------

-spec 'maps:find'(Key, #{Key => Value}) -> {ok, Value} | error.
'maps:find'(_, _) -> error(eqwalizer_specs).

-spec 'maps:from_list'([{Key, Value}]) -> #{Key => Value}.
'maps:from_list'(_) -> error(eqwalizer_specs).

-spec 'maps:merge'(#{Key => Value}, #{Key => Value}) -> #{Key => Value}.
'maps:merge'(_, _) -> error(eqwalizer_specs).

-spec 'maps:put'(Key, Value, #{Key => Value}) -> #{Key => Value}.
'maps:put'(_, _, _) -> error(eqwalizer_specs).

-spec 'maps:remove'(Key, #{Key => Value}) -> #{Key => Value}.
'maps:remove'(_, _) -> error(eqwalizer_specs).

-spec 'maps:update_with'(Key, fun((Value1) -> Value2), #{Key => Value1}) -> #{Key => Value1 | Value2}.
'maps:update_with'(_, _, _) -> error(eqwalizer_specs).

-spec 'maps:update_with'(Key, fun((Value1) -> Value2), Value2, #{Key => Value1}) -> #{Key => Value1 | Value2}.
'maps:update_with'(_, _, _, _) -> error(eqwalizer_specs).

%% -------- proplists --------

-spec 'proplists:delete'(term(), [A]) -> [A].
'proplists:delete'(_, _) -> error(eqwalizer_specs).

-spec 'proplists:from_map'(#{K => V}) -> [{K, V}].
'proplists:from_map'(_) -> error(eqwalizer_specs).

%% -------- timer --------

-spec 'timer:tc'(fun(() -> T)) -> {integer(), T}.
'timer:tc'(_) -> error(eqwalizer_specs).
