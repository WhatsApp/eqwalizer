%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(map).

-eqwalizer_unchecked([]).

-export_type([map_/2]).

-export([empty/0,
         get/2,
         member/2,
         size/1,
         is_empty/1,
         insert/3,
         remove/2,
         update/3,
         singleton/2]).

-export([union/2, intersect/2, diff/2]).

-export([fold/3, map/2, filter/2, partition/2]).

-export([keys/1, values/1, to_list/1, from_list/1]).

-import_type({'maybe', [{'maybe', 1}]}).

-opaque map_(K, V) :: map_ffi:map_(K, V).

-spec empty() -> map_(_K, _V).

empty() -> map_ffi:empty().

-spec get(K, map_(K, V)) -> 'maybe':'maybe'(V).

get(Key, Map) -> map_ffi:get(Key, Map).

-spec member(K, map_(K, _V)) -> boolean().

member(Key, Map) ->
    case get(Key, Map) of
        {'$#maybe:maybe.just', _} -> true;
        {'$#maybe:maybe.nothing'} -> false
    end.

-spec size(map_(_K, _V)) -> integer().

size(Map) -> map_ffi:size(Map).

-spec is_empty(map_(_K, _V)) -> boolean().

is_empty(Map) -> map:size(Map) =:= 0.

-spec insert(K, V, map_(K, V)) -> map_(K, V).

insert(Key, Value, Map) ->
    map_ffi:insert(Key, Value, Map).

-spec remove(K, map_(K, V)) -> map_(K, V).

remove(Key, Map) -> map_ffi:remove(Key, Map).

-spec update(K, fun(('maybe':'maybe'(V)) -> 'maybe':'maybe'(V)),
             map_(K, V)) -> map_(K, V).

update(Key, Alter, Map) ->
    case Alter(get(Key, Map)) of
        {'$#maybe:maybe.just', Value} ->
            insert(Key, Value, Map);
        {'$#maybe:maybe.nothing'} -> remove(Key, Map)
    end.

-spec singleton(K, V) -> map_(K, V).

singleton(Key, Value) -> insert(Key, Value, empty()).

-spec union(map_(K, V), map_(K, V)) -> map_(K, V).

union(Map1, Map2) -> map_ffi:union(Map1, Map2).

-spec intersect(map_(K, V), map_(K, V)) -> map_(K, V).

intersect(Map1, Map2) ->
    filter(fun (K, _) -> member(K, Map2) end, Map1).

-spec diff(map_(K, V), map_(K, V)) -> map_(K, V).

diff(Map1, Map2) ->
    filter(fun (K, _) -> not member(K, Map2) end, Map1).

-spec fold(fun((K, V, R) -> R), R, map_(K, V)) -> R.

fold(F, Acc, Map) -> map_ffi:fold(F, Acc, Map).

-spec map(fun((K, A) -> B), map_(K, A)) -> map_(K, B).

map(F, Map) -> map_ffi:map(F, Map).

-spec filter(fun((K, V) -> boolean()),
             map_(K, V)) -> map_(K, V).

filter(F, Map) -> map_ffi:filter(F, Map).

-spec partition(fun((K, V) -> boolean()),
                map_(K, V)) -> {map_(K, V), map_(K, V)}.

partition(F, Map) ->
  fold(
    fun (K, V, {D1, D2}) ->
      case F(K, V) of
        true -> {insert(K, V, D1), D2};
        false -> {D1, insert(K, V, D2)}
      end
    end,
    {empty(), empty()},
    Map
  ).

-spec keys(map_(K, _V)) -> [K].

keys(Map) -> map_ffi:keys(Map).

-spec values(map_(_K, V)) -> [V].

values(Map) -> map_ffi:values(Map).

-spec to_list(map_(K, V)) -> [{K, V}].

to_list(Map) -> map_ffi:to_list(Map).

-spec from_list([{K, V}]) -> map_(K, V).

from_list(List) -> map_ffi:from_list(List).



