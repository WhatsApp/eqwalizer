%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(strict_complex_types).

-compile([export_all, nowarn_export_all]).

-type complex_map() :: #{
  id := integer(),
  {secret, id} => integer(),
  atom() => term()
}.

-spec mk_complex_map()
      -> complex_map().
mk_complex_map() ->
  #{id => 1}.

-spec mk_complex_map_neg()
      -> complex_map().
mk_complex_map_neg() ->
  undefined.

-spec use_complex_map1(complex_map())
      -> complex_map().
use_complex_map1(Map) ->
  eqwalizer:reveal_type(Map),
  Map.

-spec use_complex_map2([complex_map()])
      -> [complex_map()].
use_complex_map2(List) ->
  eqwalizer:reveal_type(List),
  List.

-spec use_complex_map3()
      -> integer().
use_complex_map3() ->
  Map = mk_complex_map(),
  maps:get(id, Map).

-spec use_complex_map4_neg()
      -> integer().
use_complex_map4_neg() ->
  Map = mk_complex_map(),
  lists:nth(7, Map).
