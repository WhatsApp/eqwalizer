%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(gradual_complex_types).

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

-spec map_update_opt
    (eqwalizer:dynamic())
      -> eqwalizer:dynamic().
map_update_opt(M) ->
  M1 = M#{a => 1},
  eqwalizer:reveal_type(M1),
  M1.

-spec map_update_req
    (eqwalizer:dynamic())
      -> eqwalizer:dynamic().
map_update_req(M) ->
  M1 = M#{a := 1},
  eqwalizer:reveal_type(M1),
  M1.

-type dyn_map() :: #{
  eqwalizer:dynamic() =>
    eqwalizer:dynamic()
}.

-spec dyn_map_as_shape
    (dyn_map())
      -> #{a => atom(), b => binary()}.
dyn_map_as_shape(DM) ->
  DM.

-spec consume_shape
    (#{a := atom(), b := binary()})
      -> {atom(), binary()}.
consume_shape(#{a := A, b := B})  ->
  {A, B}.

use_consume_1
(#{a := ok} = Shape) ->
  consume_shape(Shape);
use_consume_1
(#{a := err}) ->
  error(err).

use_consume_2
(Shape = #{a := ok}) ->
  consume_shape(Shape);
use_consume_2
(#{a := err}) ->
  error(err).

use_consume_3
(Shape = #{extra := ok}) ->
  consume_shape(Shape);
use_consume_3
(#{extra := err}) ->
  error(err).

-spec generic(A, A) -> A.
generic(_, _) -> error(stub).

-spec use_generic_shape
    (dyn_map(), #{a => atom()}) -> ok.
use_generic_shape(DM, S) ->
  generic(DM, S).

-spec tuple_mismatch(dyn_map()) ->
  {#{a => atom}, integer()}.
tuple_mismatch(DM) ->
  {DM, ok}.
