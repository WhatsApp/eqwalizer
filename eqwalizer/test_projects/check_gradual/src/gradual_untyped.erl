%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(gradual_untyped).
-compile([export_all, nowarn_export_all]).

-spec reveal_any_list(list()) -> ok.
reveal_any_list(_L) ->
  eqwalizer:reveal_type(_L),
  ok.

-record(untyped, {
  field1 :: atom(),
  field2
}).

-spec inspect_untyped(#untyped{})
      -> atom().
inspect_untyped(U) ->
  Field2 = U#untyped.field2,
  eqwalizer:reveal_type(Field2),
  Field2.

-spec reveal_any_tuple_1(tuple()) -> ok.
reveal_any_tuple_1({A}) ->
  eqwalizer:reveal_type(A),
  A.

-spec reveal_any_tuple_2(tuple()) -> ok.
reveal_any_tuple_2({_A, B}) ->
  eqwalizer:reveal_type(B),
  B.

-spec tuple1(tuple()) ->
  {integer()}.
tuple1(T) ->
  T.

-spec tuple2(tuple()) ->
  {integer(), integer()}.
tuple2(T) ->
  T.

-spec tuple_record(tuple()) ->
  #untyped{}.
tuple_record(T) ->
  T.

parse_my_field(_X, _Y, _Z) ->
  erlang:error(stub).

-spec parse_union(
    binary(),
    ParseField,
    EmptyValue
) -> {Value | EmptyValue, binary()}
    | no_return()
  when
  ParseField :: fun(
    (binary(),
    integer(),
    non_neg_integer()) ->
      skip | {value, Value, binary()}
  ).
parse_union(_, _, _) ->
  erlang:error(stub).

-spec value_parse_atom
    (binary()) -> {atom(), binary()}.
value_parse_atom(X) ->
  parse_union(
    X,
    fun parse_my_field/3,
    'undefined'
  ).

-spec dyns(
eqwalizer:dynamic() | eqwalizer:dynamic(),
eqwalizer:dynamic() | eqwalizer:dynamic(),
eqwalizer:dynamic() | eqwalizer:dynamic()
    ) -> eqwalizer:dynamic().
dyns(D, D, D) -> D.

-spec value_parse_atom2
    (binary()) -> {atom(), binary()}.
value_parse_atom2(X) ->
  parse_union(
    X,
    fun dyns/3,
    'undefined'
  ).

