%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(unspecced).

-compile([export_all, nowarn_export_all]).

-spec expect_none(none()) -> none().
expect_none(None) -> None.

as_map1(#{id := Id}) ->
  expect_none({Id}).

as_map2(M) ->
  Res = maps:map(
    fun(_K, V) -> {V} end, M
  ),
  expect_none(Res).

as_map3(M) ->
  Id = maps:get(id, M),
  expect_none({Id}).

as_map4(M) ->
  Res = M#{id => 1},
  expect_none({Res}).

as_list([H | T]) ->
  Res = {H, T},
  expect_none(Res).

as_tuple({E1, _}) ->
  Res = {E1},
  expect_none(Res).

-record(rec, {id :: number()}).

get_id_neg1(Rec) ->
  atom_to_binary(Rec#rec.id).

get_id_neg2(#rec{id = Id}) ->
  atom_to_binary(Id).
