%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(eqwater_records).

-compile([export_all, nowarn_export_all]).

-record(rec1, {
  id :: integer(),
  name :: string()
}).
-type version() :: non_neg_integer().
-type version3() ::
  {version(), version(), version()}.
-type version3_int() ::
  {integer(), integer(), integer()}.

-type user() :: #rec1{} | integer().

-spec id1(user()) -> integer().
id1(R) when is_record(R, rec1) ->
  R#rec1.id;
id1(Id) -> Id.

-spec id2(user()) -> integer().
id2(#rec1{id = Id}) -> Id;
id2(Id) -> Id.

-spec id3_neg(user()) -> integer().
id3_neg(#rec1{id = Id, name = Name})
  when length(Name) == 0 ->
  Id;
id3_neg(Id) -> Id.

-spec id4_neg(user()) -> integer().
id4_neg(#rec1{id = Id, name = ""}) ->
  Id;
id4_neg(Id) -> Id.

-spec id5_neg(string(), user())
      -> integer().
id5_neg(Name, User) ->
  case User of
    #rec1{id = Id, name = Name} -> Id;
    Id -> Id
  end.

-spec id6(#rec1{} | version3())
      -> integer().
id6(#rec1{id = Id}) -> Id;
id6({X, _Y, _Z}) ->
  X.

-spec id7(#rec1{} | version3_int())
      -> integer().
id7(#rec1{id = Id}) -> Id;
id7({X, _Y, _Z}) ->
  X.

-spec get_rec1_id(
    #rec1{}
) -> integer().
get_rec1_id(#rec1{id = Id}) -> Id.

-spec get_id_1(
    #rec1{} | integer()
) -> integer().
get_id_1(Rec) when is_record(Rec, rec1) ->
  get_rec1_id(Rec);
get_id_1(Id) -> Id.

-spec get_id_2(
    #rec1{} | integer()
) -> integer().
get_id_2(Rec = #rec1{})  ->
  get_rec1_id(Rec);
get_id_2(Id) -> Id.

-spec get_id_3(
    #rec1{} | integer()
) -> integer().
get_id_3(#rec1{} = Rec)  ->
  get_rec1_id(Rec);
get_id_3(Id) -> Id.

-record(rec2, {
  name :: string(),
  id :: integer()
}).

-spec normalize(
    {#rec1{}, #rec2{}} |
    {#rec2{}, #rec1{}}
) -> {#rec1{}, #rec2{}}.
normalize({R2 = #rec2{}, R1}) -> {R1, R2};
normalize(Pair) -> Pair.

-spec normalize_neg1(
    {#rec1{}, #rec2{}} |
    {#rec2{}, #rec1{}}
) -> {#rec1{}, #rec2{}}.
normalize_neg1({R = #rec2{}, R}) ->
  {R, R};
normalize_neg1(Pair) -> Pair.

-spec normalize_neg2(
    {#rec1{}, #rec2{}} |
    {#rec2{}, #rec1{}}
) -> {#rec1{}, #rec2{}}.
normalize_neg2(
    {R2 = #rec2{id = I},
     R1 = #rec1{id = I}}) -> {R1, R2};
normalize_neg2(Pair) -> Pair.
