%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(dynamic_refine).

-compile([export_all, nowarn_export_all]).

dyn_val() -> erlang:error(dynamic).

-spec occ01() -> b.
occ01() ->
  Arg = dyn_val(),
  case Arg of
    a -> b;
    B -> B
  end.

-spec occ01_elab() -> b.
occ01_elab() ->
  Dyn = dyn_val(),
  Res = case Dyn of
          a -> b;
          B -> B
        end,
  Res.

-spec occ02() -> integer().
occ02() ->
  Dyn = dyn_val(),
  case Dyn of
    a -> 0;
    {_, I} -> I
  end.

occ02_cl(a) -> 0;
occ02_cl({_, I}) -> I.

-spec occ03() -> binary().
occ03() ->
  Dyn = dyn_val(),
  case Dyn of
    {a, A} -> atom_to_binary(A);
    {_, B} -> B
  end.

occ03_cl({a, A}) -> atom_to_binary(A);
occ03_cl({_, B}) -> B.

-spec occ04() -> binary().
occ04() ->
  Dyn = dyn_val(),
  case Dyn of
    A when is_atom(A) ->
      atom_to_binary(A);
    B ->
      B
  end.

occ04_cl(A) when is_atom(A) ->
  atom_to_binary(A);
occ04_cl(B) ->
  B.

-spec occ04_if() -> binary().
occ04_if() ->
  Dyn = dyn_val(),
  if
    is_atom(Dyn) -> atom_to_binary(Dyn);
    true -> Dyn
  end.

-spec occ04_if1() -> binary().
occ04_if1() ->
  Dyn = dyn_val(),
  if
    is_binary(Dyn) -> Dyn;
    true -> atom_to_binary(Dyn)
  end.

-spec refine_any1_neg(any()) -> ok.
refine_any1_neg(Arg) ->
  if
    % Arg is refined as [term()] here
    is_list(Arg) -> {Arg}
  end.

-spec refine_todo1_neg() -> ok.
refine_todo1_neg() ->
  Dyn = dyn_val(),
  if
    % It would be better to have
    % Dyn refined as [dynamic()] here
    is_list(Dyn) -> {Dyn}
  end.

-spec refine_any2_neg(any()) -> ok.
refine_any2_neg(Arg) ->
  if
    % Arg is refined as binary() here
    is_binary(Arg) -> {Arg}
  end.

-spec refine_todo2_neg() -> ok.
refine_todo2_neg() ->
  Dyn = dyn_val(),
  if
    % It would be better to have
    % Dyn refined as binary here
    is_binary(Dyn) -> {Dyn};
    true -> ok
  end.

-spec mixed1
(term(), boolean()) -> ok.
mixed1(Arg, UseOrig) ->
  Mixed =
    if
      UseOrig -> Arg;
      true -> dyn_val()
    end,
  {Mixed}.

-spec mixed2
  (term(), boolean()) -> ok.
mixed2(Arg, UseDyn) ->
  Mixed =
    if
      UseDyn -> dyn_val();
      true -> Arg
    end,
  {Mixed}.

-spec mixed3
  (boolean(), boolean()) -> ok.
mixed3(Arg, UseOrig) ->
  Mixed =
    if
      UseOrig -> Arg;
      true -> dyn_val()
    end,
  {Mixed}.

-spec mixed4
  (binary(), boolean()) -> ok.
mixed4(Arg, UseDyn) ->
  Mixed =
    if
      UseDyn -> dyn_val();
      true -> Arg
    end,
  {Mixed}.

-spec mixed5
  (binary() | atom(), boolean()) -> ok.
mixed5(Arg, Flag) ->
  Mixed1 =
    if
      Flag -> Arg;
      true -> dyn_val()
    end,
  Mixed2 =
    if
      Flag -> Mixed1;
      true -> dyn_val()
    end,
  {Mixed2}.

-spec dyn_union_1(
    [eqwalizer:dynamic()] | error
) -> [number()].
dyn_union_1(U) when is_list(U) ->
  U.

-spec use_private_record_neg() -> ok.
use_private_record_neg() ->
  Rec = records:mk_foo_pos(),
  {foo, _Id, Name} = Rec,
  eqwalizer:reveal_type(Name),
  Name.

refine_map_update1(#{key := _Val} = M) ->
  M#{key := my_value};
refine_map_update1(Other) ->
  Other.

refine_map_update2(
    Val, #{key := _Val} = M
) ->
  M#{key := Val};
refine_map_update2(_, Other) ->
  Other.
  
-spec foo1([integer()]) -> ok.
foo1(_) -> ok.

-spec dyn_refine_list() -> ok.
dyn_refine_list() ->
  Dyn = dyn_val(),
  if
    is_list(Dyn) -> foo1(Dyn)
  end.

-spec dyn_refine_union(
    eqwalizer:dynamic() | error
) -> ok.
dyn_refine_union(error) -> ok;
dyn_refine_union(D) -> D.

-spec dyn_refine_union_2(
    eqwalizer:dynamic() | error | ok
) -> ok.
dyn_refine_union_2(error) -> ok;
dyn_refine_union_2(D) -> D.

-spec dyn_refine_union_neg(
    eqwalizer:dynamic() | error | ok
) -> ok.
dyn_refine_union_neg(ok) -> ok;
dyn_refine_union_neg(D) -> D.

-record(foo, {id :: integer()}).

with_rec_neg(undefined) ->
  undefined;
with_rec_neg(#foo{id = Id}) ->
  atom_to_binary(Id).

-spec with_rec_neg2(
    eqwalizer:dynamic() | #foo{}
) -> atom().
with_rec_neg2(undefined) ->
  undefined;
with_rec_neg2(#foo{id = Id}) ->
  atom_to_binary(Id).

-type dyn_alias() :: eqwalizer:dynamic().
-type union() :: {dyn_alias() | err}.
-type spec_union() :: {integer() | err}.

-spec refine_union_alias(union()) -> ok.
refine_union_alias({err}) -> ok;
refine_union_alias({X}) -> X.

-spec refine_union_alias_neg(union()) -> ok.
refine_union_alias_neg({X}) -> X.

-spec refine_to_tuple(union()) -> {ok}.
refine_to_tuple({err}) -> {ok};
refine_to_tuple(X) -> X.

-spec refine_in_tuple(union()) -> spec_union().
refine_in_tuple(T) -> T.

-spec refine_in_tuple_2(union()) -> {err}.
refine_in_tuple_2(T) -> T.

-spec refine_in_tuple_neg(union()) -> {ok}.
refine_in_tuple_neg(T) -> T.

-spec refine_to_none(dyn_alias()) -> ok.
refine_to_none(X) when is_integer(X) -> X.

-spec occ_tuple_dyn(
    {err, binary()} | {ok, atom()} | dyn_alias()
) -> binary().
occ_tuple_dyn({err, B}) -> B;
occ_tuple_dyn({ok, A}) -> atom_to_binary(A);
occ_tuple_dyn(V) -> V.

-record(rec_err, {a :: err, b :: binary()}).
-spec occ_record_dyn(
    #rec_err{} | dyn_alias()
) -> binary().
occ_record_dyn(#rec_err{a = err, b = B}) ->
    B;
occ_record_dyn(D) -> D.

-record(ref_rec, {
    a :: eqwalizer:refinable(any())
}).
-type ref_rec1() :: #ref_rec{
    a :: integer()
}.

-spec refine_dyn_record(
    dyn_alias()
) -> ref_rec1().
refine_dyn_record(R = #ref_rec{a = I})
    when is_integer(I) -> R.

-spec refine_dyn_select(
    dyn_alias()
) -> integer().
refine_dyn_select(R = #ref_rec{a = I})
    when is_integer(I) -> R#ref_rec.a.
    
% Testing interaction between generics and dynamic()
filter_none(D) ->
    maps:filtermap(
        fun(_, X) ->
            case X of
                none -> false;
                _ -> {true, X}
            end
        end,
        D
    ).
    
filter_none_ret_dyn(D) ->
    maps:filtermap(
        fun(_, X) ->
            Dyn = dyn_val(),
            case X of
                none -> false;
                _ -> {true, Dyn}
            end
        end,
        D
    ).

-spec refine_is_list(
    eqwalizer:dynamic() | error
) -> atom().
refine_is_list(L) when is_list(L) ->
  lists:nth(1, L).

-spec refine_is_map(
    eqwalizer:dynamic() | error
) -> atom().
refine_is_map(M) when is_map(M) ->
  maps:get(key, M).
