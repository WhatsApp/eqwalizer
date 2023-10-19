%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(expand).

-export([
    mk_bin_tree/1, mk_bin_tree_typo/1,
    tuple_box/1, fun_with_rec_constraints/0,
    uses_ty_w_unbound_var/2, uses_ty_w_unbound_var2/0,
    uses_trans_unbound_var/2, mk_bad_rec/0
]).

-export_type([bad_opaque/0, rec_box/0, bad_ref/0, box/1]).

-type bin_tree() ::
      {leaf, atom()}
    | {node, bin_tree(), bin_tree()}.

-type box(A) :: {box, A}.

-spec mk_bin_tree(atom()) -> bin_tree().
mk_bin_tree(A) -> {leaf, A}.

-spec mk_bin_tree_typo(atom())
    -> types1:bin_tree_typo().
mk_bin_tree_typo(A) -> {leaf, A}.

-spec tuple_box(Tuple) -> TupleBox when
    Tuple :: {A, B}, TupleBox :: {box(A), box(B)}.
tuple_box({A, B}) -> {{box, A}, {box, B}}.

-spec fun_with_rec_constraints() ->
    Rec when Rec :: {rec, Rec}.
fun_with_rec_constraints() ->
    {rec, fun_with_rec_constraints()}.

-record(rec_box, {box :: box(atom())}).
-type rec_box() :: #rec_box{}.

-type w_unbound_var() :: {_A, _B}.
-type trans_unbound_var()
    :: w_unbound_var().

-spec uses_ty_w_unbound_var(_A, _B) ->
    w_unbound_var().
uses_ty_w_unbound_var(A, B) -> {A, B}.

-spec uses_trans_unbound_var(_A, _B) ->
    trans_unbound_var().
uses_trans_unbound_var(A, B) -> {A, B}.

-type w_unbound_var2()
    :: _A.

-spec uses_ty_w_unbound_var2() ->
    w_unbound_var2().
uses_ty_w_unbound_var2() -> 1.

-record(bad_ref, {
    ref :: undefined_mod:ref()
}).
-type bad_ref() :: #bad_ref{}.

-opaque bad_opaque() :: bad_opaque().

-record(bad_rec, {id :: unknown:id()}).
-spec mk_bad_rec() -> #bad_rec{}.
mk_bad_rec() -> #bad_rec{}.

-spec double_constrain(Thing) -> Thing
  when Thing :: pid(), Thing :: atom().
double_constrain(X) -> X.
