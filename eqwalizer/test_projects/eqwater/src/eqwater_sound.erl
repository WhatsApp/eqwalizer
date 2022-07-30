%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(eqwater_sound).

-compile([export_all, nowarn_export_all]).
-type ab() :: atom() | binary().

-record(rec1, {id:: ab()}).

-spec base1(ab())
  -> binary().
base1(A) when is_atom(A) ->
  atom_to_binary(A);
base1(B) -> B.

-spec base2(ab())
    -> binary().
base2(A) when is_atom(A); is_number(A) ->
  atom_to_binary(A);
base2(B) -> B.

-spec base3(ab())
    -> binary().
base3(B) when is_binary(B) ->
  B;
base3(A) -> atom_to_binary(A).

-spec t01(#rec1{}) -> binary().
t01(#rec1{id=A}) when is_atom(A) ->
  atom_to_binary(A);
t01(#rec1{id=B}) ->
  B.

-spec t02(ab())
    -> binary().
t02(B) when is_binary(B),
            bit_size(B) > 2 ->
  B;
%% A may be not a binary - because of
%% the condition bit_size
t02(A) -> atom_to_binary(A).

%% no smart logic about repeated vars
-spec t03(atom(), ab()) ->
  binary().
t03(A, A) -> atom_to_binary(A);
t03(_, B) -> B.

-spec tuple_trick_neg(_T, U) -> U.
tuple_trick_neg({X}, _) -> X.

-spec fun_trick_neg(_T, U) -> U.
fun_trick_neg(X, _) when is_function(X, 0)
  -> X.

-spec union_trick(A, A | {B}) -> B.
union_trick(_, {B}) -> B.

%% these edge cases don't
%% degrade with eqwater
-spec test33_pos(a | b, b | c) -> b.
test33_pos(AB, BC) ->
  case AB of BC -> BC end.

-spec test34_pos({a | b}, {b | c}) -> {b}.
test34_pos(AB, BC) ->
  case AB of BC -> BC end.

-spec test35_pos([a | b], [b | c]) -> [b].
test35_pos(AB, BC) ->
  case AB of BC -> BC end.

-spec test36_pos(F1, F2) -> F3
  when F1 :: fun((a) -> a | z),
  F2 :: fun((b) -> b | z),
  F3 :: fun((a | b) -> z).
test36_pos(F1, F2) ->
  case F1 of F2 -> F2 end.

-spec id(T) -> T.
id(X) -> X.

-spec foralls_matter() -> unreachable.
foralls_matter() ->
  X = fun erlang:is_number/1,
  case (fun id/1) of
    X -> X
  end.
