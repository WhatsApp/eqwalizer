%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(type_asserts).

-compile([export_all, nowarn_export_all]).

-spec assert1(term()) -> binary().
assert1(Arg) ->
  is_binary(Arg) orelse throw(bad_arg),
  Arg.

-spec assert2(term()) -> binary().
assert2(Arg) ->
  is_binary(Arg) orelse error(bad_arg),
  Arg.

-spec assert3(binary() | undefined)
      -> binary().
assert3(Arg) ->
  Arg =/= undefined orelse throw(bad_arg),
  Arg.

-spec assert4(binary() | undefined)
      -> binary().
assert4(Arg) ->
  Arg =/= undefined orelse error(bad_arg),
  Arg.

-spec assert5_neg(
    string() | binary()
) -> binary().
assert5_neg(Arg) ->
  is_list(Arg) orelse throw(bad_arg),
  Arg.

%% we don't support not yet
-spec assert6(
    string() | binary()
) -> binary().
assert6(Arg) ->
  (not is_list(Arg))
    orelse throw(bad_arg),
  Arg.

-spec any_to_atom1(
    string() | binary()
) -> false | atom().
any_to_atom1(A) ->
  Res = is_list(A)
    andalso list_to_atom(A),
  Res.

-spec any_to_atom2(
    string() | binary()
) -> false | atom().
any_to_atom2(A) ->
  is_binary(A)
    andalso list_to_atom(A).

-spec any_to_atom3_neg(
    term()
) -> false | atom().
any_to_atom3_neg(A) ->
  is_binary(A)
    andalso list_to_atom(A).

-spec double_andalso(
    term(), term()
) -> false | {number(), atom()}.
double_andalso(N, A) ->
  is_number(N)
    andalso is_atom(A)
      andalso {N, A}.

-spec double_andalso_neg(
    term(), term()
) -> false |  {atom(), number()}.
double_andalso_neg(N, A) ->
  is_number(N)
    andalso is_atom(A)
    andalso {N, A}.

-spec scope_neg(term())
      -> {false | number(), number()}.
scope_neg(A) ->
  X = is_number(A) andalso A,
  {X, A}.

-spec assert7(
    string() | binary()
) -> binary().
assert7(Input) ->
  true = is_binary(Input),
  Input.
