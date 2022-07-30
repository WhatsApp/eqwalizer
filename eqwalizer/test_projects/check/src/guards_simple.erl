%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(guards_simple).

-compile([export_all, nowarn_export_all]).

-spec atom_pos_1(any()) -> atom().
atom_pos_1(X) when is_atom(X) -> X.

-spec atom_pos_2(any()) -> atom().
atom_pos_2(X) ->
    if
        is_atom(X) -> X
    end.

-spec atom_pos_3(any()) -> atom().
atom_pos_3(X) ->
    case X of
        Y when is_atom(Y) -> Y
    end.

-spec atom_neg_1(any()) -> atom().
atom_neg_1(X) when is_number(X) -> X.

-spec atom_neg_2(any()) -> atom().
atom_neg_2(X) ->
    if
        is_list(X) -> X
    end.

-spec atom_neg_3(any()) -> atom().
atom_neg_3(X) ->
    case X of
        Y when is_tuple(Y) -> Y
    end.

-spec boolean_pos_1(any()) -> boolean().
boolean_pos_1(X) when is_boolean(X) -> X.

-spec boolean_pos_2(any()) -> boolean().
boolean_pos_2(X) ->
    if
        is_boolean(X) -> X
    end.

-spec boolean_pos_3(any()) -> boolean().
boolean_pos_3(X) ->
    case X of
        Y when is_boolean(Y) -> Y
    end.

-spec boolean_neg_1(any()) -> boolean().
boolean_neg_1(X) when is_atom(X) -> X.

-spec boolean_neg_2(any()) -> boolean().
boolean_neg_2(X) ->
    if
        is_atom(X) -> X
    end.

-spec boolean_neg_3(any()) -> boolean().
boolean_neg_3(X) ->
    case X of
        Y when is_atom(Y) -> Y
    end.

-spec float_pos(any()) -> float().
float_pos(X) when is_float(X) -> X.

-spec float_neg(any()) -> float().
float_neg(X) when is_boolean(X) -> X.

-spec integer_pos(any()) -> integer().
integer_pos(X) when is_integer(X) -> X.

-spec integer_neg(any()) -> integer().
integer_neg(X) when is_boolean(X) -> X.

-spec pid_pos(any()) -> pid().
pid_pos(X) when is_pid(X) -> X.

-spec pid_neg(any()) -> pid().
pid_neg(X) when is_number(X) -> X.

-spec port_pos(any()) -> port().
port_pos(X) when is_port(X) -> X.

-spec port_neg(any()) -> pid().
port_neg(X) when is_reference(X) -> X.

-spec reference_pos(any()) -> port().
reference_pos(X) when is_reference(X) -> X.

-spec reference_neg(any()) -> pid().
reference_neg(X) when is_port(X) -> X.
