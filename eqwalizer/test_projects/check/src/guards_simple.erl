%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(guards_simple).

-compile([export_all, nowarn_export_all]).

-spec atom_pos_1(term()) -> atom().
atom_pos_1(X) when is_atom(X) -> X.

-spec atom_pos_2(term()) -> atom().
atom_pos_2(X) ->
    if
        is_atom(X) -> X
    end.

-spec atom_pos_3(term()) -> atom().
atom_pos_3(X) ->
    case X of
        Y when is_atom(Y) -> Y
    end.

-spec atom_neg_1(term()) -> atom().
atom_neg_1(X) when is_number(X) -> X.

-spec atom_neg_2(term()) -> atom().
atom_neg_2(X) ->
    if
        is_list(X) -> X
    end.

-spec atom_neg_3(term()) -> atom().
atom_neg_3(X) ->
    case X of
        Y when is_tuple(Y) -> Y
    end.

-spec boolean_pos_1(term()) -> boolean().
boolean_pos_1(X) when is_boolean(X) -> X.

-spec boolean_pos_2(term()) -> boolean().
boolean_pos_2(X) ->
    if
        is_boolean(X) -> X
    end.

-spec boolean_pos_3(term()) -> boolean().
boolean_pos_3(X) ->
    case X of
        Y when is_boolean(Y) -> Y
    end.

-spec boolean_neg_1(term()) -> boolean().
boolean_neg_1(X) when is_atom(X) -> X.

-spec boolean_neg_2(term()) -> boolean().
boolean_neg_2(X) ->
    if
        is_atom(X) -> X
    end.

-spec boolean_neg_3(term()) -> boolean().
boolean_neg_3(X) ->
    case X of
        Y when is_atom(Y) -> Y
    end.

-spec float_pos(term()) -> float().
float_pos(X) when is_float(X) -> X.

-spec float_neg(term()) -> float().
float_neg(X) when is_boolean(X) -> X.

-spec integer_pos(term()) -> integer().
integer_pos(X) when is_integer(X) -> X.

-spec integer_neg(term()) -> integer().
integer_neg(X) when is_boolean(X) -> X.

-spec pid_pos(term()) -> pid().
pid_pos(X) when is_pid(X) -> X.

-spec pid_neg(term()) -> pid().
pid_neg(X) when is_number(X) -> X.

-spec port_pos(term()) -> port().
port_pos(X) when is_port(X) -> X.

-spec port_neg(term()) -> pid().
port_neg(X) when is_reference(X) -> X.

-spec reference_pos(term()) -> port().
reference_pos(X) when is_reference(X) -> X.

-spec reference_neg(term()) -> pid().
reference_neg(X) when is_port(X) -> X.
