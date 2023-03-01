%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(pos).

-compile([export_all, nowarn_export_all]).

-spec atom_id1(atom()) -> atom().
atom_id1(X) ->
    X.

-spec atom_pat(atom()) -> term().
atom_pat(1) ->
    ok.

-spec atom_lit() -> atom().
atom_lit() -> ok.

-spec char_lit() -> number().
char_lit() -> $A.

-spec nil_lit() -> [atom()].
nil_lit() -> [].

-spec block_example() -> {atom()}.
block_example() ->
    begin
        block,
        {block}
    end.

-spec match_example() -> {match}.
match_example() ->
    M = match,
    {match} = {M}.

-spec clauses(term()) ->
    atom() | number().
clauses(X) ->
    case X of
        1 -> 1;
        1 -> ok
    end.
