%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(hints).

-compile([export_all, nowarn_export_all]).

-spec reveal_type1
    (atom() | binary()) -> term().
reveal_type1(A) when is_atom(A) ->
    eqwalizer:reveal_type(A),
    A;
reveal_type1(B) ->
    B.

-type data() ::
    {tag1, atom()}
    | {tag1, binary()}
    | {tag2, pid()}.
-spec reveal_type1a(data()) -> atom().
reveal_type1a({tag1, _A}) ->
    eqwalizer:reveal_type(_A),
    tag1.

-spec reveal_type2
    (atom() | binary()) -> term().
reveal_type2(A) when is_atom(A) ->
    A;
reveal_type2(B) ->
    eqwalizer:reveal_type(B),
    B.

-spec reveal_type3
    (atom() | binary() | tuple())
        -> term().
reveal_type3(A) when is_atom(A) ->
    A;
reveal_type3(BT) ->
    eqwalizer:reveal_type(BT),
    BT.

-spec no_cast() -> atom().
no_cast() ->
    Key = get(key),
    Key.

-spec cast1() -> atom().
cast1() ->
    Key = get(key),
    Key.

-spec reveal_none(none()) -> none().
reveal_none(X) ->
    eqwalizer:reveal_type(X),
    error(none).
