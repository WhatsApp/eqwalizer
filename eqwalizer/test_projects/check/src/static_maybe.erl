%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(static_maybe).
-feature(maybe_expr, enable).

-compile([export_all, nowarn_export_all]).

-spec maybe_01({term(), atom()})
    -> atom().
maybe_01(T) ->
    maybe
        {ok, A} ?= T,
        A
    else
        _ -> err
    end.

-spec maybe_02_neg({term(), term()})
    -> atom().
maybe_02_neg(T) ->
    maybe
        {ok, A} ?= T,
        A
    else
        _ -> err
    end.

% In static mode, else patterns
% are mapped to term()
-spec maybe_03_neg({term(), atom()})
    -> atom().
maybe_03_neg(T) ->
    maybe
        {ok, A} ?= T,
        A
    else
        B -> B
    end.

-spec maybe_04_neg({term(), atom()})
    -> atom().
maybe_04_neg(T) ->
    maybe
        {ok, A} ?= T,
        A
    else
        _ -> 3
    end.

-spec maybe_05({term(), atom()})
    -> atom().
maybe_05(T) ->
    maybe
        {ok, A} ?= T,
        A
    else
        B when is_atom(B) -> B;
        _ -> err
    end.

% Occurrence typing is not supported
-spec maybe_06_neg({term(), atom()})
    -> atom().
maybe_06_neg(T) ->
    maybe
        {ok, A} ?= T,
        A
    else
        B when not is_atom(B) -> err;
        Atom -> Atom
    end.

-spec maybe_07
    ({ok, atom()})
    -> number().
maybe_07(T) ->
    maybe
        {err, V} ?= T,
        V
    else
        A when is_number(A) -> A
    end.
