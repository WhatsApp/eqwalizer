%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(gradual_maybe).
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

% In gradual mode, else patterns
% are just mapped to dynamic()
-spec maybe_03({term(), atom()})
    -> atom().
maybe_03(T) ->
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

-spec maybe_05_neg
    ({ok, atom()} | {err, term()})
    -> atom().
maybe_05_neg(T) ->
    maybe
        {err, A} ?= T,
        A
    end.

-spec maybe_06_neg
    ({ok, atom()} | {err, term()})
    -> ok.
maybe_06_neg(T) ->
    maybe
        {ok, A} ?= T,
        ok = B ?= A,
        B
    end.

-spec maybe_07
    ({ok, atom()} | {err, term()})
    -> ok.
maybe_07(T) ->
    maybe
        {ok, A} ?= T,
        ok = B ?= A,
        B
    else
        _ -> ok
    end.

-spec maybe_08_neg
    ({ok, atom()} | {err, term()})
    -> err.
maybe_08_neg(T) ->
    maybe
        {ok, A} ?= T,
        ok = B ?= A,
        B
    else
        _ -> ok
    end.

-spec maybe_09
    ({ok, atom()})
    -> number().
maybe_09(T) ->
    maybe
        {err, V} ?= T,
        V
    else
        _ -> 0
    end.

-spec maybe_10
    ({ok, atom()})
    -> number().
maybe_10(T) ->
    maybe
        {err, V} ?= T,
        V
    else
        A -> A
    end.

-spec maybe_11_neg
    (a | b | c)
    -> b | c.
maybe_11_neg(T) ->
    maybe
        a = A ?= T,
        b
    end.

-spec maybe_12
    ({ok, a | b} | err)
    -> ok | b | err.
maybe_12(T) ->
    maybe
        {ok, V} ?= T,
        a = A ?= V,
        ok
    else
        _ -> ok
    end.

-spec maybe_13_neg
    (term()) -> ok.
maybe_13_neg(T) ->
    maybe
        A = ok ?= T
    else
        a -> ok;
        b -> ok;
        c -> err
    end.

% Testing elab mode
-spec maybe_14_neg
    (term()) -> err.
maybe_14_neg(T) ->
    maybe
        A ?=
            maybe
                B ?= {a, T},
                V = {b, B},
                C ?= V
            end
    end.
