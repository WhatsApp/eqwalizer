%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(united_fun).

-compile([export_all, nowarn_export_all]).

-spec pair1
    (A, B) -> {A, B}.
pair1(X, Y) -> {X, Y}.

-spec pair2
    (A, A) -> {A, A}.
pair2(X, Y) -> {X, Y}.

-spec transform(AN, AN) -> {AN, AN}
    when AN :: atom() | number().
transform(Elem1, Elem2) ->
    Fun = if
        is_number(Elem1) ->
            fun pair1/2;
        true ->
            fun pair2/2
        end,
    Fun(Elem1, Elem2).
