%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(lists_tests).

-compile([export_all, nowarn_export_all]).

-spec lists_union_neg
    (atom(), binary()) -> [atom()] | [binary()].
lists_union_neg(V1, V2) -> [V1, V2].

-spec lists_union_2_neg
    (atom(), [atom()] | [binary()]) -> [atom()] | [binary()].
lists_union_2_neg(V, L) -> [V | L].
