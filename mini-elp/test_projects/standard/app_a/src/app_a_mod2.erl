%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(app_a_mod2).
-include("app_a.hrl").
-typing([eqwalizer]).
-export([
    id/1, unspecced/0, unspecced2_neg/1






]).
-export_type([
    alias/1,
    invalid/0



]).

-type alias(T) :: app_b:tup(T).
-type invalid() :: invalid().

-spec id(X) -> X.
id(X) ->
    X.

unspecced() -> whatevs.

unspecced2_neg(X) ->
    1 + an_atom,
    X.
