%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(fun_stats2).
-typing([eqwalizer]).

-export([
  errors/1
]).

-spec errors(atom()) -> ok.
errors(Atom) ->
  _ = 1 + Atom,
  _ = binary_to_atom(Atom),
  _ = "" ++ Atom,
  ok.
