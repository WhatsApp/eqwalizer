%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(auto_imports).

-compile([export_all, nowarn_export_all]).
-compile({no_auto_import, [error/2]}).

-export([]).

-spec error(atom(), atom()) -> atom().
error(_, A) -> A.

-spec use_local_error() -> atom().
use_local_error() ->
  error(ok, ok).

-spec use_erlang_error() -> atom().
use_erlang_error() ->
  erlang:error(ok, ok).
