%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(callbacks5_neg).
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  format_status/2
]).
-behavior(gen_server).

init([]) -> {ok, []}.

handle_call(_, _From, State) ->
  {reply, ok, State}.

-spec handle_cast(ok, ok) ->
  {stop, ok, ok}.
handle_cast(_, _) ->
  {stop, ok, ok}.

-spec format_status(bad, term())->term().
format_status(_, _) -> ok.
