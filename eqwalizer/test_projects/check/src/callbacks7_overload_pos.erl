%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(callbacks7_overload_pos).

-export([
  init/1,
  handle_call/3,
  handle_cast/2
]).
-behavior(gen_server).

init([]) -> {ok, []}.
-spec handle_call
    ({a, atom()}, term(), term())
      -> {reply, term(), term()};
    ({n, number()}, term(), term())
      -> {reply, term(), term()}.
handle_call({a, A}, From, State) ->
  log_atom(A),
  {reply, From, State};
handle_call({n, N}, From, State) ->
  log_number(N),
  {reply, From, State}.

-spec handle_cast(ok, ok) ->
  {stop, ok, ok}.
handle_cast(_, _) ->
  {stop, ok, ok}.

-spec log_atom(atom()) -> ok.
log_atom(_A) -> ok.

-spec log_number(number()) -> ok.
log_number(_N) -> ok.