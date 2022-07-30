%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(callbacks3_neg).
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).
-behavior(gen_server).

init([]) -> {ok, []}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(ok, ok) -> wrong_ret.
handle_cast(_, _) ->
    wrong_ret.
