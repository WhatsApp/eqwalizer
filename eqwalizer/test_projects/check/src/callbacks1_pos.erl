%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(callbacks1_pos).
-compile([export_all, nowarn_export_all]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).
-behavior(gen_server).
-behavior(my_behaviour).
-export_type([ty/0]).

-type ty() :: ok.

init(_) -> {ok, my_ret}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(ok, ok) ->
    {stop, ok, ok}.
handle_cast(_, _) ->
    {stop, ok, ok}.
