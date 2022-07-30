%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(eqwalizer_rebar3).

-export([init/1]).

init(State) ->
    {ok, (rebar_state:create_logic_providers([eqwalizer_build_info_prv], State))}.
