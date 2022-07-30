%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(app_a_SUITE).
-compile([export_all, nowarn_export_all]).
-typing([eqwalizer]).

-include_lib("stdlib/include/assert.hrl").
-include("app_a.hrl").

-spec ok() -> ok.
ok() ->
    ?assert(true),
    case rand:uniform(1) of
        1 -> app_a_test_helpers:ok();
        _ -> app_a_test_helpers_not_opted_in:ok()
    end.

-spec fail() -> ok.
fail() ->
    app_a_test_helpers:fail().

