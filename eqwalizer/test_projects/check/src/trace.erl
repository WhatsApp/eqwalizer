%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(trace).

-export([mk_result_erlang/0]).

-spec mk_result_erlang() ->
    erlang:trace_info_return().
mk_result_erlang() ->
    {call_count, 1}.
