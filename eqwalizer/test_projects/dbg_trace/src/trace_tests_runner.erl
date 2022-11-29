%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(trace_tests_runner).

-export([base_trace_tests/0, type_trace_tests/0]).

base_trace_tests() ->
  TracerFile = "traces/base.dbg_trace",
  ok = filelib:ensure_dir(TracerFile),
  dbg:tracer(port, dbg:trace_port(file, TracerFile)),
  dbg:p(all, c),
  dbg:tpl(base_trace_tests, '_', cx),
  base_trace_tests:run(),
  dbg:flush_trace_port(),
  dbg:stop(),
  erlang:halt().

type_trace_tests() ->
  TracerFile = "traces/type.dbg_trace",
  ok = filelib:ensure_dir(TracerFile),
  dbg:tracer(port, dbg:trace_port(file, TracerFile)),
  dbg:p(all, c),
  dbg:tpl(type_trace_tests, '_', cx),
  type_trace_tests:run(),
  dbg:flush_trace_port(),
  dbg:stop(),
  erlang:halt().
