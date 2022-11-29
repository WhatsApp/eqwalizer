%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(base_trace_tests).
-export([run/0]).

run() ->
  case01_bad_arg1([]),
  case02_bad_arg2(a, "b"),
  case03_bad_arg23(a, "b", {c}),
  case04_ok(a),
  case05_ok(a, b),
  case06_ok(a, b, c),
  case07_bad_arg123(1, 2, 3),
  case08_bad_res(),
  case09_overload_bad_arg({}),
  case10_overload_bad_res({}),
  ok.

-spec case01_bad_arg1(atom()) -> ok.
case01_bad_arg1(_) -> ok.

-spec case02_bad_arg2(atom(), atom()) -> ok.
case02_bad_arg2(_, _) -> ok.

-spec case03_bad_arg23(atom(), atom(), atom()) -> ok.
case03_bad_arg23(_, _, _) -> ok.

-spec case04_ok(atom()) -> ok.
case04_ok(_) -> ok.

-spec case05_ok(atom(), atom()) -> ok.
case05_ok(_, _) -> ok.

-spec case06_ok(atom(), atom(), atom()) -> ok.
case06_ok(_, _, _) -> ok.

-spec case07_bad_arg123(atom(), atom(), atom()) -> ok.
case07_bad_arg123(_, _, _) -> ok.

-spec case08_bad_res() -> atom().
case08_bad_res() -> #{}.

% we do not report mismatches in args
% for overloaded specs yet
-spec case09_overload_bad_arg
    (atom()) -> atom();
    (binary()) -> binary().
case09_overload_bad_arg(_) -> ok.

-spec case10_overload_bad_res
    (atom()) -> atom();
    (binary()) -> binary().
case10_overload_bad_res(_) -> {}.
