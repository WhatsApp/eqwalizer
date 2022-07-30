%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(dyn_calls).

-compile([export_all, nowarn_export_all]).

-spec dyn_call_check0
(atom(), atom()) -> integer().
dyn_call_check0(M, F) ->
  M:F().

-spec dyn_call_check1
(atom(), atom(), integer()) ->
integer().
dyn_call_check1(M, F, Arg) ->
  M:F(Arg).

-spec dyn_call_check2
(atom(), atom(), integer(), atom()) ->
integer().
dyn_call_check2(M, F, Arg1, Arg2) ->
  M:F(Arg1, Arg2).

-spec dyn_call_eval0
(atom(), atom()) -> integer().
dyn_call_eval0(M, F) ->
  Res = M:F(),
  Res.

-spec dyn_call_eval1
(atom(), atom(), integer()) ->
integer().
dyn_call_eval1(M, F, Arg) ->
  Res = M:F(Arg),
  Res.

-spec dyn_call_eval2
(atom(), atom(), integer(), atom()) ->
  integer().
dyn_call_eval2(M, F, Arg1, Arg2) ->
  Res = M:F(Arg1, Arg2),
  Res.

-spec dyn_arity_neg() -> ok.
dyn_arity_neg() ->
  (fun() -> res end)(arg),
  ok.
