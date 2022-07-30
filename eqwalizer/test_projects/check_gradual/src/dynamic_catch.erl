%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(dynamic_catch).
-compile([export_all, nowarn_export_all]).

-spec foo() -> {ok, atom()}.
foo() -> {ok, foo}.

-spec test01_pos() -> atom().
test01_pos() ->
  case catch foo() of
    {ok, A} -> A;
    {err, A} -> A
  end.

-spec test02_neg() -> binary().
test02_neg() ->
  case catch foo() of
    {ok, A} -> A;
    {err, A} -> A
  end.
