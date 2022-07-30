%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(nowarn).

-compile([export_all, nowarn_export_all]).
-eqwalizer({nowarn_function, nowarn_bad/0}).
-eqwalizer({nowarn_function, nowarn_redundant/0}).

-spec good() ->
  integer().
good() ->
  1.

-spec bad() ->
  integer().
bad() ->
  ok.

-spec nowarn_bad() ->
  integer().
nowarn_bad() ->
  ok.

-spec nowarn_redundant() ->
  integer().
nowarn_redundant() ->
  1.
