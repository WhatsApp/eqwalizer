%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(index2).
-compile([export_all, nowarn_export_all]).
-export_type([
  ty_a/0
]).

-record(rec, {field :: [atom()]}).
-type ty_a() :: boolean().

-spec foo(#rec{}) -> {
  ty_a(),
  index1:ty_a(),
  index1:rec()
}.
foo(X) ->
  X#rec.field.
