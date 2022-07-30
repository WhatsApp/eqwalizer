%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(index1).
-compile([export_all, nowarn_export_all]).
-export_type([
  ty_a/0,
  ty_b/0,
  rec/0
]).
-include("my_header.hrl").

-record(rec, {field :: [integer()]}).
-type ty_a() :: ty_b().
-type ty_b() :: pid().
-type rec() :: #rec{}.

-spec foo(#rec{}) -> {
  ty_a(),
  ty_b(),
  index1:ty_a(),
  index1:ty_b(),
  index2:ty_a(),
  #rec{},
  rec()
}.
foo(X) ->
  _ = X#rec.field,
  _ = X#rec{field = 3},
  _ = #rec{},
  % note: we don't index headers
  _ = #header_rec{},
  _ = foo(X),
  _ = index1:foo(X),
  _ = index2:foo(X),
  _ = fun foo/1,
  _ = fun index2:foo/1.
