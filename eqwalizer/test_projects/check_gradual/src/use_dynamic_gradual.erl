%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(use_dynamic_gradual).

-compile([export_all, nowarn_export_all]).

-compile([export_all, nowarn_export_all]).

-spec use_dynamic_type
    (eqwalizer:dynamic()) -> {atom()}.
use_dynamic_type(Dyn) -> {Dyn}.

-spec use_dynamic_type_neg
    (eqwalizer:dynamic()) -> {atom()}.
use_dynamic_type_neg(Dyn) ->
  {erlang:atom_to_binary(Dyn)}.

-spec use_cast(any()) -> {atom()}.
use_cast(A) ->
  Dyn = eqwalizer:dynamic_cast(A),
  {Dyn}.
