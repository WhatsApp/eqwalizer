%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(callbacks6_neg).
-compile([export_all, nowarn_export_all]).
-behavior(behave).

-spec use_invalid() -> ok.
use_invalid() -> ok.

% unspecced
use_invalid2() -> ok.

-spec use_use_invalid2_neg() -> ok.
use_use_invalid2_neg() ->
  use_invalid2().