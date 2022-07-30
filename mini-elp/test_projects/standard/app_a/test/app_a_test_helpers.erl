%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(app_a_test_helpers).
-typing([eqwalizer]).
-compile([export_all, nowarn_export_all]).

-spec fail() -> error.
fail() -> wrong_ret.

-spec ok() -> ok.
ok() -> ok.

