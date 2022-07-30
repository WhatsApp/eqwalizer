%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(detached_specs2).

-compile([export_all, nowarn_export_all]).

bar() -> bar.
-spec foo() -> atom().

foo() -> foo.
-spec bar() -> bar.
