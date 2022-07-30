%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(detached_specs1).

-compile([export_all, nowarn_export_all]).

-spec foo() -> atom().
-spec bar() -> atom().
foo() -> foo.
bar() -> bar.
