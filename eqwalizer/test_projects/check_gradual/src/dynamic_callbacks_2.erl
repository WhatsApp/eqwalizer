%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(dynamic_callbacks_2).
-export([
    init/1,
    stop/1
]).
-behavior(my_gradual_behaviour).

-spec init(number() | eqwalizer:dynamic()) -> ok.
init(_) -> ok.

-spec stop(ok) -> ok.
stop(A) -> A.
