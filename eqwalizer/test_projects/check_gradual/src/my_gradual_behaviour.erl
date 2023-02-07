%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(my_gradual_behaviour).

-callback init(atom()) -> ok.

-callback stop(a | eqwalizer:dynamic()) -> ok.
