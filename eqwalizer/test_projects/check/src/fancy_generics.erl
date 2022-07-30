%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(fancy_generics).

-export([f/2]).

-spec f(T, _U) -> T.
f(T, _) -> T.
