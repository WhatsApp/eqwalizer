%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(parse_error_a).
-export([
    test1/1, test2_neg/1



]).

-spec test1(_) -> atom().
test1(X) when is_atom(X) ->
    X.

-spec test2_neg([number()]) -> atom().
test2_neg(X) ->
    X.
