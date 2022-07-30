%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(structured_comment).

test(X) ->
    %$eqwalizer: dynamic(X)
    X.

test2(X) ->
    %$eqwalizer: X :: dynamic()
    X.
