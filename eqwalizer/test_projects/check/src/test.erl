%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(test).
-compile([export_all, nowarn_export_all]).

-spec clauses(term()) ->
    atom() | number().
clauses(X) ->
    case X of
        1 -> 1;
        1 -> ok
    end.
