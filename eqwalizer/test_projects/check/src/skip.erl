%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(skip).

-compile([export_all, nowarn_export_all]).

-spec bad_mixed_update
    (term()) -> term().
bad_mixed_update(M)
    when is_map(M#{b => b, a := a}) -> M.
