%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(misplaced_comment_error).

bad_hint_arg() ->
  get(
    %$eqwalizer: cast(Key) :: dynamic()
    key
  ).

bad_hint_comprehension() ->
  L0 = [1, 2, 3],
  L1 = [
    X || X <- L0,
    %$eqwalizer: cast(X) :: pos_integer()
    X > 1
  ],
  L1.
