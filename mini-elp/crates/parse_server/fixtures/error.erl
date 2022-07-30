%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(error).

-spec unrecognized1() -> atom.
unrecognized1() ->
    %$:eqwalizer: cast(Key)
    Key = get(key),
    Key.

-spec unrecognized2() -> atom.
unrecognized2() ->
    %$eqwalizer:cast(Key)::dynamic()
    Key = get(key),
    Key.

-spec unrecognized3() -> atom.
unrecognized3() ->
    %$eqwalizer:cast(Key)
    %$eqwalizer: ::dynamic()
    Key = get(key),
    Key.

-spec unrecognized4() -> atom.
unrecognized4() ->
    %$eqwalizer: handle(Key)
    Key = get(key),
    Key.
