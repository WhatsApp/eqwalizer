%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(behave).

-callback foo() -> behave1:test().

-type invalid() :: _T.

-callback use_invalid() -> invalid().

-callback use_invalid2() -> invalid().
