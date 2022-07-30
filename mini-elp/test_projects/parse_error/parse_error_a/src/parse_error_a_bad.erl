%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(parse_error_a_bad).
-compile([export_all, nowarn_export_all]).

foo() -> ok;
foon() -> ok. % head-mismatch
