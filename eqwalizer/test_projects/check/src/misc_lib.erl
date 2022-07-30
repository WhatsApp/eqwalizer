%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(misc_lib).

-compile([export_all, nowarn_export_all]).

-spec boolean_id(boolean()) -> boolean().
boolean_id(B) -> B.
