%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(gradual_misc).

-compile([export_all, nowarn_export_all]).

% code is unsafe
-spec use_invalid_opaque_1(
    opaque:contravariant(a)
) -> opaque:contravariant(a | b).
use_invalid_opaque_1(X) ->
  X.

% code is safe but violates opacity
-spec use_invalid_opaque_2(
    opaque:contravariant(a | b)
) -> opaque:contravariant(a).
use_invalid_opaque_2(X) ->
  X.
