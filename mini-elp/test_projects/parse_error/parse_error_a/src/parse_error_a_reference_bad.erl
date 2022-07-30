%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(parse_error_a_reference_bad).
-export([main/0]).

-spec main() -> atom().
main() ->
    parse_error_a_bad:foo().
