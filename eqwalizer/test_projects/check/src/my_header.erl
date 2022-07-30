%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(my_header).
-compile([export_all, nowarn_export_all]).


-include("my_header.hrl").

% some stuff
-spec good_fun() -> atom().
good_fun() -> ok.
