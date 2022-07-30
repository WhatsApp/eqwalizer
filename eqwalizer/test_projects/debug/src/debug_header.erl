%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(debug_header).
-compile([export_all, nowarn_export_all]).
-include("debug_header.hrl").
-export_type([t/0]).

% illustrates error locations
% when a definition comes from
% a header file
-type t() :: has_unbound().
