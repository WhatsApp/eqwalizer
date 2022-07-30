%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(attributes).

%% TODO restore later - see T119734649
%% -binary_attr(<<1,2,3>>).

-map_attr(#{id => 1}).

-list_attr([1|1]).
