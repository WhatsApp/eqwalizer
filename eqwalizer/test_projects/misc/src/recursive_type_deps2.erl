%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(recursive_type_deps2).

-export([]).

-export_type([my_int_list_alias1/0, tree/0]).

-type my_int_list_alias1() :: recursive_type_deps1:my_int_list().

-record(leaf, {value :: atom()}).
-record(node, {value :: atom(), left :: tree(), right :: tree()}).
-type tree() :: #leaf{} | #node{}.
