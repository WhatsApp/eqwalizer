%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(recursive_type_deps1).

-export_type([my_int_list/0, tree2/0, expr/0]).

-type my_int_list() :: {nil} | {cons, integer(), my_int_list()}.

-type expr() :: expr_var() | expr_call() | expr_tuple().
-type expr_var() :: {expr_var, atom()}.
-type expr_call() :: {expr_call, Fun :: atom(), Args :: [expr()]}.
-type expr_tuple() :: {expr_tuple, Elems :: [expr()]}.

-record(leaf, {value :: atom()}).
-record(node, {value :: atom(), left :: tree(), right :: tree()}).
-type tree() :: #leaf{} | #node{}.

-type tree2() :: recursive_type_deps2:tree().
