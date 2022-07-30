%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(deep_tuples).

-compile([export_all, nowarn_export_all]).

-spec test1_neg
    ({{ok, number()}, {ok, number()}})
      -> none().
test1_neg({{Tag1, _}, {_Tag2, _}}) ->
  Tag1.

-spec test2_neg
    ({{ok, number()}, {ok, number()}})
      -> none().
test2_neg({{_Tag1, _}, {Tag2, _}}) ->
  Tag2.

-spec test3_neg
    ({{ok, number()}, {ok, number()}})
      -> none().
test3_neg({{Tag1, _}, {_, _}}) ->
  Tag1.

-spec test4_neg
    ({{ok, number()}, {ok, number()}})
      -> none().
test4_neg({{_, _}, {Tag2, _}}) ->
  Tag2.
