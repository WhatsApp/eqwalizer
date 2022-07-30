%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(strict_fun).
-compile([export_all, nowarn_export_all]).

-record(fun_wrap1, {
  inner_fun :: fun()
}).

-record(fun_wrap2, {
  inner_fun :: fun((atom()) -> atom())
}).

-spec mk_fun_wrap1a() -> #fun_wrap1{}.
mk_fun_wrap1a() ->
  #fun_wrap1{inner_fun =
    fun mk_fun_wrap1a/0
  }.

-spec mk_fun_wrap1b() -> #fun_wrap1{}.
mk_fun_wrap1b() ->
  #fun_wrap1{inner_fun =
    fun lists:filter/2
  }.

-spec mk_fun_wrap2(fun()) ->
  #fun_wrap2{}.
mk_fun_wrap2(F) ->
  #fun_wrap2{inner_fun = F}.

-spec filter_any(fun(), list()) -> list().
filter_any(F, L) ->
  lists:filter(F, L).

-spec fun_apply(fun(), atom()) -> atom().
fun_apply(F, A) ->
  F(A).
