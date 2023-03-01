%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(apply_none).

-compile([export_all, nowarn_export_all]).

-spec mk_fun() -> fun((term()) -> term()).
mk_fun() ->
    Fun = erlang:error(not_implemented),
    Fun.

-spec apply_my_fun(term()) -> term().
apply_my_fun(Arg) ->
    Fun = mk_fun(),
    Fun(Arg).

-spec apply_my_fun_inline(term()) -> term().
apply_my_fun_inline(Arg) ->
    Fun = erlang:error(not_implemented),
    Fun(Arg).

-spec apply_none1(term()) -> nok.
apply_none1(F)
    when is_function(F, 1),
    is_function(F, 2) ->
    F(a).

-spec apply_none2(none()) -> none().
apply_none2(F) -> F(ok).

-spec apply_none3(none()) -> none().
apply_none3(F) ->
    Res = F(ok),
    Res.
