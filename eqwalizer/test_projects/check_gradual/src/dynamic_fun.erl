%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(dynamic_fun).
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

-spec m_id1(module()) -> fun().
m_id1(M) ->
  fun M:id/0.

-spec m_id2(module()) -> fun().
m_id2(M) ->
  F = fun M:id/0,
  F.

-spec m_id3(module()) ->
  fun(() -> pid()).
m_id3(M) ->
  fun M:id/0.

-spec m_id4(module()) ->
  fun(() -> pid()).
m_id4(M) ->
  Res = fun M:id/0,
  Res.

-spec m_id5_neg(module()) ->
  fun(() -> pid()).
m_id5_neg(M) ->
  fun M:id/1.

-spec m_id6_neg(module()) ->
  fun(() -> pid()).
m_id6_neg(M) ->
  Res = fun M:id/1,
  Res.

-spec m_fun1(module(), atom()) ->
  fun(() -> pid()).
m_fun1(M, F) ->
  fun M:F/0.

-spec m_fun2(module(), atom()) ->
  fun(() -> pid()).
m_fun2(M, F) ->
  Res = fun M:F/0,
  Res.

-spec m_fun3_neg(module(), atom()) ->
  fun((pid()) -> pid()).
m_fun3_neg(M, F) ->
  fun M:F/2.

-spec m_fun4_neg(module(), atom()) ->
  fun((pid()) -> pid()).
m_fun4_neg(M, F) ->
  Res = fun M:F/2,
  Res.

-spec m_fun5_neg({module()}, atom()) ->
  fun((pid()) -> pid()).
m_fun5_neg(M, F) ->
  fun M:F/1.

-spec m_fun6_neg(module(), {atom()}) ->
  fun((pid()) -> pid()).
m_fun6_neg(M, F) ->
  fun M:F/1.

-spec m_fun7_neg
  (module(), atom(), pid())
    -> fun((pid()) -> pid()).
m_fun7_neg(M, F, A) ->
  fun M:F/A.

-spec take_fn_in_tup_1(
    {fun((a) -> b)}
) -> ok.
take_fn_in_tup_1({F}) ->
  _ = atom_to_list(F(a)),
  ok.

-spec lambda_1() -> ok.
lambda_1() ->
  take_fn_in_tup_1(
    {fun(_X) -> b end}
  ).

-spec lambda_2() -> ok.
lambda_2() ->
  take_fn_in_tup_1(
    {fun Named(_X) -> b end}
  ).
