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

-type f4(T) :: fun((...) -> T).

-type f5(T) :: fun((term()) -> T).

-type f6(T) :: fun((term(), term()) -> T).

-spec f4_to_f(f4(term())) -> fun().
f4_to_f(F) -> F.

-spec f5_to_f4(f5(term())) -> f4(term()).
f5_to_f4(F) -> F.

-spec f5_to_f4_cov(f5(a)) -> f4(a | b).
f5_to_f4_cov(F) -> F.

-spec f5_to_f4_cov_neg(f5(a | b)) -> f4(a).
f5_to_f4_cov_neg(F) -> F.

-spec f5_or_f6_to_f4(f5(atom()) | f6(number())) -> f4(atom() | number()).
f5_or_f6_to_f4(F) -> F.

-spec apply_f4_pos(f4(number())) -> number().
apply_f4_pos(F) -> F(a).

-spec apply_f4_neg(f4(number())) -> boolean().
apply_f4_neg(F) -> F(a).

-spec f4_id_T(f4(T)) -> f4(T).
f4_id_T(F) -> F.

-spec apply_f4_id(f4(a)) -> f4(a | b).
apply_f4_id(F) -> f4_id_T(F).

-spec fun_to_f4(fun()) -> f4(atom()).
fun_to_f4(F) -> F.

-spec fun2_to_f4(fun((term()) -> a)) -> f4(a | b).
fun2_to_f4(F) -> F.

-spec fun3_to_f4_neg(fun((term()) -> a | b)) -> f4(a).
fun3_to_f4_neg(F) -> F.

-spec f4_to_fun_pos(f4(a)) -> fun((term()) -> (a | b)).
f4_to_fun_pos(F) -> F.

-spec f4_to_fun_neg(f4(a | b)) -> fun((term()) -> a).
f4_to_fun_neg(F) -> F.

-spec map_f1(fun((term()) -> atom()), [term()]) -> [atom()].
map_f1(F, Ts) -> lists:map(F, Ts).

-spec map_f2(fun((eqwalizer:dynamic()) -> atom()), [term()]) -> [atom()].
map_f2(F, Ts) -> lists:map(F, Ts).

-spec map_f3(fun((...) -> atom()), [term()]) -> [atom()].
map_f3(F, Ts) -> lists:map(F, Ts).

-spec map_f4(fun((...) -> A), [term()]) -> [A].
map_f4(F, Ts) -> lists:map(F, Ts).

-spec map_f5(fun((term()) -> atom()), [term()]) -> [atom()].
map_f5(F, Ts) -> map_f4(F, Ts).
