%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(gradual_custom).

-compile([export_all, nowarn_export_all]).

% element/2 - dynamic tuple examples

-spec element_2_dynamic_1_print(eqwalizer:dynamic()) -> none().
element_2_dynamic_1_print(Tup) ->
    eqwalizer:reveal_type(element(42, Tup)).

-spec element_2_dynamic_2_print(eqwalizer:dynamic({number(), atom()})) -> none().
element_2_dynamic_2_print(Tup) ->
    eqwalizer:reveal_type(element(2, Tup)).

-spec element_2_dynamic_3_print(eqwalizer:dynamic({number(), atom()})) -> none().
element_2_dynamic_3_print(Tup) ->
    eqwalizer:reveal_type(element(1, Tup)).

-spec element_2_dynamic_4_print(eqwalizer:dynamic(number())) -> none().
element_2_dynamic_4_print(Tup) ->
    eqwalizer:reveal_type(element(1, Tup)).

-spec element_2_dynamic_5_print(pos_integer(), eqwalizer:dynamic({number(), atom()})) -> none().
element_2_dynamic_5_print(N, Tup) ->
    eqwalizer:reveal_type(element(N, Tup)).

-spec app_env1_gradual() -> number().
app_env1_gradual() ->
  Res = application:get_env(app1),
  case Res of
    undefined -> 0;
    {ok, N} -> N
  end.

-spec app_env1_gradual_neg()
-> number().
app_env1_gradual_neg() ->
  application:get_env(app1).

-spec app_env2_gradual() -> number().
app_env2_gradual() ->
  Res = application:get_env(app1, key1),
  case Res of
    undefined -> 0;
    {ok, N} -> N
  end.

-spec app_env2_gradual_neg()
-> number().
app_env2_gradual_neg() ->
  application:get_env(app1, key1).

-spec app_env3_gradual() -> number().
app_env3_gradual() ->
  A = app1,
  K = key,
  U = undefined,
  application:get_env(A, K, U).

-spec flatten1_gradual() -> [atom()].
flatten1_gradual() ->
  In = [a, [b, c]],
  lists:flatten(In).

-spec flatten1_gradual_neg() -> atom().
flatten1_gradual_neg() ->
  In = [a, [b, c]],
  lists:flatten(In).

-spec flatten2_gradual() -> [atom()].
flatten2_gradual() ->
  In = [a, [b, c]],
  Tail = [x, y, z],
  lists:flatten(In, Tail).

-spec flatten2_gradual_neg() -> atom().
flatten2_gradual_neg() ->
  In = [a, [b, c]],
  Tail = [x, y, z],
  lists:flatten(In, Tail).

-spec file_consult() -> [anything].
file_consult() ->
  {ok, Res} = file:consult(some_file),
  Res.

-spec file_consult_neg() -> nok.
file_consult_neg() ->
  file:consult(some_file).

-spec abs1() ->
  anything.
abs1() ->
  abs(dynamic()).

-spec abs2_neg() ->
  anything.
abs2_neg() ->
  N = case rand:uniform(2) of
        1 -> dynamic();
        2 -> a
      end,
  abs(N).

dynamic() ->
  ok.

-spec ets_lookup_1(term()) ->
  [{whatev} | {2}].
ets_lookup_1(Any) ->
  ets:lookup(tab, Any).

-spec ets_lookup_2_neg(term()) -> pid().
ets_lookup_2_neg(Any) ->
  ets:lookup(tab, Any).

-spec ets_lookup_3_neg(term()) -> pid().
ets_lookup_3_neg(Any) ->
  ets:lookup("not atom", Any).

-spec ets_lookup_4(term()) ->
  [tuple()].
ets_lookup_4(Any) ->
  ets:lookup(tab, Any).

-spec ets_tab2list_1(atom()) ->
  [tuple()].
ets_tab2list_1(Atom) ->
  ets:tab2list(Atom).

-spec ets_tab2list_2(atom()) ->
  [{whatev} | {2}].
ets_tab2list_2(Atom) ->
  ets:tab2list(Atom).

-spec ets_tab2list_3_neg() -> term().
ets_tab2list_3_neg() ->
  ets:tab2list("not atom").

-spec maps_without_1
    (eqwalizer:dynamic()) ->
  #{atom() => y | z}.
maps_without_1(Dyn) ->
  maps:without(
    [Dyn],
    #{a => y, b => z}
  ).

-spec maps_without_2_neg
    (eqwalizer:dynamic()) ->
  wrong_ret.
maps_without_2_neg(Dyn) ->
  maps:without(
    [a],
    Dyn
  ).

-spec maps_without_3_neg
    (none()) ->
  wrong_ret.
maps_without_3_neg(None) ->
  maps:without(
    [a, b],
    None
  ).

-spec maps_with_1
    (eqwalizer:dynamic()) ->
  #{atom() => y | z}.
maps_with_1(Dyn) ->
  maps:with(
    [Dyn],
    #{a => y, b => z}
  ).

-spec maps_with_2_neg
    (eqwalizer:dynamic()) ->
  wrong_ret.
maps_with_2_neg(Dyn) ->
  maps:with(
    [a],
    Dyn
  ).

-spec maps_with_3
    (none()) ->
  wrong_ret.
maps_with_3(None) ->
  maps:with(
    [a, b],
    None
  ).

-spec filename_join_2_dyn1
    (eqwalizer:dynamic()) -> ok.
filename_join_2_dyn1(Dyn) ->
  Res = filename:join("path", Dyn),
  eqwalizer:reveal_type(Res),
  Res.

-spec filename_join_2_dyn2
    (eqwalizer:dynamic()) -> ok.
filename_join_2_dyn2(Dyn) ->
  Res = filename:join(Dyn, Dyn),
  eqwalizer:reveal_type(Res),
  Res.

-spec filename_join_1_dyn1
    (eqwalizer:dynamic()) -> ok.
filename_join_1_dyn1(Dyn) ->
  Res = filename:join(["path", Dyn]),
  eqwalizer:reveal_type(Res),
  Res.

-spec filename_join_1_dyn2
    (eqwalizer:dynamic()) -> ok.
filename_join_1_dyn2(Dyn) ->
  Res = filename:join([Dyn, Dyn]),
  eqwalizer:reveal_type(Res),
  Res.
