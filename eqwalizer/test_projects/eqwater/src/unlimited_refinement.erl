%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(unlimited_refinement).
-compile([export_all, nowarn_export_all]).

-eqwalizer({unlimited_refinement, foo/1}).

-spec foo
    (binary() | integer()) -> boolean().
foo(B) when is_binary(B) ->
  foo(binary_to_integer(B));
foo(X) when 100 =< X, X =< 200 ->
  true;
foo(X) when 300 =< X, X =< 400 ->
  true;
foo(X) when 500 =< X, X =< 600 ->
  true;
foo(X) when 700 =< X, X =< 800 ->
  true;
foo(X) when 900 =< X, X =< 1000 ->
  true;
foo(X) when 1100 =< X, X =< 1200 ->
  true;
foo(X) when 1300 =< X, X =< 1400 ->
  true;
foo(X) when 1500 =< X, X =< 1600 ->
  true;
foo(X) ->
  is_special(X).

-spec is_special
    (integer()) -> boolean().
is_special(_N) ->
  false.

-spec bar
    (binary() | integer()) -> boolean().
bar(B) when is_binary(B) ->
  foo(binary_to_integer(B));
bar(X) when 100 =< X, X =< 200 ->
  true;
bar(X) when 300 =< X, X =< 400 ->
  true;
bar(X) when 500 =< X, X =< 600 ->
  true;
bar(X) when 700 =< X, X =< 800 ->
  true;
bar(X) when 900 =< X, X =< 1000 ->
  true;
bar(X) when 1100 =< X, X =< 1200 ->
  true;
bar(X) when 1300 =< X, X =< 1400 ->
  true;
bar(X) when 1500 =< X, X =< 1600 ->
  true;
bar(X) ->
  is_special(X).

-type map1() :: #{k11 := any(), k12 := any(), k13 := any()}.
-type map2() :: #{k21 := any(), k22 := any(), k23 := any()}.
-type map3() :: #{k31 := any(), k32 := any(), k33 := any()}.
-type map4() :: #{k41 := any(), k42 := any(), k43 := any()}.
-type map5() :: #{k51 := any(), k52 := any(), k53 := any()}.
-type map6() :: #{k61 := any(), k62 := any(), k63 := any()}.
-type map7() :: #{k71 := any(), k72 := any(), k73 := any()}.
-type map8() :: #{k81 := any(), k82 := any(), k83 := any()}.
-type map9() :: #{k91 := any(), k92 := any(), k93 := any()}.
-type or_map() ::
    #{ty := ok1, data := map1()} |
    #{ty := ok2, data := map2()} |
    #{ty := ok3, data := map3()} |
    #{ty := ok4, data := map4()} |
    #{ty := ok5, data := map5()} |
    #{ty := ok6, data := map6()} |
    #{ty := ok7, data := map7()} |
    #{ty := ok8, data := map8()} |
    #{ty := ok9, data := map9()}.

-spec map_union_neg(or_map()) -> ok.
map_union_neg(#{ty := Ty, data := Data}) ->
    case {Ty, Data} of
        {ok1, #{k11 := V1, k12 := V2, k13 := V3}} -> ok;
        {ok2, #{k21 := V1, k22 := V2, k23 := V3}} -> ok;
        {ok3, #{k31 := V1, k32 := V2, k33 := V3}} -> ok;
        {ok4, #{k41 := V1, k42 := V2, k43 := V3}} -> ok;
        {ok5, #{k51 := V1, k52 := V2, k53 := V3}} -> ok;
        {ok6, #{k61 := V1, k62 := V2, k63 := V3}} -> ok;
        {ok7, #{k71 := V1, k72 := V2, k73 := V3}} -> ok;
        {ok8, #{k81 := V1, k82 := V2, k83 := V3}} -> ok;
        {ok9, #{k91 := V1, k92 := V2, k93 := V3}} -> ok;
        V -> V
    end.
