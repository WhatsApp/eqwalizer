%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(tuple).

-export([pair/2, pair/1]).

-export([first/1, second/1]).

-export([map_first/2,
         map_first/1,
         map_second/2,
         map_second/1,
         map_both/3,
         map_both/2]).

-spec pair(A, B) -> {A, B}.

pair(A, B) -> {A, B}.

-spec pair(A) -> fun((B) -> {A, B}).

pair(A) -> fun (B) -> pair(A, B) end.

-spec first({A, _}) -> A.

first({A, _}) -> A.

-spec second({_, B}) -> B.

second({_, B}) -> B.

-spec map_first(fun((A) -> X), {A, B}) -> {X, B}.

map_first(F, {A, B}) -> {F(A), B}.

-spec map_first(fun((A) -> X)) -> fun(({A, B}) -> {X,
                                                   B}).

map_first(F) -> fun (P) -> map_first(F, P) end.

-spec map_second(fun((B) -> Y), {A, B}) -> {A, Y}.

map_second(F, {A, B}) -> {A, F(B)}.

-spec map_second(fun((B) -> Y)) -> fun(({A, B}) -> {A,
                                                    Y}).

map_second(F) -> fun (P) -> map_second(F, P) end.

-spec map_both(fun((A) -> X), fun((B) -> Y),
               {A, B}) -> {X, Y}.

map_both(Fa, Fb, {A, B}) -> {Fa(A), Fb(B)}.

-spec map_both(fun((A) -> X), fun((B) -> Y)) -> fun(({A,
                                                      B}) -> {X, Y}).

map_both(Fa, Fb) -> fun (P) -> map_both(Fa, Fb, P) end.



