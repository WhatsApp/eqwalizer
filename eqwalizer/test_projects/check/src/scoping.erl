%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(scoping).

-compile([export_all, nowarn_export_all]).

-spec foo() -> term().
foo() -> atom.

-spec bar() -> term().
bar() -> number.

-spec test01() ->
    {tuple | triple, atom(), number()}.
test01() ->
    Marker = case bar() of
        {A, N} when
            is_atom(A),
            is_number(N) ->
                tuple;
        {A, N, _} when
            is_atom(A),
            is_number(N) ->
                triple
    end,
    {Marker, A, N}.

-spec test02() ->
    {an | na,
     atom() | number(),
     atom() | number()}.
test02() ->
    {X, Y} = {foo(), bar()},
    Marker = if
                 is_atom(X),
                 is_number(Y) ->
                     an;
                 is_number(X),
                 is_atom(Y) ->
                     na
             end,
    {Marker, X, Y}.

-spec test03() ->
    {tuple | triple, atom(), number()}.
test03() ->
    Marker = receive
                 {A, N} when
                     is_atom(A),
                     is_number(N) ->
                     tuple;
                 {A, N, _} when
                     is_atom(A),
                     is_number(N) ->
                     triple
             end,
    {Marker, A, N}.

-spec test04() ->
    {tuple | triple | timeout,
     atom(),
     number()}.
test04() ->
    Marker = receive
                 {A, N} when
                     is_atom(A),
                     is_number(N) ->
                     tuple;
                 {A, N, _} when
                     is_atom(A),
                     is_number(N) ->
                     triple
             after 10 ->
                A = a,
                N = 1,
                timeout
             end,
    {Marker, A, N}.

-spec test05() ->
    tuple | triple.
test05() ->
    Marker = try bar() of
                 {A, N} when
                     is_atom(A),
                     is_number(N) ->
                     tuple;
                 {A, N, _} when
                     is_atom(A),
                     is_number(N) ->
                     triple
             after
                    foo()
             end,
    Marker.

-spec test06() ->
    tuple | triple | error.
test06() ->
    Marker = try bar() of
                 {A, N} when
                     is_atom(A),
                     is_number(N) ->
                     tuple;
                 {A, N, _} when
                     is_atom(A),
                     is_number(N) ->
                     triple
             catch _ ->
                 error
             end,
    Marker.
