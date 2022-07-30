%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(tagged_tuples).

-compile([export_all, nowarn_export_all]).

% ordinary tagged tuples
-type result(Res, Err) ::
    {ok, Res} | {err, Err}.

% "reversed" tagged tuples
% the tag/marker is the last element
-type r_result(Res, Err) ::
    {Res, ok} | {Err, err}.

-spec mk_result(boolean()) ->
    result(atom(), binary()).
mk_result(true) -> {ok, result};
mk_result(false) -> {err, <<>>}.

-spec test(boolean()) -> atom().
test(B) ->
    case mk_result(B) of
        {ok, X} -> X;
        {err, _} -> err
    end.

-spec mk_r_result(boolean()) ->
    r_result(atom(), binary()).
mk_r_result(true) -> {result, ok};
mk_r_result(false) -> {<<>>, err}.

-spec test_r(boolean()) -> atom().
test_r(B) ->
    case mk_r_result(B) of
        {X, ok} ->
            X;
        {Z, err} ->
            erlang:binary_to_atom(Z)
    end.

-type marked_tuple()
    :: {number(), atom()}
     | {atom(), number()}.

% extract atom directly
-spec get_atom1(marked_tuple()) -> atom().
get_atom1({_, A}) when is_atom(A) -> A;
get_atom1({A, _}) when is_atom(A) -> A.

% extract atom by elimination
-spec get_atom2(marked_tuple()) -> atom().
get_atom2({N, A}) when is_number(N) -> A;
get_atom2({A, N}) when is_number(N) -> A.

% the tag is nested and a bit indirect
-type deep_tagged_tuple() ::
{{id, atom()}, {age, number()}} |
{{borough, string()}, {city, string()}}.

-spec get_age
(deep_tagged_tuple()) -> number().
get_age({{id, _}, {_, A}}) -> A.

-spec get_city
    (deep_tagged_tuple()) -> string().
get_city({{borough, _}, {_, C}}) -> C.

-type file_result() ::
{ok, Handle :: integer()}
| {error, Reason :: string()}.

-spec file_open() -> file_result().
file_open() ->
    case rand:uniform(2) of
        1 -> {ok, 20};
        2 -> {error, "ENOTFOUND"}
    end.
