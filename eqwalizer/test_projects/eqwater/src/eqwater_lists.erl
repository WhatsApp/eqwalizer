%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(eqwater_lists).

-compile([export_all, nowarn_export_all]).

-spec occ_list_01
    ([integer()] | [atom()]) ->
    [integer()].
occ_list_01(L = [H | _]) when is_integer(H) -> L;
occ_list_01(_) -> [].

-spec occ_list_02
    ([binary()] | [atom()]) ->
    binary().
occ_list_02([H | _]) when is_binary(H) -> H;
occ_list_02([H | _]) -> atom_to_binary(H).

-spec occ_list_03
    ([binary()] | [atom()]) ->
    [binary()].
occ_list_03(L = [H | _]) when is_binary(H) -> L;
occ_list_03(L) -> lists:map(fun atom_to_binary/1, L).

-spec occ_list_04_neg
    ([binary() | atom()]) ->
    [binary()].
occ_list_04_neg(L = [H | _]) when is_binary(H) -> L.

-spec occ_list_05
    ([binary()] | [atom()]) ->
    [binary()].
occ_list_05([H | T]) when is_binary(H) -> T.

-spec occ_list_06
    ([term()]) ->
    [].
occ_list_06([_ | _]) -> [];
occ_list_06(L) -> L.

-spec occ_list_07_neg
    ([term()]) ->
    [].
occ_list_07_neg([_, _ | _]) -> [];
occ_list_07_neg(L) -> L.

-spec occ_list_08_neg
    ([term()] | atom()) ->
    atom().
occ_list_08_neg([_ | _]) -> ok;
occ_list_08_neg(A) -> A.

-spec occ_list_09
    ([term()] | atom()) ->
    atom().
occ_list_09([_ | _]) -> ok;
occ_list_09([]) -> nil;
occ_list_09(A) -> A.

% Not supported
-spec occ_list_10
    ([term()]) ->
    [].
occ_list_10([_, _ | _]) -> [];
occ_list_10([_]) -> [];
occ_list_10(L) -> L.

-spec occ_list_11_neg
    ([atom()] | [binary()]) ->
    [binary()].
occ_list_11_neg([_, X | _]) when is_atom(X)
    -> [atom_to_binary(X)];
occ_list_11_neg(L) -> L.

-spec occ_list_12
    ([atom()] | [binary()]) ->
    [binary()].
occ_list_12(L = [_, X | _]) when is_binary(X)
    -> L;
occ_list_12(_) -> [].

-spec occ_list_13
    ([binary()] | [atom()]) ->
    binary() | [].
occ_list_13([H | _]) when is_binary(H) -> H;
occ_list_13([H | _]) -> atom_to_binary(H);
occ_list_13(L) -> L.

-spec occ_list_14_neg
    ([binary()] | [atom()]) ->
    binary().
occ_list_14_neg([H | _]) when is_binary(H) -> H;
occ_list_14_neg([H | _]) -> atom_to_binary(H);
occ_list_14_neg(L) -> L.

-spec occ_list_15
    ([atom()] | [binary()]) ->
    [binary()].
occ_list_15(L) when is_binary(hd(L))
    -> L;
occ_list_15(_) -> [].

-spec occ_list_16
    ([atom()] | [binary()]) ->
    [atom()].
occ_list_16(L) when is_binary(hd(L))
    -> [];
occ_list_16(L) -> L.

-spec occ_list_17
    ([atom()] | [binary()]) ->
    [atom()].
occ_list_17([H | _]) when is_binary(H)
    -> [];
occ_list_17(L) -> L.

-spec occ_list_18
    (atom() | binary()) -> [atom()] | [binary()].
occ_list_18(V) -> [V].

-spec occ_list_19
    (atom() | binary()) -> [atom() | binary()].
occ_list_19(V) -> [ok | [V]].

-spec occ_list_20_neg
    (atom() | binary()) -> [atom()] | [binary()].
occ_list_20_neg(V) -> [ok | [V]].
