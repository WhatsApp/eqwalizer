%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(eqwater_generics).

-compile([export_all, nowarn_export_all]).

-spec lists_map1([atom() | string()])
      -> [binary()].
lists_map1(L) ->
  lists:map(
    fun
      (A) when is_atom(A) ->
        atom_to_binary(A);
      (S) when is_list(S) ->
        list_to_binary(S)
    end,
    L
  ).

-spec lists_map2([atom() | string()])
      -> [binary()].
lists_map2(L) ->
  lists:map(
    fun
      (A) when is_atom(A) ->
        atom_to_binary(A);
      (S) ->
        list_to_binary(S)
    end,
    L
  ).
