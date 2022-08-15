%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(gradual_overloaded).

-compile([export_all, nowarn_export_all]).

-spec swap
    (atom()) -> binary();
    (binary()) -> atom().
swap(A) when is_atom(A) ->
    atom_to_binary(A);
swap(B) when is_binary(B) ->
    binary_to_atom(B).

-spec extract
    ({pid, pid()}) -> pid();
    ({binary, binary()}) -> binary().
extract({pid, Pid}) -> Pid;
extract({binary, Bin}) -> Bin.

use_swap(X) ->
    Y = swap(X),
    atom_to_binary(Y).

use_extract1(X) ->
    Y = extract(X),
    binary_to_atom(Y).

use_extract2(X) ->
    Y = extract({binary, X}),
    binary_to_atom(Y).

use_extract3(X) ->
    Y = extract({pid, X}),
    binary_to_atom(Y).

-spec use_swap_specced(binary()) ->
    binary().
use_swap_specced(X) ->
    Y = swap(X),
    atom_to_binary(Y).

-spec use_extract1_specced(
    {binary, binary()}
) -> atom().
use_extract1_specced(X) ->
    Y = extract(X),
    binary_to_atom(Y).

dynamic() ->
    dynamic.

-spec use_swap_one_more_time()
    -> integer().
use_swap_one_more_time() ->
    X = dynamic(),
    Y = swap(X),
    {Y}.

-spec fst_gen
    ({A}) -> A;
    ([A]) -> A.
fst_gen({A}) -> A;
fst_gen([A]) -> A.

-spec use_fst_gen1
    ({atom()}) -> atom().
use_fst_gen1(X) ->
    fst_gen(X).

-spec use_fst_gen2
    ({atom()} | [atom()]) -> atom().
use_fst_gen2(X) ->
    fst_gen(X).

-spec use_fst_gen3
    (eqwalizer:dynamic())
        -> eqwalizer:dynamic().
use_fst_gen3(X) ->
    fst_gen(X).

-spec use_fst_gen4
    (eqwalizer:dynamic())
        -> eqwalizer:dynamic().
use_fst_gen4(X) ->
    Res = fst_gen(X),
    eqwalizer:reveal_type(Res),
    Res.

-record(r, {count :: integer()}).

-spec rec_each
    (fun((#r{}) -> #r{}), #r{}) -> #r{};
    (fun((#r{}) -> #r{}), [#r{}]) -> [#r{}].
rec_each(F, R) when is_record(R, r) ->
    F(R);
rec_each(F, Rs) when is_list(Rs) ->
    lists:map(F, Rs).

-spec rec_each1(#r{}) -> #r{}.
rec_each1(Rec) ->
    rec_each(
        fun(R) -> R#r{count = 0} end,
        Rec
    ).

-spec rec_each2_neg(#r{}) -> atom().
rec_each2_neg(Rec) ->
    rec_each(
        fun(R) -> R#r{count = 0} end,
        Rec
    ).

-spec rec_each3_neg(#r{}) -> atom().
rec_each3_neg(Rec) ->
    rec_each(
        fun(I) -> I + 1 end,
        Rec
    ).

% optimistically assuming dynamic()
-spec rec_each4
    (#r{} | [#r{}]) -> atom().
rec_each4(Rec) ->
    rec_each(
        fun(I) -> I + 1 end,
        Rec
    ).
