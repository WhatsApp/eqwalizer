%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(type_predicates).

-compile([export_all, nowarn_export_all]).

-spec any_tuple(term()) -> tuple().
any_tuple(T) when is_tuple(T) -> T;
any_tuple(_) -> {}.

-spec tuple_slice
    ({term(), term()} | number())
    -> {term(), term()} | {}.
tuple_slice(T) when is_tuple(T) -> T;
tuple_slice(_) -> {}.

-spec tuple_gen
    ({} | {term()} | {term(), term()}) ->
    tuple().
tuple_gen(T) -> T.

-spec any_list(term()) -> list().
any_list(L) when is_list(L) -> L;
any_list(_) -> [].

-spec list_slice([a] | tuple()) -> [a].
list_slice(L) when is_list(L) -> L;
list_slice(_) -> [].

-spec list_gen
    ([number()] | [atom()]) -> list().
list_gen(L) -> L.

-spec any_binary(term()) -> binary().
any_binary(B) when is_binary(B) -> B;
any_binary(_) -> <<>>.

-spec any_bitstring(term()) -> bitstring().
any_bitstring(B)
    when is_bitstring(B) -> B;
any_bitstring(_) -> <<>>.

-spec binary_slice
    (binary() | list()) -> binary().
binary_slice(B) when is_binary(B) -> B.

-spec unit_fun() -> {}.
unit_fun() -> {}.

-spec id_any(term()) -> term().
id_any(X) -> X.

-spec any_fun(term()) -> fun().
any_fun(F) when is_function(F) -> F;
any_fun(_) -> fun unit_fun/0.

-spec fun_slice
    (fun((term()) -> term()) | atom())
    -> fun((term()) -> term()).
fun_slice(F) when is_function(F) -> F;
fun_slice(_) -> fun id_any/1.

-record(rec1, {id :: atom()}).
-record(rec2, {id :: atom()}).

-spec rec_slice
    (#rec1{} | atom()) -> #rec1{}.
rec_slice(R)
    when is_record(R, rec1) -> R;
rec_slice(A)
    when is_atom(A) -> #rec1{id = A}.

-spec rec_slice1
    (#rec1{} | atom()) -> #rec1{}.
rec_slice1(R)
    when is_record(R, rec1, 1) -> R;
rec_slice1(A)
    when is_atom(A) -> #rec1{id = A}.

-spec as_rec1
    (#rec1{} | #rec2{}) -> #rec1{}.
as_rec1(R)
    when is_record(R, rec1) -> R;
as_rec1(R)
    when is_record(R, rec2) ->
        #rec1{id = R#rec2.id}.

-spec as_rec1a
    (#rec1{} | #rec2{}) -> #rec1{}.
as_rec1a(R)
    when is_record(R, rec1, 1) -> R;
as_rec1a(R)
    when is_record(R, rec2, 1) ->
    #rec1{id = R#rec2.id}.

-spec as_rec1_neg
    (#rec1{} | #rec2{}) -> #rec1{}.
as_rec1_neg(R)
    when is_record(R, rec2) -> R.

-spec as_rec1a_neg
    (#rec1{} | #rec2{}) -> #rec1{}.
as_rec1a_neg(R)
    when is_record(R, rec2, 1) -> R.

-spec any_fun_x(term()) -> fun().
any_fun_x(F) when is_function(F, 2) -> F;
any_fun_x(F) when is_function(F, 3) -> F;
any_fun_x(_) -> fun unit_fun/0.

-spec any_fun_x(term(), number()) -> fun().
any_fun_x(F, Arity)
    when is_function(F, Arity) -> F;
any_fun_x(_, _) -> fun unit_fun/0.

-spec any_mk(term(), term()) -> map().
any_mk(M, K) when is_map_key(K, M) -> M;
any_mk(_, _) -> #{}.

-record(f0, {
    f :: fun(() -> term())
}).
-record(f1, {
    f :: fun((term()) -> term())
}).
-record(f2, {
    f :: fun((term(), term()) -> term())
}).

-type fs() :: #f0{} | #f1{} | #f2{}.

-spec as_fs(term()) -> fs().
as_fs(F) when is_function(F, 0) ->
    #f0{f = F};
as_fs(F) when is_function(F, 1) ->
    #f1{f = F};
as_fs(F) when is_function(F, 2) ->
    #f2{f = F}.

-spec as_f0_neg(term(), term()) -> fs().
as_f0_neg(F, A)
    when is_function(F, A) -> #f0{f = F}.

-spec as_f1_neg(term()) -> #f1{}.
as_f1_neg(F)
    when is_function(F, 2) -> #f1{f = F}.
