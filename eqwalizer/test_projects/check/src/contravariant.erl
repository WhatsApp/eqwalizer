%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(contravariant).

-compile([export_all, nowarn_export_all]).

-type contravariant(X) :: fun((X) -> ok).

-type ref_contravariant(X) :: contravariant(X).

-type ref_contravariant_ab() :: ref_contravariant(a | b).

-opaque opaque_contra(X) :: fun((X) -> ok).

-opaque opaque_ref_contra(X) :: ref_contravariant(X).

-opaque opaque_ref_contra_ab() :: ref_contravariant_ab().

-opaque opaque_ref_opaque(X) :: opaque_contra(X).

-opaque recur_contra(X) :: fun((recur_contra(X)) -> X).

-type complex_expansion_1(X) :: #{a => contravariant(X)}.

-type complex_expansion_2(X) :: {complex_expansion_2(X), complex_expansion_1(X)} | nil.

-opaque complex_expansion_opaque(X) :: complex_expansion_2(X).

-opaque contra_in_res(X) :: fun((a) -> contravariant(X)).

-opaque opaque_ok(X) :: X.

-opaque contra_in_opaque(X) :: opaque_ok(contravariant(X)).

-record(my_rec, {a :: eqwalizer:refinable(term())}).

-opaque contra_in_rec_ref(X) :: #my_rec{a :: contravariant(X)}.

-opaque contra_in_dict_values(X) :: #{atom() => contravariant(X)}.

-opaque contra_in_shape(X) :: #{a => ok, b := ok, c := contravariant(X)}.

-spec contravariant_subtype
    (ref_contravariant_ab()) ->
    ref_contravariant(a).
contravariant_subtype(F) -> F.

-spec contravariant_subtype_neg
    (ref_contravariant(a)) ->
    ref_contravariant_ab().
contravariant_subtype_neg(F) -> F.

-type runnable(A) :: fun((A) -> ok).
-type id(A) :: fun((A) -> A).

-type next(A) ::
    eos | {head, A}.

-type pred(A) ::
    {check, fun((next(A)) -> pred(A))}
    | stop.

-spec log(term()) -> ok.
log(Thing) ->
    io:format("~p~n", [Thing]).

-spec log_runnable1
    (runnable(A)) -> runnable(A).
log_runnable1(F) ->
    fun(A) ->
        log(A),
        F(A)
    end.

-spec log_runnable2
    (fun((A) -> ok)) -> fun((A) -> ok).
log_runnable2(F) ->
    fun(A) ->
        log(A),
        F(A)
    end.

-spec log_runnable3
    (runnable(atom())) -> runnable(atom()).
log_runnable3(F) ->
    fun(A) ->
        log(A),
        F(A)
    end.

-spec log_runnable4
    (fun((atom()) -> ok)) -> fun((atom()) -> ok).
log_runnable4(F) ->
    fun(A) ->
        log(A),
        F(A)
    end.

-spec log_id1
    (id(A)) -> id(A).
log_id1(F) ->
    fun(A) ->
        log(A),
        F(A)
    end.

-spec log_id2
    (fun((A) -> A)) -> fun((A) -> A).
log_id2(F) ->
    fun(A) ->
        log(A),
        F(A)
    end.

-spec log_id3
    (id(atom())) -> id(atom()).
log_id3(F) ->
    fun(A) ->
        log(A),
        F(A)
    end.

-spec log_id4
    (fun((atom()) -> atom())) -> fun((atom()) -> atom()).
log_id4(F) ->
    fun(A) ->
        log(A),
        F(A)
    end.

-type stream(A) :: eos | {head, A}.
-type predicate(A) :: fail | {next, fun((stream(A)) -> predicate(A))}.

-spec id_predicate(predicate(A)) -> predicate(A).
id_predicate(P) -> P.

-spec wrap_id(predicate(A)) -> predicate(A).
wrap_id(P) -> id_predicate(P).

-type constant(X) :: {a, constant(X)} | nil.

-spec constant_f(constant(X)) -> constant(X).
constant_f(C) -> C.

-spec apply_f(constant(X)) -> constant(X).
apply_f(C) -> constant_f(C).

-spec hd_contravariant([predicate(A)]) -> (predicate(A)).
hd_contravariant([F | _]) -> F.

-spec test_contravariant() -> (predicate(term())).
test_contravariant() -> hd_contravariant([]).

-spec id_pred
    (pred(A)) -> pred(A).
id_pred(P) -> P.

-spec apply_id_pred
    (pred(number())) -> pred(number()).
apply_id_pred(P) -> id_pred(P).
