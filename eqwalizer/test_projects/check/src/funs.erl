%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(funs).
-compile([export_all, nowarn_export_all]).

-spec map_const_pos1()
    -> [number()].
map_const_pos1() ->
    lmap(fun (_) -> 3 end, [a, a, a]).

-spec map_const_pos2() -> [number()].
map_const_pos2() ->
    X =lmap(fun (_) -> 3 end, [a, a, a]),
    X.

-spec check_fun_pos() -> fun().
check_fun_pos() -> fun() -> ok end.

-spec check_fun_neg() -> pid().
check_fun_neg() -> fun() -> ok end.

-spec body_is_not_checked() -> fun().
body_is_not_checked() ->
    Tup = {},
    fun() -> list_to_atom(Tup) end.

-spec test_id() -> number().
test_id() ->
    (fun(X) -> X end)(2).

-spec test_const() -> number().
test_const() ->
    X = (fun() -> 2 end)(),
    X.

% we won't do anything interesting here
% until much later
-spec check_fun_specific1() ->
    fun((T) -> T).
check_fun_specific1() ->
    fun(T) -> T end.

-spec check_fun_specific2() ->
    fun(({T}) -> T).
check_fun_specific2() ->
    fun({T}) -> T end.

-type n() :: number().

-spec lam_arg_1_pos() -> [n()].
lam_arg_1_pos() ->
    lmap_n_to_n(
        fun(X) -> X * 2 end,
        [1,2,3]
    ).

-spec lam_arg_2_neg() -> [n()].
lam_arg_2_neg() ->
    lmap_n_to_n(fun(X) ->
        X ++ [] end,
        [1,2,3]
    ).

-spec lam_arg_3_neg() -> [n()].
lam_arg_3_neg() ->
    lmap_n_to_n(fun(X) ->
            erlang:atom_to_binary(X)
            end,
        [1,2,3]
    ).

-spec lam_arg_4_neg() -> [a].
lam_arg_4_neg() ->
    lmap_n_to_n(
        fun(X) -> X end,
        [1,2,3]
    ).

-spec lam_ret_1_pos() ->
    fun((n(), n()) -> n()).
lam_ret_1_pos() ->
    fun(X, Y) -> X + Y end.

-spec lam_ret_1_neg() ->
    fun((n(), n()) -> n()).
lam_ret_1_neg() ->
    fun(X, Y) -> X ++ Y end.

-spec lam_ret_2_neg() ->
    fun((n(), n()) -> n()).
lam_ret_2_neg() ->
    fun(X, Y) ->
        erlang:atom_to_binary(X),
        X ++ Y
    end.

-spec lam_ret_3_neg() ->
    fun((n(), n()) -> n()).
lam_ret_3_neg() ->
    fun(X, _) ->
      erlang:is_number(X)
    end.

-spec map_lam_ab() -> [a].
map_lam_ab() ->
    lmap(fun (X) -> X end, [a, a, a]).

-spec map_lam_aa() -> [a].
map_lam_aa() ->
    lmap_aa(fun (X) -> X end, [a, a, a]).

-spec map_lam_a_num() -> [n()].
map_lam_a_num() ->
    lmap_a_num(
        fun (_) ->
            3 end,
        [a, a, a]
    ).

-spec map_lam_bad_arity()
    -> [n()].
map_lam_bad_arity() ->
    lmap_n_to_n(
        fun (X, _) -> X end,
        [3]
    ).

-spec x4([n()], atom()) -> [n()].
x4(L, N) ->
    nmap(
        fun (Y) -> Y + N end,
        L
    ).

-spec x5([[n()]], n()) -> [[n()]].
x5(L, N) ->
    lmap(
        fun (Ns) ->
            nmap(
                fun (Y) -> Y + N end,
                Ns)
        end,
        L
    ).

-spec xmap(
    fun((A) -> none()),
    [A]) -> [none()].
xmap(F, XS) -> [F(X) || X <- XS].

-spec x6([none()], none()) -> [none()].
x6(L, B) ->
    xmap(fun (_) -> B end, L).

% helpers

-spec lmap_n_to_n(
    fun((n()) -> n()), [n()]) -> [n()].
lmap_n_to_n(F, Xs) -> lmap(F, Xs).

 -spec lmap(fun((A) -> B), [A]) -> [B].
 lmap(_F, _XS) -> [].

 -spec lmap_aa(fun((A) -> A), [A]) -> [A].
 lmap_aa(_F, _XS) -> [].

-spec lmap_a_num(
    fun((A) -> n()), [A]) -> [n()].
lmap_a_num(_F, _XS) -> [].

-spec nmap(
    fun((n()) -> n()),
    [n()]) -> [n()].
nmap(F, XS) -> [F(X) || X <- XS].

-spec test_dynamic() -> number().
test_dynamic() ->
    Res = (fun(X) -> X end)(2),
    Res.

-spec test_dynamic2() -> a | b.
test_dynamic2() ->
    Res = (
        fun
            (a) -> a;
            (b) -> b end
    )(a),
    Res.

-spec test_arity_1() -> ok.
test_arity_1() ->
    (fun () -> ok end)(1, 2, 3).

-spec test_arity_2() -> ok.
test_arity_2() ->
    (fun (_X) -> ok end)(1, 2, 3).

-spec test_arity_3() -> ok.
test_arity_3() ->
    (fun (_X) -> ok end)().

% function() is a system alias to fun()
-spec fun_id(function()) -> fun().
fun_id(F) -> F.

-spec named_1() ->
    fun((string()) -> atom()).
named_1() ->
    fun _Lta(X) -> list_to_atom(X) end.

-spec named_2() -> [number()].
named_2() ->
    Res = lists:map(
        fun _Map(X) -> X + 1 end,
        [1, 2, 3]
    ),
    Res.

-spec named_3() -> fun(() -> a).
named_3() ->
    fun F() -> F end.

-spec named_4() -> nok.
named_4() ->
    lists:map(
        fun Fib(N) when N < 2 -> 1;
            Fib(N) ->
                Fib(N - 2) + Fib(N - 1)
        end,
        [1, 2, 3]
    ),
    nok.

% Sound but not complete named fun logic:
% The pattern `F = 1 can never match,
% so `F` should be of type `none()`
% but we treat it as `number()`
-spec named_match() -> whatevs.
named_match() ->
    (fun F() -> F = 1, F end)().

% Understanding auto-import funs
-spec atoms_to_binaries(
    [atom()]
) -> [binary()].
atoms_to_binaries(As) ->
    lists:map(fun atom_to_binary/1, As).

-type int_result() :: fun(() ->
    {ok, integer()} | error).

-spec ints([int_result()]) ->
    [integer()].
ints([]) -> [];
ints([F | Fs]) ->
    case F() of
        {ok, I} -> [I | ints(Fs)];
        error -> ints(Fs)
    end.

-spec lambda_app() -> number().
lambda_app() ->
    Res = (fun
        Fib(N) when N < 2 ->
            1;
        Fib(N) ->
            Fib(N - 2) + Fib(N - 1)
    end)(4),
    Res.
    
-type getter(A) :: fun((atom()) -> A).

-spec getter1
    (getter(A)) -> getter(A).
getter1(F) ->
    fun(A) ->
        F(A)
    end.

-spec getter2
    (fun((atom()) -> A)) -> fun((atom()) -> A).
getter2(F) ->
    fun(A) ->
        F(A)
    end.

-spec getter3
    (fun((atom()) -> A)) -> ((fun((atom()) -> A)) | (fun ((atom()) -> A))).
getter3(F) ->
    fun(A) ->
        F(A)
    end.

-spec getter4_neg
    (fun((atom()) -> A)) -> ((fun((atom()) -> A)) | (fun ((term()) -> ok))).
getter4_neg(F) ->
    fun(A) ->
        F(A)
    end.
    
-spec getter5_neg
    (fun((atom()) -> A)) -> ((fun ((term()) -> ok)) | (fun((atom()) -> A))) .
getter5_neg(F) ->
    fun(A) ->
        F(A)
    end.
    
-spec getter6_neg
    (fun((atom()) -> _)) -> ((fun()) | (fun ((term()) -> ok))).
getter6_neg(F) ->
    fun(A) ->
        F(A)
    end.
    
-spec getter7
    (fun((atom()) -> A)) -> ok | (fun ((atom()) -> A)).
getter7(F) ->
    fun(A) ->
        F(A)
    end.
    
-spec getter8_neg
    (fun((atom()) -> _)) -> ok | getter(other).
getter8_neg(F) ->
    fun(A) ->
        F(A)
    end.
    
-spec getter9_neg
    (fun((atom()) -> A)) -> a | ((fun ((term(), term()) -> ok)) | (fun((term()) -> A))).
getter9_neg(F) ->
    fun(A) ->
        F(A)
    end.

-spec getter10
    (fun((atom()) -> A)) -> a | ((fun ((term(), term()) -> ok)) | (fun((atom()) -> A))).
getter10(F) ->
    fun(A) ->
        F(A)
    end.

-type stream(A) :: eos | {head, A}.

-spec id_predicate(fun((stream(A)) -> ok)) -> (fun((stream(A)) -> ok)).
id_predicate(P) -> P.

-spec wrap_id(fun((stream(A)) -> ok)) -> (fun((stream(A)) -> ok)).
wrap_id(P) -> id_predicate(P).

-spec hd_invariant([fun((A) -> A)]) -> (fun((A) -> A)).
hd_invariant([F | _]) -> F.

-spec test_invariant() -> (fun((atom()) -> atom())).
test_invariant() -> hd_invariant([]).
