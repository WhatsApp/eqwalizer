%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(generic_fun_application).
-compile([export_all, nowarn_export_all]).

% References to Gradualizer are
% github.com/josefs/Gradualizer
% c83a9b49e2c43d7a63

% References to TypeScript are v4.1.5.
% See adapted examples playground:
% https://fburl.com/1dg2xiyb

% References to "TRacket"
% are Racket 7.8: #lang typed/racket
% Adapted examples paste:
% https://fburl.com/phabricator/4uygbfg7

-spec test00_pos() -> number().
test00_pos() -> fapply(fun id_num/1, 1).

% gradualizer (unsound) accepts this
-spec test01_neg(atom()) -> pid().
test01_neg(X) -> fapply(fun id/1, X).

% TRacket rejects this, msg is interesting
% TypeScript (expressive) accepts this
% Gradualizer (expressive) accepts this
-spec test02() -> number().
test02() -> fapply(fun id/1, 1).

% `lmap/2` is data-last
-spec test03_neg() -> ok.
test03_neg() ->
    lmap(fun atom_to_pid/1, [2, 4]).

% `mapl/2` is data-first
-spec test03b_neg() -> ok.
test03b_neg() ->
    mapl([2, 4], fun atom_to_pid/1).

-spec test04_neg() -> ok.
test04_neg() ->
    X = lmap(fun atom_to_pid/1, [2, 4]),
    X.

-spec test04b_neg() -> ok.
test04b_neg() ->
    X = mapl([2, 4], fun atom_to_pid/1),
    X.

-spec test05_pos(atom()) -> atom().
test05_pos(X) -> id(X).

-spec test06_pos() -> {v2, v3, v1}.
test06_pos() ->
    tuple3_rotate({v1, v2, v3}).

-spec test07_pos() -> h.
test07_pos() ->
    lhead([h]).

-spec test08_neg() -> h.
test08_neg() -> lhead(h).

-spec maptt(#{T => T}) -> T.
maptt(_) -> throw(not_implemented).

-spec test08_pos(
    #{number() => number()}
) -> number().
test08_pos(M) -> maptt(M).

-spec test09_neg(
    #{number() => number()}
) -> pid().
test09_neg(M) -> maptt(M).

-spec map_att(#{a => T}) -> T.
map_att(M) -> maps:get(a, M).

-spec test10_pos(
    #{a => pid()}
) -> pid().
test10_pos(M) ->
    map_att(M).

-spec test11_neg(
    #{true => pid()}
) -> pid().
test11_neg(M) ->
    map_att(M).

-spec test12_neg() -> pid().
test12_neg() ->
    map_att(3).

-spec test13_neg() -> ok.
test13_neg() -> lmap({}, [2, 4]).

-spec test13b_neg() -> ok.
test13b_neg() -> mapl([2, 4], {}).

-spec test14_pos(
    #{a => number()}
) -> number().
test14_pos(Shape) ->
    map_att(Shape).

% gradualizer (unsound) accepts this
-spec test15_neg(
    fun((#{atom() => T}) -> T),
    #{a => number()}
) -> pid().
test15_neg(F, Shape) ->
    F(Shape).

-spec test16_neg(
    fun(({X}) -> X),
    tuple()
) -> false.
test16_neg(F, Tuple) ->
    F(Tuple).

-spec test17_neg(fun()) -> ok.
test17_neg(F) ->
    lmap(F, [2, 4]).

-spec test17b_neg(fun()) -> ok.
test17b_neg(F) ->
    mapl([2, 4], F).

-type kv(K, V) :: #{K => V}.

-spec test18_pos(K, V) -> kv(K, V).
test18_pos(K, V) ->
    #{K => V}.

-spec test19_neg(a | b) -> a.
test19_neg(AB) ->
    ttt(AB, a).

% Gradualizer accepts this
% TypesScript rejects this
% TRacket requires annotations for `let`
-spec test20_pos() -> a | b.
test20_pos() ->
    X = ttt(a, b),
    X.

% Gradualizer (unsound) accepts this
-spec test21_neg() -> pid().
test21_neg() ->
    ttt(a, b).

% All checkers tested accept this
-spec test22_pos() -> a | b.
test22_pos() ->
    ttt(a, b).

% Gradualizer (unsound) accepts this
-spec test23_neg() -> a.
test23_neg() ->
    ttt(a, b).

% Gradualizer (unsound) accepts this
-spec test24_neg() -> b.
test24_neg() ->
    ttt(a, b).

-spec test25_pos() -> term().
test25_pos() ->
    l_item([2, a], b).

-spec test25b_pos() -> term().
test25b_pos() ->
    item_l(b, [2, a]).

-spec test26_pos() -> term().
test26_pos() ->
    l_item([2, a], a).

-spec test26b_pos() -> term().
test26b_pos() ->
    item_l(a, [2, a]).

-spec test27_pos(
    #{a => number()},
    #{atom() => number()}
) -> #{atom() => number()}.
test27_pos(Shape, Dict) ->
    X = ttt(Shape, Dict),
    X.

-spec test28_pos(
    #{atom() => number()},
    #{a => number()}
) -> #{atom() => number()}.
test28_pos(Dict, Shape) ->
    X = ttt(Dict, Shape),
    X.

-spec test29_pos(
    {#{atom() => number()}},
    {#{a => number()}}
) -> {#{atom() => number()}}.
test29_pos(DictTup, ShapeTup) ->
    X = ttt(DictTup, ShapeTup),
    X.

-spec test30_neg(
    {#{a => number()}},
    {#{atom() => number()}}
) -> {#{atom() => number()}}.
test30_neg(ShapeTup, DictTup) ->
    X = ttt(ShapeTup, DictTup),
    X.

-spec test31_pos(
    term(),
    none()
) -> term().
test31_pos(Any, None) ->
    X = ttt(Any, None),
    X.

-spec test32_pos(
    term(),
    pid()
) -> term().
test32_pos(Any, Pid) ->
    X = ttt(Any, Pid),
    X.

-spec test33_pos(
    none(),
    pid()
) -> term().
test33_pos(None, Pid) ->
    X = ttt(None, Pid),
    X.

-spec test34_pos(
    none()
) -> [pid()].
test34_pos(None) ->
    X=lmap(fun atom_to_pid/1, [None, a]),
    X.

-spec test35_pos() -> number().
test35_pos() ->
    (id(fun id_num/1))(3).

-spec test36_neg(fun(({T}) -> U), T) -> U.
test36_neg(F, X) -> F(X).

-spec test38_pos(
    fun((F) -> T),
    F
) -> T when F :: fun((Z, {Z}, [Z]) -> Z).
test38_pos(FT, F) -> FT(F).

-spec test39_neg
    (fun(() -> A), A, X)
    -> fun(() -> X).
test39_neg(F, _, _) ->
    fun0_id(F).

-spec test40_neg
    (fun((A) -> B), {A, B},  {X, Y})
    -> fun((X) -> Y).
test40_neg(F, _, _) ->
    fun0_id(F).

-spec test41_neg() -> atom().
test41_neg() ->
    (test39_neg(fun mk_number/0, 0, x))().

-spec
last_shall_be_first_neg
    (first, Last, Last) -> first.
last_shall_be_first_neg
(First, Last, Last) ->
    X = second_among_equals(First, Last),
    X.

-spec
atoms_shall_be_anything_neg
    (atom(), T, T) -> atom().
atoms_shall_be_anything_neg
(Atom, Foo, _) ->
    X = second_among_equals(Atom, Foo),
    X.

 -spec fun1_id_pos
 (fun((A) -> B)) -> fun((A) -> B).
fun1_id_pos(F) -> id(F).

 -spec fun1_id_pos_a
 (fun((A) -> A)) -> fun((A) -> A).
fun1_id_pos_a(F) -> id(F).

 -spec fun1_id_pos_z
 (fun((Z) -> Z)) -> fun((Z) -> Z).
fun1_id_pos_z(F) -> id(F).

-spec shape_order_pos() ->
    {pid(), number()}.
shape_order_pos() ->
    Pid = erlang:self(),
    expect_shape(
        #{u => 1, t => Pid}
    ).

-spec shape_order_neg() ->
    {pid(), number()}.
shape_order_neg() ->
    Pid = erlang:self(),
    expect_shape(
        #{t => 1, u => Pid}
    ).

-spec leak_c_neg
    (fun((A) -> B), A, B, C)
    -> C.
leak_c_neg(F, A, _B, _C) ->
    F(A).

-spec exploit_leak_c_neg() -> expect_pid.
 exploit_leak_c_neg() ->
    leak_c_neg(
        fun atom_to_pid/1,
        a,
        erlang:self(),
        expect_pid).

% Could be supported in future.
-spec test_fun_doubler1() -> number().
test_fun_doubler1() ->
    F = fun id/1,
    G = fun_doubler(
        F,
        fun fun1_id_pos_a/1),
    X = (G(G))(fun id/1),
    X(1).

% should type-check iff test_fun_doubler1
% type-checks. Only difference is
% type variable names in fun1_id_pos_X
-spec test_fun_doubler2() -> number().
test_fun_doubler2() ->
    F = fun id/1,
    G = fun_doubler(
        F,
        fun fun1_id_pos_z/1),
    X = (G(G))(fun id/1),
    X(1).

% helpers

-spec expect_shape (#{t := T, u := U}) ->
    {T, U}.
expect_shape(#{t := T, u:= U}) ->
    {T, U}.

-spec item_l(T, [T]) -> T.
item_l(X, _) -> X.

-spec l_item([T], T) -> T.
l_item(_, X) -> X.

-spec ttt(T, T) -> T.
ttt(X, _) -> X.

-spec second_among_equals
    (A, A)  -> A.
second_among_equals
(_, A) -> A.

-spec atom_to_pid(atom()) -> pid().
atom_to_pid(_) -> erlang:self().

-spec tuple3_rotate(
    {TT1, TT2, TT3}) -> {TT2, TT3, TT1}.
tuple3_rotate({V1, V2, V3}) ->
    {V2, V3, V1}.

-spec lhead([LH]) -> LH.
lhead([LH]) -> LH.

-spec id(Z) -> Z.
id(Z) -> Z.

-spec id_num(number()) -> number().
id_num(X) -> X.

-spec fapply(fun((T) -> U), T) -> U.
fapply(F, X) ->
    ZZ = F(X),
    ZZ.

-spec lmap(fun((A) -> B), [A]) -> [B].
lmap(_F, _XS) -> [].

-spec mapl([A], fun((A) -> B)) -> [B].
mapl(_XS, _F) -> [].

-spec fun0_id
    (fun(() -> X)) -> fun(() -> X).
fun0_id(F) ->
    F.

-spec mk_number() -> number().
mk_number() -> 1.

-spec fun_doubler(T, fun((T) -> T)) ->
    fun((T) -> T).
fun_doubler(_, X) -> X.

-spec num_and_t(number(), T) -> T.
num_and_t(_, X) -> X.

-spec shape_vars(#{a := T}, T)
    -> ok.
shape_vars(_, _) -> ok.

-spec test_dict1(#{atom() => pid()}) ->
    ok.
test_dict1(D) -> num_and_t(D, D).

-spec dict_vars
    (fun((#{{T} => T}) -> T), T)
    -> ok.
dict_vars(_, _) -> ok.

-spec test_dict2(T) -> T.
test_dict2(T) ->
    shape_vars(#{T => 3}, 3).

-spec test_dict3
    (fun((#{a => 1}) -> 1), 1) -> ok.
test_dict3(F, N) ->
    dict_vars(F, N).

-spec atat(atom()) -> atom().
atat(X) -> X.

-type tup(X, Y) :: {X, Y}.



-spec idtup(tup(T, U)) -> tup(T, U).
idtup(X) -> id(X).

-spec test_idtup() -> {a, b}.
test_idtup() ->
    X = idtup({a, b}),
    X.

-spec test_aliases_pos(
    fun(({X, Y}) -> {X, Y})) ->
    fun((tup(X, Y)) -> tup(X, Y)).
test_aliases_pos(F) -> F.

-spec test_aliases_neg(
     fun((a) -> b)
) -> fun((a) -> c).
test_aliases_neg(F) -> F.

-spec takesShape1(#{a := T}) -> T.
takesShape1(#{a := X}) -> X.

-spec shapes_neg() -> nok.
shapes_neg() ->
    takesShape1(#{}).

-spec one() -> 1.
one() -> 1.

-spec to_from(T) ->
    fun((T) -> b).
to_from(_) -> throw(not_implemented).

-spec test_to_from_pos() -> fun((a) -> b).
test_to_from_pos() ->
    to_from(a).

-spec test_to_from_neg() ->
    fun((term()) -> b).
test_to_from_neg() ->
    to_from(a).

-spec test_quanitifer_scope_neg(
    fun((T) -> T),
    number()
) -> number().
test_quanitifer_scope_neg(F, X) -> F(X).

-spec val_ty(
    #{number() => T}
) -> T.
val_ty(#{3 := T}) -> T.

-spec test_dict_ty() -> ok.
test_dict_ty() ->
    val_ty(#{3 => ok}).

-spec tuplify(T) -> {T, T}.
tuplify(T) -> {T, T}.

-spec test_tuplify() -> {ok, ok}.
test_tuplify() ->
    X = tuplify(ok),
    X.

-spec contravariant(T) -> fun((T) -> ok).
contravariant(_) ->
    throw(not_implemented).

-type invar(T) :: fun((T) -> T).
-type contravar(T) :: fun((T) -> ok).

-spec invariant(T) -> invar(T).
invariant(_) -> throw(not_implemented).

-spec test_contravariant_1() ->
    fun((term()) -> ok).
test_contravariant_1() ->
    X = contravariant(3),
    X.

-spec test_contravariant_2() ->
    fun((number() | pid()) -> ok).
test_contravariant_2() ->
    X = contravariant(3),
    X.

-spec test_contravariant_3(
    contravar(a | b)
) -> contravar(a).
test_contravariant_3(X) ->
    X.

-spec test_contravariant_4_neg(
    contravar(contravar(a | b))
) -> contravar(contravar(a)).
test_contravariant_4_neg(X) ->
    X.

-spec test_contravariant_5(
    contravar(contravar(a))
) -> contravar(contravar(a | b)).
test_contravariant_5(X) ->
    X.

-spec test_invariant_neg() ->
    fun((atom()) -> atom()).
test_invariant_neg() ->
    X = invariant(a),
    X.

-spec test_invariant_neg_2() ->
    invar(atom()).
test_invariant_neg_2() ->
    X = invariant(a),
    X.

-spec test_invariant_pos_1(term()) ->
    fun((term()) -> term()).
test_invariant_pos_1(Any) ->
    X = invariant(Any),
    X.

-spec test_invariant_pos_2() ->
    fun((a) -> a).
test_invariant_pos_2() ->
    invariant(a).

-spec test_invariant_pos_3() ->
    invar(a).
test_invariant_pos_3() ->
    a.

-spec arg_eqv(
    fun((T) -> pid()),
    fun((T) -> pid())
) -> T.
arg_eqv(_, _) -> throw(not_implemented).

-spec num_to_pid(number()) -> pid().
num_to_pid(_) -> erlang:self().

-spec any_to_pid(term()) -> pid().
any_to_pid(_) -> erlang:self().

-spec meets_1() -> anything.
meets_1() ->
    X = arg_eqv(
        fun num_to_pid/1,
        fun atom_to_pid/1),
    X.

-spec meets_2() -> term().
meets_2() ->
    X = arg_eqv(
        fun any_to_pid/1,
        fun num_to_pid/1),
    X.

-spec meets_3() -> term().
meets_3() ->
    arg_eqv(
        fun num_to_pid/1,
        fun any_to_pid/1).

-spec stuff_1([
    {a, #{f => a, ff := a}}
    | fun((a) -> b)
    | #{{} => 3}]
    | #{a => a}) ->
        pid().
stuff_1(0) -> erlang:self().

-spec stuff_2([
    {c, #{f => a, ff := b}}
    | fun((b) -> d)
    | #{{} => {}}]
    | #{b => b}) ->
        pid().
stuff_2(0) -> erlang:self().

-spec meets_4() -> ok.
meets_4() ->
    X = arg_eqv(
        fun stuff_1/1,
        fun stuff_2/1),
    X.

-spec arity_check_1() -> nok.
arity_check_1() ->
    F = fun id/1,
    F(1, 2),
    nok.

-spec arity_check_2() -> number().
arity_check_2() ->
    F = fun id/1,
    F(1, 2).

-spec funify(A) -> fun((A) -> A).
funify(_) -> erlang:error(no_lambdas).

-spec test_funify(
    fun((number()) -> number()))
    -> term().
test_funify(F) ->
    Res = funify(F),
    Res.

-spec to_shape(T) -> #{a := T, b => T}.
to_shape(X) -> #{a => X, b => X}.

-spec test_to_shape() ->
    #{a := number(), b => number()}.
test_to_shape() ->
    X = to_shape(1),
    X.

-spec map_t_t_t(#{T => T}) -> T.
map_t_t_t(_) -> throw(not_implemented).

-spec test_shape_as_dict() -> a.
test_shape_as_dict() ->
    map_t_t_t(#{a => a}).

-spec shape_width_neg() ->
    {pid(), number()}.
shape_width_neg() ->
    Pid = erlang:self(),
    expect_shape(
        #{u => 1, t => Pid, extra => 3}
    ).

-spec expect_shape_opt(
    #{t => T, u => U}
) -> {T, U} | undefined.
expect_shape_opt(#{t := T, u:= U}) ->
    {T, U};
expect_shape_opt(_) ->
    undefined.

-spec shape_width_pos() ->
    {pid(), number()} | undefined.
shape_width_pos() ->
    Pid = erlang:self(),
    expect_shape_opt(
        #{t => Pid}
    ).

-spec test_hygiene_pos([Elem], [Elem])
        -> [Elem].
test_hygiene_pos(_, List) ->
    % regression test:
    % funcs without ty vars should not
    % reset the var counter to 0
    _ = length(List),
    _ = length(List),
    lists:split(0, List),
    List.

-spec tat(T, a) -> T.
tat(X, a) -> X.

-spec apply2(fun((T, U) -> T), T, U) -> T.
apply2(F, X, Y) -> F(X, Y).

-spec test_apply2() -> number().
test_apply2() -> apply2(fun tat/2, 1, 1).

-spec invar(T, T) -> fun((T) -> T).
invar(_T, U) ->
    fun(_X) -> U end.

-spec test_invar1() -> fun((a) -> a).
test_invar1() ->
    invar(a, a).

-spec test_invar2(a | b) ->
    fun((a | b) -> a | b).
test_invar2(AB) ->
    invar(a, AB).

-spec test_invar3_neg() ->
    fun((a | b) -> a | b).
test_invar3_neg() ->
    invar(a, a).

-spec fun_with_tuple
    (tuple(), [A]) -> {tuple(), [A]}.
fun_with_tuple(T, L) -> {T, L}.

-spec use_fun_with_tuple_1() -> ok.
use_fun_with_tuple_1() ->
    _ = fun_with_tuple({a, b}, []),
    ok.

-spec use_fun_with_tuple_2_neg() -> ok.
use_fun_with_tuple_2_neg() ->
    _ = fun_with_tuple({a, b}, {}),
    ok.
