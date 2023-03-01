%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(refine).

-compile([export_all, nowarn_export_all]).

-spec param1(_A, _B) ->
    none().
param1(X, X) -> X.

-spec param2({A, B}, {B, A}) ->
    {none(), none()}.
param2(X, X) -> X.

-spec maps(
    #{a := term()},
    #{atom() => atom()}
) -> none().
maps(M, M) -> M.

-spec funs1(
    fun((A) -> B),
    fun((B) -> A)
) -> fun((A | B) -> none()).
funs1(F, F) -> F.

-spec funs2(
    fun((A) -> atom()),
    fun((atom()) -> A)
) -> fun((A | atom()) -> none()).
funs2(F, F) -> F.

-spec tuple1
    ({number(), atom()}) -> none().
tuple1(X) ->
    X = {a, 1},
    X.

-spec tuple2(
    {number(), atom()},
    {atom(), number()}
) -> none().
tuple2(T1, T2) ->
    T1 = T2,
    T1.

-spec list1
    ([atom()], [number()]) -> [].
list1(L1, L2) ->
    L1 = L2,
    L1.

-spec shape1(
    #{id := atom()},
    #{id := number()}
) -> none().
shape1(S1, S2) ->
    S1 = S2,
    S1.

%% no support for
%% "elimination by negation"
-spec eliminate1
(binary() | string()) -> string().
eliminate1(A)
    when not is_binary(A) -> A;
eliminate1(_) -> "".

-spec eliminate2
    (atom() | pid() | string())
    -> string().
eliminate2(A) ->
    if
        not (is_pid(A) or is_atom(A))
            -> A;
        true
            -> ""
    end.

-spec list_trick_neg(_T, U) -> U.
list_trick_neg([X], _) -> X.

-spec use_list_refine() -> ok.
use_list_refine() ->
    _ = list_trick_neg([self()], 1) * 3,
    ok.

-spec tuple_trick_neg(_T, U) -> U.
tuple_trick_neg({X}, _) -> X.

-spec use_tuple_refine() -> ok.
use_tuple_refine() ->
    _ = tuple_trick_neg({self()}, 1) * 3,
    ok.

-spec fun_trick_neg(_T, U) -> U.
fun_trick_neg(X, _) when is_function(X, 0)
    -> X.

-spec use_fun_refine(fun(() -> ok))
        -> ok.
use_fun_refine(F) ->
    _ = fun_trick_neg(F, 1) * 3,
    ok.

-spec map_refine_tvar_neg
    (_V) -> none().
map_refine_tvar_neg
    (#{key := Val}) -> Val.

-spec binary_refine_tvar_neg
    (_V) -> {none(), none()}.
binary_refine_tvar_neg
    (<<X, Y/binary>>) -> {X, Y}.

-record(my_rec, {
    n :: integer(),
    a :: atom()
}).
-spec record_refine_tvar_neg
    (_V) -> {none(), none()}.
record_refine_tvar_neg
    (#my_rec{n = N, a = A}) -> {N, A}.

-spec refine_w_char(term())
        -> fun(() -> term()).
refine_w_char(X)
    when is_function(X, $\x00) ->
    X.

-type alist() :: [alist() | a].

-spec refine_alist1_neg(
    alist(), [a | b]
) -> pid().
refine_alist1_neg(X, X) -> X.

-spec refine_alist2(
    alist(), [a | b]
) -> [a].
refine_alist2(X, X) -> X.

-type tlist(T) :: [tlist(T) | T].

-spec refine_tlist1_neg(
    tlist(a), [a | b]
) -> pid().
refine_tlist1_neg(X, X) -> X.

-spec refine_tlist2(
    tlist(a), [a | b]
) -> [a].
refine_tlist2(X, X) -> X.

-type my_list()
:: nil
| {cons, nil | my_list()}.

-spec refine_recursive_neg(my_list())
        -> number().
refine_recursive_neg({cons, H}) ->
    Res = {cons, H},
    Res.

-spec exploit_recursive() -> ok.
exploit_recursive() ->
    refine_recursive_neg({cons, nil}) + 1,
    ok.

% returns a number
-spec opaque1_neg(
    gb_sets:set(atom())) ->
    pid().
opaque1_neg({X, _}) -> X.

% returns a tuple
-spec opaque2_neg(
    gb_sets:iter(atom())) ->
    pid().
opaque2_neg([X, _]) -> X.

% returns an atom
-spec opaque_3_neg(
    sets:set(atom())
) -> pid().
opaque_3_neg({
    X, _, _, _, _, _, _, _, _
}) -> X.

% returns a 2-tuple
-spec opaque_4_neg(
    gb_sets:set(atom()),
    {term(), term()}
) -> pid().
opaque_4_neg(X, X) -> X.

-spec record_as_tuple1_neg
(#my_rec{}, tuple()) -> none().
record_as_tuple1_neg(R, R) -> R.

-spec record_as_tuple2_neg
(tuple(), #my_rec{}) -> none().
record_as_tuple2_neg(R, R) -> R.

-spec record_as_tuple3
(#my_rec{}) -> tuple().
record_as_tuple3(R) -> R.

-spec record_as_tuple4
(#my_rec{}) -> {my_rec, term(), term()}.
record_as_tuple4(R) -> R.

-spec record_as_tuple5_neg
(tuple()) ->  #my_rec{}.
record_as_tuple5_neg(R) -> R.

-spec record_as_tuple6_neg
(#my_rec{}) -> {not_my_rec, term(), term()}.
record_as_tuple6_neg(R) -> R.

% subtyping "bad" record
-record(bad_rec, {
    i :: unknown:unknown()
}).
-spec record_as_tuple7_neg
(#bad_rec{}) -> {bad_rec, atom()}.
record_as_tuple7_neg(R) -> R.

-spec tuple_as_record1
({my_rec, integer(), atom()}) ->
#my_rec{}.
tuple_as_record1(R) -> R.

-spec tuple_as_record2_neg
  ({my_rec, atom(), integer()}) ->
    #my_rec{}.
tuple_as_record2_neg(R) -> R.

%% different arities -> X = none()
-spec tup_guard_record1
({my_rec, _}) -> term().
tup_guard_record1(X)
    when is_record(X, my_rec) ->
    X * 2.

-spec tup_guard_record2_neg
({my_rec, _, _}) -> term().
tup_guard_record2_neg(X)
    when is_record(X, my_rec) ->
    X * 2.

-spec record_guard_tup_neg
(#my_rec{}) -> term().
record_guard_tup_neg(X)
    when is_tuple(X) ->
    X * 2.

-spec ftt(fun((T) -> ok), T) -> T.
ftt(X, _) -> X.

-spec my_rec_to_ok(#my_rec{}) -> ok.
my_rec_to_ok(X) -> X.

-spec use1
(fun((#my_rec{}) -> ok),
{my_rec, integer(), atom()})
-> term().
use1(F, X) -> ftt(F, X).

-spec use2_neg
(fun((#my_rec{}) -> ok),
{my_rec, atom(), integer()})
-> term().
use2_neg(F, X) -> ftt(F, X).

-spec use3
(fun(({my_rec, integer(), atom()}) -> ok),
#my_rec{})
-> term().
use3(F, X) -> ftt(F, X).

-spec use4_neg
(fun(({my_rec, atom(), integer()}) -> ok),
#my_rec{})
-> term().
use4_neg(F, X) -> ftt(F, X).

-spec ttt1
({my_rec, integer(), T}, T) -> T.
ttt1(T, T) -> T.

-spec use5
(#my_rec{}) -> atom().
use5(X) -> ttt1(X, x).

-spec ttt2
({my_rec, atom(), T}, T) -> T.
ttt2(T, T) -> T.

-spec use6_neg
(#my_rec{}) -> atom().
use6_neg(X) -> ttt2(X, x).

-spec use7_neg
(#my_rec{}) -> integer().
use7_neg(X) -> ttt1(X, 1).

-spec deets(#my_rec{} | a)
-> {my_rec, _, _}.
deets(X) -> X.

-record(union_rec, {
    b :: undefined | binary(),
    s :: undefined | string()
}).

-spec rec_field_b_1_neg
    (#union_rec{}) -> binary().
rec_field_b_1_neg(#union_rec{b = B}) ->
    B.

-spec rec_field_b_2
    (#union_rec{}) -> binary().
rec_field_b_2(#union_rec{b = B})
    when is_binary(B) ->
    B.

-spec rec_field_b_3
    (#union_rec{}) -> undefined.
rec_field_b_3(#union_rec{b = B})
    when B == undefined ->
    B.

-spec rec_field_b_4
    (#union_rec{}) -> undefined.
rec_field_b_4(#union_rec{b = B})
    when B =:= undefined ->
    B.

-spec rec_field_b_5
    (#union_rec{}) -> binary().
rec_field_b_5(#union_rec{b = B})
    when B =/= undefined ->
    B.

-spec rec_field_b_6
    (#union_rec{}) -> binary().
rec_field_b_6(#union_rec{b = B})
    when B /= undefined ->
    B.

-spec rec_field_b_7
    (#union_rec{}) -> undefined.
rec_field_b_7(#union_rec{b = B})
    when undefined == B ->
    B.

-spec rec_field_b_8
    (#union_rec{}) -> undefined.
rec_field_b_8(#union_rec{b = B})
    when undefined =:= B ->
    B.

-spec rec_field_b_9
    (#union_rec{}) -> binary().
rec_field_b_9(#union_rec{b = B})
    when undefined =/= B ->
    B.

-spec rec_field_b_10
    (#union_rec{}) -> binary().
rec_field_b_10(#union_rec{b = B})
    when undefined /= B ->
    B.

-type shape() :: #{
    id => number(),
    name => string()
}.

-spec double_id_neg(shape()) -> shape().
double_id_neg(S) ->
    S#{id := 42}.

-spec double_id(shape()) -> shape().
double_id(#{id := Id} = S) ->
    S#{id := Id * 2};
double_id(S) ->
    S.

-record(any_rec, {field :: term()}).
-type tagged() :: {tag, integer()}.

-spec snd(#any_rec{} | tagged()) ->
    integer().
snd({tag, I}) -> I.

-spec use_private_record_neg() -> ok.
use_private_record_neg() ->
    Rec = records:mk_foo_pos(),
    {foo, _Id, Name} = Rec,
    eqwalizer:reveal_type(Name),
    Name.
