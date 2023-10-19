%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(misc).

%% The majority of these tests are just
%% to get some code coverage.
%% Many of them do not have any special meaning.

-compile([export_all, nowarn_export_all]).

-import(misc_lib, [boolean_id/1]).
-export_type([
    o/0,
    o_rec/0,
    o_cycle/0,
    x_cycle/0,
    int_result_o/0,
    my_record_hidden/0,
    rec_w_opaque_field/0
]).

-record(my_record, {
    str :: string(),
    atom :: atom()
}).

-type my_record_hidden() ::
    #my_record{}.

-spec atomize(
    my_record_hidden()
) -> atom().
atomize(#my_record{atom = A}) ->
    A.

-spec test01(term()) -> number().
test01(-4 = X) -> X.

-spec test02(term()) -> number().
test02(6 / 3 = Y) -> Y.

-spec test03_neg(term()) -> atom().
test03_neg(-4 = X) -> X.

-spec test04_neg(term()) -> atom().
test04_neg(6 / 3 = Y) -> Y.

-spec test05_neg(term()) -> term().
test05_neg(X) -> + X.

-spec test06_neg(term()) -> atom().
test06_neg(X) when is_number(X) -> + X.

-spec test07(boolean()) -> atom().
test07(X) ->
    not X.

-spec test08_neg(boolean()) -> number().
test08_neg(X) ->
    not X.

-spec test09(term(), term()) -> number().
test09(X, Y) when
    is_number(X),
    is_number(Y) -> X + Y.

-spec test10_neg (term(), term())
               -> number().
test10_neg(X, Y) when
    is_number(X) -> X + Y.

-spec test11_neg (number(), number())
        -> atom().
test11_neg(X, Y) -> X + Y.

-spec test12_neg (atom())
        -> number().
test12_neg(X) -> -X.

-spec test13_neg (term(), term())
        -> boolean().
test13_neg(X, Y) -> X or Y.

-spec test14 (boolean(), atom())
        -> atom().
test14(X, Y) -> X orelse Y.

-spec test15 (boolean(), atom())
        -> atom().
test15(X, Y) -> X andalso Y.

-spec test16_neg (boolean(), boolean())
        -> number().
test16_neg(X, Y) -> X or Y.

-spec test17_neg (boolean(), atom())
        -> boolean().
test17_neg(X, Y) -> X or Y.

-spec test18_neg (boolean(), atom())
        -> number().
test18_neg(X, Y) -> X orelse Y.

-spec test19_neg (boolean(), number())
        -> {number()}.
test19_neg(X, Y) -> X andalso Y.

-spec test20_neg(number()) -> [atom()].
test20_neg(X) -> [- X].

-spec test21_neg (number(), number())
              -> [atom()].
test21_neg(X, Y) -> [X / Y].

-spec test22_neg (boolean())
        -> [number()].
test22_neg(X) -> [not X].

-spec test23_neg (boolean(), boolean())
        -> [number()].
test23_neg(X, Y) -> [X or Y].

-spec test24_neg (boolean(), atom())
        -> [number()].
test24_neg(X, Y) -> [X orelse Y].

-spec test25_pos
  ([boolean()]) -> boolean() | undefined.
test25_pos([]) -> undefined;
test25_pos([X]) -> X;
test25_pos([_|T]) -> test25_pos(T).

-spec test26_pos(X) -> X.
test26_pos(X) -> X.

-spec test27_pos(X) -> [X].
test27_pos(X) -> [X].

-spec test28_neg(atom()) -> boolean().
test28_neg(B) -> boolean_id(B).

-spec test29_neg() -> [].
test29_neg() -> [true, false].

-spec test30_neg() -> atom.
test30_neg() -> 1.

-spec test31_neg() -> atom.
test31_neg() -> fun test30_neg/0.

-spec test32_neg() -> atom.
test32_neg() -> fun misc_lib:boolean_id/1.

-spec test33_pos(a | b, b | c) -> b.
test33_pos(AB, BC) ->
    case AB of BC -> BC end.

-spec test34_pos({a | b}, {b | c}) -> {b}.
test34_pos(AB, BC) ->
    case AB of BC -> BC end.

-spec test35_pos([a | b], [b | c]) -> [b].
test35_pos(AB, BC) ->
    case AB of BC -> BC end.

-spec test36_pos(F1, F2) -> F3
    when F1 :: fun((a) -> a | z),
         F2 :: fun((b) -> b | z),
         F3 :: fun((a | b) -> z).
test36_pos(F1, F2) ->
    case F1 of F2 -> F2 end.

-spec test37_pos (term(), term())
              -> {number(), number()}.
test37_pos(X, Y) when X + Y > 0 -> {X, Y}.

-spec test38_pos (term())
        -> {number()}.
test38_pos(X) when bnot X > 0 -> {X}.

-spec test39_pos
    ({atom(), atom()} | number())
  -> {atom(), atom()}.
test39_pos({X, Y}) -> {X, Y};
test39_pos(_) -> {a, b}.

-spec test40_pos
    (number()) -> {none(), none()}.
test40_pos({X, Y}) -> {X, Y}.

-spec test41_pos
    (number()) -> {none(), [none()]}.
test41_pos([X | Y]) -> {X, Y}.

-spec test42_pos
    (term()) -> {term(), [term()]}.
test42_pos([X | Y]) -> {X, Y}.

-spec test43_pos
    (term()) -> none().
test43_pos([_ | Y]) when Y + 1 > 0 -> Y.

-spec test44_pos(term()) -> term().
test44_pos(X) ->
    case X of
        Z -> ok
    end,
    Z.

-spec test45_pos(term()) -> atom().
test45_pos(X) ->
    case X of
        _ -> Z = ok
    end,
    Z.

-spec test46_neg() -> number().
test46_neg() -> false.

-spec test47_neg() -> number().
test47_neg() -> [].

-spec test48_neg() -> atom().
test48_neg() -> test47_neg().

-spec test49_neg() -> atom().
test49_neg() -> misc:test47_neg().

-spec test50_pos() -> number().
test50_pos() -> misc:test47_neg().

-spec test51_pos() -> number().
test51_pos() -> test47_neg().

-spec test52_pos() ->
      fun(() -> number()).
test52_pos() ->
    fun test47_neg/0.

-spec test53_pos() ->
    fun(() -> number()).
test53_pos() ->
    fun misc:test47_neg/0.

test54_unspecced() -> ok.

-spec test55_neg() ->
    fun(() -> number()).
test55_neg() ->
    fun misc:test54_unspecced/0.

-spec test56_neg() ->
    fun(() -> number()).
test56_neg() ->
    fun test54_unspecced/0.

-spec test57_pos() -> {atom(), number()}.
test57_pos() ->
    A = atom,
    N = 1,
    {A, N}.

-spec test58_pos
    (boolean(), atom(), number()) ->
    atom() | number().
test58_pos(B, A, N) ->
    Res =
    begin
    if B -> A; true -> N end
    end,
    Res.

-spec test59_pos() -> number().
test59_pos() ->
    X = misc:test47_neg(),
    X.

-spec test60_pos() -> number().
test60_pos() ->
    X = test47_neg(),
    X.

-spec test61_neg() -> number().
test61_neg() ->
    X = misc:test54_unspecced(),
    X.

-spec test62_neg() -> number().
test62_neg() ->
    X = test54_unspecced(),
    X.

-spec test63_pos() ->
    fun(() -> number()).
test63_pos() ->
    Fun = fun misc:test47_neg/0,
    Fun.

-spec test64_pos() ->
    fun(() -> number()).
test64_pos() ->
    Fun = fun test47_neg/0,
    Fun.

-spec test65_pos() -> [term()].
test65_pos() ->
    L = [],
    L.

-spec test66_pos() ->
    {atom(), number()}.
test66_pos() ->
    Result =
        begin
            X = a,
            Y = 1,
            {X, Y}
        end,
    Result.

-spec test67_pos
    (atom(), number()) ->
    [atom() | number()].
test67_pos(A, N) ->
    Result =
        begin
            Head = A,
            Tail = [N],
            [Head | Tail]
        end,
    Result.

-spec test68_pos({atom()}) -> none().
test68_pos({E, _}) -> E.

-spec test69_pos
    ([atom()] | [number()]) ->
    [atom() | number()].
test69_pos([H | T]) -> [H | T].

-spec test70_neg() ->
    [atom() | number()].
test70_neg() ->
    catch test69_pos([atom]).

-spec test71_neg() ->
    [atom() | number()].
test71_neg() ->
    catch test69_pos(atom).

-spec test72_neg() ->
    term().
test72_neg() ->
    catch test69_pos(atom).

-spec test73_pos() ->
    term().
test73_pos() ->
    catch test69_pos([atom]).

-spec test74_pos() ->
    term().
test74_pos() ->
    X = (catch test69_pos([atom])),
    X.

-spec test75_pos() -> ok.
test75_pos() ->
    try
        ok
    of
        ok -> ok
    after
        nook
    end.

-spec test76_pos(term()) -> atom().
test76_pos(A) when is_atom(A) -> A;
test76_pos(_) ->
    erlang:throw({error, not_an_atom}).

-spec test77_neg(term()) -> atom().
test77_neg(A) -> catch(test76_pos(A)).

-spec test78_pos() -> term().
test78_pos() ->
    receive
        X -> X
    end.

-spec test79_neg() -> atom().
test79_neg() ->
    receive
        X -> X
    end.

-spec test80_neg(term()) -> atom().
test80_neg(Timeout) ->
    receive
        X -> X
    after Timeout ->
        default
    end.

-spec test81_neg(term()) -> atom().
test81_neg(Timeout) ->
    receive
        X when is_atom(X) -> X
    after Timeout ->
        default
    end.

-spec test82_pos() -> atom().
test82_pos() ->
    A = atom,
    Msg = receive
        A -> A
    after 10 ->
        default
    end,
    {Msg}.

-spec test83_pos(integer()) -> atom().
test83_pos(Timeout) ->
    receive
        _ -> atom
    after Timeout ->
        default
    end.

-spec test84_pos() -> number().
test84_pos() ->
    begin
        Z = 2
    end,
    Z.

-spec test85() -> atom().
test85() ->
    receive
        A when is_atom(A) -> A;
        X when is_number(X) -> number
    end. % scroll down for test86

-spec unzip1_neg([{integer(),atom()}])
        -> {[atom()], [integer()]}.
unzip1_neg([]) -> {[],[]};
unzip1_neg([{H1, H2}|T]) ->
    {T1, T2} = unzip1_neg(T),
    {[H1|T1], [H2|T2]}.

-spec unzip2_neg([{integer(),atom()}])
    -> {[atom()] | [integer()],
        [atom()] | [integer()]}.
unzip2_neg([]) -> {[],[]};
unzip2_neg([{H1, H2}|T]) ->
    {T1, T2} = unzip2_neg(T),
    {[H1|T1], [H2|T2]}.

-spec wrong_list_neg() -> term().
wrong_list_neg() ->
    X = [1 | 2],
    X.

-spec comp01(term(), term()) -> boolean().
comp01(X, Y) -> X < Y.

-spec comp01_neg(term(), term()) -> pid().
comp01_neg(X, Y) -> X < Y.

-spec comp02(term(), term()) -> boolean().
comp02(X, Y) ->
    Res = X < Y,
    Res.

-spec comp02_neg(term(), term()) -> pid().
comp02_neg(X, Y) ->
    Res = X < Y,
    Res.

-spec send01(pid() | port(), term()) ->
    term().
send01(Where, What) ->
    Where ! What.
%% - erlang:dst() is OTP specific. - OTP23 and OTP24 are different.
%%-spec send02_neg(term(), term()) ->
%%    term().
%%send02_neg(Where, What) ->
%%    Where ! What.

-spec send03_neg(pid(), term()) ->
    pid().
send03_neg(Where, What) ->
    Where ! What.

-record(str_box, {str :: string()}).

-spec mk_str_box(string()) -> #str_box{}.
mk_str_box(Str) -> #str_box{str = Str}.

-spec mk_str_box_neg
    (atom()) -> #str_box{}.
mk_str_box_neg(Str) ->
    #str_box{str = Str}.

-spec mk_str_box() -> #str_box{}.
mk_str_box() -> #str_box{str = "Str"}.

-spec string_neg() -> atom().
string_neg() -> "str".

% this is not very correct
% since binary_to_list returns [byte()].
-spec get_str(string() | binary()) ->
    string().
get_str(S) when is_list(S) -> S;
get_str(B) when is_binary(B) ->
    erlang:binary_to_list(B).

-spec get_atom(string() | binary()) ->
    atom().
get_atom(B) when is_binary(B) ->
    erlang:binary_to_atom(B);
get_atom(S) when is_list(S) ->
    erlang:list_to_atom(S).

-spec str_pat(string() | binary()) ->
    string().
str_pat(SB = "some_string") -> SB.

-spec atom_not_string_neg() ->
    atom().
atom_not_string_neg() ->
    Atom = "atom",
    Atom.

-spec ans(Atoms, Numbers) -> ANs
    when
        Atoms :: [atom()],
        Numbers :: [number()],
        AN :: atom() | number(),
        ANs :: [AN].
ans(Atoms, Numbers) ->
    Atoms ++ Numbers.

-spec ans_neg(Atoms, [binary()]) -> ANs
    when
    Atoms :: [atom()],
    AN :: atom() | number(),
    ANs :: [AN].
ans_neg(Atoms, Numbers) ->
    Atoms ++ Numbers.

-spec ans2(Atoms, [binary()]) -> ANs
    when
    Atoms :: [atom()],
    AN :: atom() | number(),
    ANs :: [AN].
ans2(Atoms, Numbers) ->
    Atoms -- Numbers.

-spec ans3(Atoms, [term()]) -> Atoms
    when
    Atoms :: [atom()].
ans3(Atoms, Anys) ->
    Atoms -- Anys.

-spec ans3_neg(Atoms, term()) -> Atoms
    when
    Atoms :: [atom()].
ans3_neg(Atoms, Anys) ->
    Atoms -- Anys.

-spec ans4_neg(term(), Atoms) -> Atoms
    when
    Atoms :: [atom()].
ans4_neg(Anys, Atoms) ->
    Anys ++ Atoms.

-spec slice_tuple_atom
    (tuple()) -> atom().
slice_tuple_atom({El})
    when is_atom(El) -> El;
slice_tuple_atom({_, El})
    when is_atom(El) -> El.

-spec slice_tuple_neg
    (tuple()) -> none().
slice_tuple_neg({_, El}) ->
    El.

-spec start([A]) -> [A].
start([] ++ X) -> X.

-spec start_neg([A]) -> [[A]].
start_neg([] ++ X) -> X.

-spec ss
    (binary() | string()) -> string().
ss("s" ++ S) -> S.

-spec ss1
    (binary() | string()) -> string().
ss1("" ++ S) -> S.

-spec ss2_neg
    ([atom() | char()]) -> string().
ss2_neg("" ++ S) -> S.

-spec ss3
    (binary()) -> none().
ss3("b" ++ S) -> S.

-spec ss4
    ([atom()] | string()
    , [pid()] | string()
    , term()) ->
    string().
ss4(Same, Same, Test) ->
    case Test of
        "prefix" ++ Same -> Same
    end.

-spec catch_env_neg_1(term()) -> number().
catch_env_neg_1(X) ->
    _ = (catch case X of
            X when is_number(X) -> X
        end),
    X.

-spec catch_env_neg_2(term()) -> number().
catch_env_neg_2(X) ->
    catch case X of
            X when is_number(X) -> X
        end.

-spec badspec(_) -> #{
    atom() := atom(),
    integer() := integer()
}.
badspec(_) -> ok.

-spec test86_neg() -> term().
test86_neg() ->
    catch lists:map({}, {}).

-spec test87_neg() -> term().
test87_neg() ->
    X = (catch lists:map({}, {})),
    X.

-spec test88_neg() -> term().
test88_neg() -> fun badspec/1.

% *can* reference skipped specs from other modules
-spec test89_neg() -> term().
test89_neg() ->
    fun skip:bad_mixed_update/1.

% cannot reference invalid specs from other modules
-spec test90_neg() -> term().
test90_neg() ->
fun generics_with_unions:test_03_neg/3.

-spec list_with_vars_neg
(number(), number()) -> pid().
list_with_vars_neg(X, Y) ->
    [X, 1, 2, 3 | Y].

-spec improper_list1
    (number()) -> pid().
improper_list1(X) ->
    [X, 1, 2, 3 | an_atom].

-spec improper_list2
    (number()) -> pid().
improper_list2(X) ->
    Res = [X, 1, 2, 3 | an_atom],
    Res.

-spec not_list1_neg(atom()) -> none().
not_list1_neg("atom" ++ A) -> A.

-spec not_list2_neg
    (atom(), [atom()]) -> none().
not_list2_neg
    (A, [_ | A]) -> A.

-spec not_list3_neg
    (atom(), string()) -> none().
not_list3_neg
    (A, "atom" ++ A) -> A.

-spec catch_me(term()) -> term().
catch_me(X) -> catch X.

-spec receive_timeout1(
    integer()
) -> atom().
receive_timeout1(Timeout) ->
    receive after Timeout -> timeout end.

-spec receive_timeout2(
    integer()
) -> {number(), atom()}.
receive_timeout2(Timeout) ->
    Res =
        receive
            after Timeout ->
            Z = 1,
            timeout
        end,
    {Z, Res}.

-spec test_flatten1() -> [term()].
test_flatten1() ->
    lists:flatten([1, 2, [3]]).

-spec test_flatten2() -> [term()].
test_flatten2() ->
    lists:flatten([1, 2, [3]], []).

-spec test91_neg(none()) ->
    {ok, ok}.
test91_neg(None) ->
    Res = {None, err},
    Res.


-type stuff() :: stuff1 | stuff2.
-type v0_op() :: v0_op1 | v0_op2.
-type v1_op() :: v1_op1 | v1_op2
    | stuff() | v0_op().
-type v2_op() :: v2_op | v1_op().

-spec v2_to_v1_neg(v2_op()) -> v1_op().
v2_to_v1_neg(X) ->
    X.
-opaque o() :: {ok}.

-record(rec, {}).
-opaque o_rec() :: #rec{}.

-record(rec_w_opaque_field, {
  o :: o()
}).
-type rec_w_opaque_field() ::
  #rec_w_opaque_field{}.

-spec to_rec_w_opaque_field() ->
  rec_w_opaque_field().
to_rec_w_opaque_field() ->
  #rec_w_opaque_field{
    o = {ok}
  }.

-opaque o_cycle() :: opaque:o_cycle().

-spec use_o_cycle1(misc:o_cycle()) ->
    opaque:o_cycle().
use_o_cycle1(X) -> X.

-spec use_o_cycle2(opaque:o_cycle()) ->
    misc:o_cycle().
use_o_cycle2(X) -> X.

-type contravariant(T)
:: fun((T) -> ok).

-spec use_contra_neg(fun((ok) -> ok))
        -> contravariant(ok).
use_contra_neg(X) -> X.

-spec use_spec_from_behaviour() ->
    ok | stop.
use_spec_from_behaviour() ->
    {Res, _} = callbacks1_pos:init({}),
    Res.

-type x_cycle() ::
    recursive_aliases:x_cycle().

-opaque int_result_o() ::
fun(() -> {ok, integer()} | error).

-spec use_opaque_w_bad_arg_neg(
    opaque:opair(a, x:y())) ->
    opaque:opair(a, x:y()).
use_opaque_w_bad_arg_neg(X) ->
    X.

-type set() :: [].
-spec names(set()) -> sets:set().
names(X) -> X.

-spec use_abs(integer()) -> integer().
use_abs(I) -> abs(I).
- spec module_ty_1() -> module().
module_ty_1() ->
    refine.

-spec module_ty_2_neg() -> module().
module_ty_2_neg() ->
    nonexistent_module.

-spec module_ty_3_neg(
    atom()
) -> module().
module_ty_3_neg(Atom) ->
    Atom.

-spec module_ty_4(
    mfa()
) -> module().
module_ty_4({Mod, _F, _A}) ->
    Mod.

-spec module_ty_5(
    module()
) -> atom().
module_ty_5(Mod) ->
    Mod.

-spec invariant(T, fun((T) -> ok)) -> ok.
invariant(_, _) ->
    ok.

-spec mod_to_ok(module()) -> ok.
mod_to_ok(_) ->
    ok.

-spec module_ty_6(module()) -> ok.
module_ty_6(Mod) ->
    invariant(
        Mod,
        fun mod_to_ok/1
    ).

-spec module_ty_7() ->
    {ok, module()}.
module_ty_7() ->
    {ok, lists}.

-spec module_ty_8_neg() ->
    {ok, module(), pid()}.
module_ty_8_neg() ->
    {ok, lists, 1.0}.

-spec module_ty_9_neg() ->
    {ok, module()}.
module_ty_9_neg() ->
    {ok, nonexistent}.

-spec module_ty_10_neg(module()) ->
    wrong_ret.
module_ty_10_neg(Mod) ->
    Mod.

% accepted because `node()` is
% an alias for atom()
-spec module_ty_11(module()) ->
    node().
module_ty_11(Mod) ->
    Mod.

-spec arity_1(arity()) -> number().
arity_1(Arity) ->
    Arity.

-spec arity_2_neg(arity()) -> pid().
arity_2_neg(Arity) ->
    Arity.

-spec module_1(module()) -> atom().
module_1(Mod) ->
    Mod.

-spec node_1(node()) -> atom().
node_1(Node) ->
    Node.

-spec node_2_neg(node()) -> pid().
node_2_neg(Node) ->
    Node.

-type tab() :: atom() | reference().

-spec foldl_ets_table_raw1
    (ets:tab(), atom()) -> atom().
foldl_ets_table_raw1(EtsTable, Acc) ->
    foldl_ets(EtsTable, Acc).

-spec foldl_ets
    (ets:tab(), atom()) -> atom().
foldl_ets(_, T) -> T.

-spec foldl_ets_table_raw1_gen
    (ets:tab(), T) -> T.
foldl_ets_table_raw1_gen(EtsTable, Acc) ->
    foldl_ets1_gen(EtsTable, Acc).

-spec foldl_ets1_gen
    (ets:tab(), T) -> T.
foldl_ets1_gen(_, T) -> T.

-spec foldl_ets_table_raw2_gen
    (ets:tid(), T) -> T.
foldl_ets_table_raw2_gen(EtsTable, Acc) ->
    foldl_ets_gen2(EtsTable, Acc).

-spec foldl_ets_gen2
    (ets:tid(), T) -> T.
foldl_ets_gen2(_, T) -> T.

-spec foldl_ets_table_raw3_gen
    (tab(), T) -> T.
foldl_ets_table_raw3_gen(EtsTable, Acc) ->
    foldl_ets_gen3(EtsTable, Acc).

-spec foldl_ets_gen3
    (tab(), T) -> T.
foldl_ets_gen3(_, T) -> T.

-spec iovec_neg()
    -> erlang:iovec().
iovec_neg() -> {<<>>}.

-spec repeated_vars() -> ok.
repeated_vars() ->
    case {ok, ok} of
        {X, X} -> X
    end.

-spec repeated_vars2() -> ok.
repeated_vars2() ->
    Res = case {ok, ok} of
        {X, X} -> X
    end,
    Res.

-spec empty_string() -> [].
empty_string() -> "".

-spec n_a_neg(erlang:number())
    -> erlang:atom().
n_a_neg(N) -> N.

% -- types from erlang --

-spec my_priorities_pos()
        -> [erlang:priority_level()].
my_priorities_pos() ->
    MyPriorities =
        [low, normal, high, max],
    MyPriorities.

-spec my_priorities_neg()
        -> [erlang:priority_level()].
my_priorities_neg() ->
    MyPriorities =
        ['MS', 'MM', 'MA', 'EE', 'GE'],
    MyPriorities.

-spec timestamp_neg(
    erlang:timestamp()
) -> atom().
timestamp_neg(X) ->
    X.

-spec orelse1(boolean(), pid())
        -> true | pid().
orelse1(Flag, Pid) ->
    Flag orelse Pid.

-spec orelse2(boolean(), pid())
        -> true | pid().
orelse2(Flag, Pid) ->
    Res = (Flag orelse Pid),
    Res.

-spec orelse3_neg(atom(), pid())
        -> true | pid().
orelse3_neg(Flag, Pid) ->
    Res = (Flag orelse Pid),
    Res.

-spec orelse4(
    undefined | fun(() -> boolean())
) -> boolean().
orelse4(Validator) ->
    Res =
        Validator =:= undefined
            orelse Validator(),
    Res.

-spec orelse5_neg(
    undefined | fun(() -> boolean())
) -> boolean().
orelse5_neg(Validator) ->
    Res =
        Validator =/= undefined
            orelse Validator(),
    Res.

-spec non_exported_id(any_fun_type:f1()) -> any_fun_type:f1().
non_exported_id(F) -> F.

-type non_exported_id_t() :: any_fun_type:f1().
