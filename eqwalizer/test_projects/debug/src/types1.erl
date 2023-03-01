%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(types1).

-compile([export_all, nowarn_export_all]).

-export_type([
    my_opaque/0,
    my_record/0,
    my_pair/2,
    my_any/0,
    my_none/0,
    my_pid/0,
    my_port/0,
    my_reference/0,
    my_nil1/0,
    my_nil2/0,
    my_atom/0,
    my_atom_literal/0,
    my_bitstring1/0,
    my_bitstring2/0,
    my_bitstring3/0,
    my_fun_any/0,
    my_fun_any_arg/0,
    my_fun/0,
    my_float/0,
    my_integer/0,
    my_integer_literal/0,
    my_integer_range/0,
    my_list/1,
    my_nonempty_improper_list2/2,
    my_nonempty_list1/1,
    my_tuple/0,
    my_term/0,
    my_binary/0,
    my_bitstring/0,
    my_boolean/0,
    my_byte/0,
    my_char/0,
    my_nil/0,
    my_number/0,
    my_list/0,
    my_maybe_improper_list0/0,
    my_maybe_improper_list2/2,
    my_nonempty_list0/0,
    my_string/0,
    my_nonempty_string/0,
    my_iodata/0,
    my_iolist/0,
    my_map/0,
    my_function/0,
    my_module/0,
    my_mfa/0,
    my_arity/0,
    my_identifier/0,
    my_node/0,
    my_timeout/0,
    my_no_return/0,
    my_non_neg_integer/0,
    my_pos_integer/0,
    my_neg_integer/0,
    atom_or_number/0,
    op_type1/0,
    op_type2/0,
    char_lit/0,
    prop_map/0,
    my_unary_fun/2,
    my_boolean_unary_fun/0,
    my_booleans/0,
    any_pair/0,
    bad_map1/0,
    bad_map2/0,
    empty_map/0,
    int_dict/0,
    single_shape1/0,
    single_shape2/0,
    e_list1/0,
    e_list2/0,
    e_list3/0,
    foo_rec/0
]).

-record(test_record, {}).

-opaque my_opaque() :: atom().

-type my_record() ::
    #test_record{}.

-type my_pair(A, B) ::
    {A, B}.

-type my_any() ::
    term().

-type my_none() ::
    none().

-type my_pid() ::
    pid().

-type my_port() ::
    port().

-type my_reference() ::
    reference().

-type my_nil1() ::
    [].

-type my_nil2() ::
    nil().

-type my_atom() ::
    atom().

-type my_atom_literal() ::
    ok.

-type my_bitstring1() ::
    <<>>.

-type my_bitstring2() ::
    <<_:2>>.

-type my_bitstring3() ::
    <<_:_*2>>.

%%% functions

-type my_fun_any() ::
    fun().

-type my_fun_any_arg() ::
    fun((...) -> ok).

-type my_fun() ::
    fun((number()) -> number()).

%% NUMBERS

-type my_float() ::
    float().

-type my_integer() ::
    integer().

-type my_integer_literal() ::
    1.

-type my_integer_range() ::
    [0..33].

%% TODO - Ops

-type my_list(A) ::
    list(A).

-type
my_maybe_improper_list2(T1, T2) ::
    maybe_improper_list(T1, T2).

-type
my_nonempty_improper_list2(T1, T2) ::
    nonempty_improper_list(T1, T2).

-type my_nonempty_list1(T) ::
    nonempty_list(T).

-type my_tuple() ::
    tuple().

%%%%%%%%

-type my_term() ::
    term().

-type my_binary() ::
    binary().

-type my_bitstring() ::
    binary().

-type my_boolean() ::
    boolean().

-type my_byte() ::
    byte().

-type my_char()  ::
    char().

-type my_nil() ::
    nil().

-type my_number() ::
    number().

-type my_list() ::
    list().

-type my_maybe_improper_list0() ::
    maybe_improper_list().

-type my_nonempty_list0() ::
    nonempty_list().

-type my_string() ::
    string().

-type my_nonempty_string() ::
    nonempty_string().

-type my_iodata() ::
    iodata().

-type my_iolist() ::
    iolist().

-type my_map() ::
    map().

-type my_function() ::
    function().

-type my_module() ::
    module().

-type my_mfa() ::
    mfa().

-type my_arity() ::
    arity().

-type my_identifier() ::
    identifier().

-type my_node() ::
    node().

-type my_timeout() ::
    timeout().

-type my_no_return() ::
    no_return().

-type my_non_neg_integer() ::
    non_neg_integer().

-type my_pos_integer() ::
    pos_integer().

-type my_neg_integer() ::
    neg_integer().

%% the same logic for types in specs

-spec
number_id(number()) ->
          number().
number_id(X) -> X.

-spec
atom_id(Atom :: atom()) -> atom().
atom_id(A) -> A.

-spec
record_id(#test_record{}) ->
          #test_record{}.
record_id(X) -> X.

-spec
my_number_id(my_number()) ->
             my_number().
my_number_id(N) -> N.

-type
atom_or_number() :: atom() | number().

-type op_type1() :: -1.
-type op_type2() :: 1 + 2.

-type char_lit() :: $a.

-type prop_map() ::
    #{a := number(), b => atom()}.

-type my_booleans() :: my_list(boolean()).

-type my_unary_fun(A, B) ::
    fun((A) -> B).
-type my_boolean_unary_fun() ::
    my_unary_fun(boolean(), boolean()).

-spec atom_list_id(AList) -> AList when AList :: [atom()].
atom_list_id(AList) -> AList.

-type any_pair() :: {_, _}.

-spec inter
    (ok) -> nok;
    (nok) -> ok.
inter(ok) -> nok;
inter(nok) -> ok.

-type bad_map1() :: #{
    atom() => atom(),
    integer() => integer()
}.

-type bad_map2() :: #{
    atom() := atom(),
    integer() := integer()
}.

-type empty_map() :: #{}.

-type int_dict() ::
    #{integer() => integer()}.

-type single_shape1() ::
    #{id => integer()}.
-type single_shape2() ::
    #{id := integer()}.

-type e_list1() ::
    nonempty_maybe_improper_list().
-type e_list2() ::
    nonempty_maybe_improper_list(atom(), atom()).
-type e_list3() ::
    nonempty_improper_list(atom(), atom()).

-spec ret_op() -> -1.
ret_op() -> -1.

-spec ret_op_in_constraint() ->
    Ret when Ret :: -1.
ret_op_in_constraint() -> -1.

-record(foo, {
    id :: pid(),
    name :: pid()
}).

-type foo_rec() :: #foo{}.
