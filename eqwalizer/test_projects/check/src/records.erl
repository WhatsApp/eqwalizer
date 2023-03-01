%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(records).

-compile([export_all, nowarn_export_all]).

-record(rec1, {}).
-record(rec2, {}).
-record(rec3, {}).

-record(any_foo, {id, name}).

-record(foo, {
    id :: integer(),
    name :: atom()
}).

-record(foo_def, {
    id = 1 :: integer(),
    name = default :: atom()
}).

-spec mk_rec1_pos() -> #rec1{}.
mk_rec1_pos() -> #rec1{}.

-spec mk_rec1_neg() -> #rec1{}.
mk_rec1_neg() -> #rec2{}.

-spec mk_rec1a_pos() -> #rec1{}.
mk_rec1a_pos() ->
    R = #rec1{},
    R.

-spec mk_rec1a_neg() -> #rec1{}.
mk_rec1a_neg() ->
    R = #rec2{},
    R.

-spec mk_rec_pos(atom()) ->
    #rec1{} | #rec2{}.
mk_rec_pos(rec1) -> #rec1{};
mk_rec_pos(rec2) -> #rec2{}.

-spec mk_rec_neg(atom()) ->
    #rec1{} | #rec3{}.
mk_rec_neg(rec1) -> #rec1{};
mk_rec_neg(rec2) -> #rec2{}.

-spec mk_foo_pos() -> #foo{}.
mk_foo_pos() ->
    Foo = #foo{id = 42},
    Foo.

-spec mk_foo_neg() -> #foo{}.
mk_foo_neg() ->
    Foo = #foo{name = bar},
    Foo.

-spec fix_foo_pos(#foo{}) -> #foo{}.
fix_foo_pos(Foo) ->
    Foo#foo{name = default, id = 0}.

-spec fix_foo1_pos(#foo{}) -> #foo{}.
fix_foo1_pos(Foo) ->
    Foo1 = Foo#foo{name = default, id = 0},
    Foo1.

-spec fix_foo_neg(#foo{}) -> #foo{}.
fix_foo_neg(Foo) ->
    Foo#foo_def{name = default, id = 0}.

-spec index1_pos() -> integer().
index1_pos() ->
    #foo.name.

-spec index2_pos() -> integer().
index2_pos() ->
    Index = #foo.name,
    Index.

-spec index1_neg() -> atom().
index1_neg() ->
    #foo.name.

-spec index2_neg() -> atom().
index2_neg() ->
    Index = #foo.name,
    Index.

-spec select1_pos(#foo{}) -> integer().
select1_pos(Foo) ->
    Foo#foo.id.

-spec select2_pos(#foo{}) -> integer().
select2_pos(Foo) ->
    Id = Foo#foo.id,
    Id.

-spec select1_neg(#foo{}) -> integer().
select1_neg(Foo) ->
    Foo#foo.name.

-spec select2_neg(#foo{}) -> integer().
select2_neg(Foo) ->
    Id = Foo#foo.name,
    Id.

-spec rec_index_pat_pos(term())
    -> integer().
rec_index_pat_pos(#foo.id = I) -> I.

-spec rec_index_pat_neg(term())
        -> atom().
rec_index_pat_neg(#foo.id = I) -> I.

-spec rec_pat_pos(term())
        -> {integer(), atom()}.
rec_pat_pos(#foo{id = I, name = N}) ->
    {I, N}.

-spec rec_pat_neg(term())
        -> {integer(), atom()}.
rec_pat_neg(#foo{id = I, name = N}) ->
    {N, I}.

-spec pat_fields(#any_foo{}) ->
    {#foo{}, #foo{}}.
pat_fields(
    #any_foo{
        id = (#foo{} = I),
        name = (#foo{} = N)
    }
) -> {I, N}.

-spec rec_guard1(term()) -> #foo{}.
rec_guard1(Foo)
    when Foo#foo.id > 0 -> Foo.

-spec rec_guard2_neg(term()) -> ok.
rec_guard2_neg(Foo)
    when Foo == #foo{} -> ok.

-spec rec_guard3_pos(term(), term())
    -> number().
rec_guard3_pos(Foo, X)
    when Foo == #foo_def{id = X} -> X.

-spec rec_guard4_neg(term(), term())
        -> atom().
rec_guard4_neg(Foo, X)
    when Foo == #foo_def{id = X} -> X.

-record(rec_a, { field :: number() }).
-record(rec_b, { field :: #rec_a{} }).

-spec record_in_record
    (#rec_b{}) -> number().
record_in_record(X) ->
    (X#rec_b.field)#rec_a.field.

-spec record_in_record_neg
    (#rec_b{}) -> atom().
record_in_record_neg(X) ->
    (X#rec_b.field)#rec_a.field.

% iodata() type is WIP
-record(iodata_box, {iod :: iodata()}).

-spec use_iodata_box() -> term().
use_iodata_box() ->
    #iodata_box{iod = 's'}.

-record(mrec, {
    id1 :: integer(),
    id2 :: integer(),
    name1 :: atom(),
    name2 :: atom()
}).

-spec field_gen_mk() -> #mrec{}.
field_gen_mk() ->
    #mrec{id1 = 1, id2 = 2, _ = ok}.

-spec field_gen_mk_neg() -> #mrec{}.
field_gen_mk_neg() ->
    #mrec{id1 = 1, name1 = n, _ = ok}.

-spec field_gen_pat(term())
        -> atom().
field_gen_pat(
    #mrec{id1 = 1, id2 = 2, _ = A}
) ->
    A.

-spec field_gen_pat_neg(term())
        -> none().
field_gen_pat_neg(
    #mrec{id1 = 1, name1 = n, _ = A}
) ->
    % is both id2 (integer)
    % and name2 (atom)
    A.

-record(any_box, {
    inner :: eqwalizer:refinable(term())
}).

%% "Refined" record type
-type int_box() ::
    #any_box{inner :: integer()}.

-spec mk_int_box_1_neg() -> int_box().
mk_int_box_1_neg() ->
    #any_box{inner = ok}.

-spec mk_int_box_2_neg() -> IB when
    IB :: #any_box{inner :: integer()}.
mk_int_box_2_neg() ->
    #any_box{inner = ok}.

-type box2(T) :: #any_box{inner :: T}.

-spec unbox2(box2(T)) -> T.
unbox2(B) ->
    B#any_box.inner.

-spec test_unbox2() -> ok.
test_unbox2() ->
    unbox2(#any_box{inner = ok}).
    
-spec test_unbox2_gen() -> ok.
test_unbox2_gen() ->
    unbox2(#any_box{_ = ok}).
    
-spec test_unbox2_undef() -> undefined.
test_unbox2_undef() ->
    unbox2(#any_box{}).
    
-type box2_tuple(T) ::
    #any_box{inner :: T}.

-spec unbox2_tuple({any_box, T}) -> T.
unbox2_tuple({_, I}) -> I.

-spec test_unbox2_tuple() -> ok.
test_unbox2_tuple() ->
    unbox2_tuple(#any_box{inner = ok}).
    
-spec test_tuple_unbox2() -> ok.
test_tuple_unbox2() ->
    unbox2({any_box, ok}).

-record(int_bool_box, {
    inner = true :: eqwalizer:refinable(
        integer() | boolean()
    )
}).

-type only_int_box() ::
    #int_bool_box{inner :: integer()}.
    
-type only_bool_box() ::
    #int_bool_box{inner :: boolean()}.

-type only_true_box() ::
    #int_bool_box{inner :: true}.

-type only_atom_box() ::
    #int_bool_box{inner :: atom()}.

-spec test_int_bool_box()
    -> only_int_box().
test_int_bool_box() ->
    #int_bool_box{inner = 0}.

-spec test_int_bool_box_neg()
    -> only_atom_box().
test_int_bool_box_neg() ->
    #int_bool_box{inner = a}.

-spec test_int_bool_box_gen_neg()
    -> only_atom_box().
test_int_bool_box_gen_neg() ->
    #int_bool_box{_ = a}.
    
-spec test_int_bool_box_default_neg()
    -> only_int_box().
test_int_bool_box_default_neg() ->
    #int_bool_box{}.

-spec test_int_bool_box_default()
    -> only_bool_box().
test_int_bool_box_default() ->
    #int_bool_box{}.
    
-spec test_rec_update(only_bool_box())
    -> only_int_box().
test_rec_update(B) ->
    B#int_bool_box{inner = 3}.
    
-spec test_rec_update_neg
    (only_bool_box())
    -> only_atom_box().
test_rec_update_neg(B) ->
    B#int_bool_box{inner = a}.
    
-spec test_rec_update_02_neg
    (only_bool_box())
    -> only_bool_box().
test_rec_update_02_neg(B) ->
    B#int_bool_box{inner = 4}.
    
-spec select_bad_neg(only_int_box())
    -> integer().
select_bad_neg(R) ->
    R#any_box.inner.

-spec test_select_neg(only_int_box())
    -> boolean().
test_select_neg(R) ->
    R#int_bool_box.inner.
    
-spec test_tuple_record_neg()
    -> #int_bool_box{}.
test_tuple_record_neg() ->
    {int_bool_box, a}.

-spec test_tuple_record_02_neg()
    -> only_int_box().
test_tuple_record_02_neg() ->
    {int_bool_box, a}.

-spec test_tuple_select
    ({int_bool_box, boolean()})
    -> boolean().
test_tuple_select(R) ->
    R#int_bool_box.inner.

-spec test_select_union
    (only_bool_box() | only_true_box())
    -> boolean().
test_select_union(R) ->
    R#int_bool_box.inner.

-spec test_select_union_neg
    (only_bool_box() | only_true_box())
    -> true.
test_select_union_neg(R) ->
    R#int_bool_box.inner.
    
-record(bad_default, {
    inner = true :: integer()
}).

-spec test_bad_default_neg()
    -> #bad_default{}.
test_bad_default_neg() ->
    #bad_default{}.

-spec test_rec_to_refined_neg()
    -> only_int_box().
test_rec_to_refined_neg() ->
    #bad_default{inner = 42}.

-spec test_tuple_select_neg
    ({int_bool_box, integer()})
    -> integer().
test_tuple_select_neg(R) ->
    R#bad_default.inner.
    
-record(refined_two_fields, {
    inner :: eqwalizer:refinable(term()),
    other :: integer()
}).

-spec test_select_other
    (#refined_two_fields{})
    -> integer().
test_select_other(R) ->
    R#refined_two_fields.other.

-spec test_subtype_refine_neg
    (#refined_two_fields{})
    -> #refined_two_fields{inner :: integer()}.
test_subtype_refine_neg(R) -> R.

-spec test_subtype_union_refine
    ({refined_two_fields, integer() | boolean(), integer()})
    -> (#refined_two_fields{inner :: integer()}
     | #refined_two_fields{inner :: boolean()}).
test_subtype_union_refine(T) -> T.

-spec test_unrec_neg() -> term().
test_unrec_neg() ->
    L = case #foo{ id = 1, name = name} of
        {foo, 1, Name} ->
            [Name]
    end,
    lists:nth(1, L) * 3.

-record(recurd, {rec :: #recurd{}}).

-spec test_recurd(#recurd{}) ->
    #recurd{}.
test_recurd(X) -> X#recurd.rec.

-record(recurd2, {
    rec :: #recurd2{},
    field :: _T
}).

-spec test_recurd2_neg(#recurd2{}) ->
    #recurd2{}.
test_recurd2_neg(X) ->
    X#recurd.rec.

-type loop() :: loop().
-record(invalid, {field :: loop()}).

-spec unbound_select_neg(term()) -> nok.
unbound_select_neg(X) ->
    X#invalid.field.

-spec unbound_select_neg2(term()) -> ok.
unbound_select_neg2(X) ->
    _ = X#invalid.field,
    ok.

-spec unbound_update_neg(none()) -> term().
unbound_update_neg(X) ->
    X#invalid{field = 2}.

-record(untyped, {
    field1 :: atom(),
    field2
}).

-spec inspect_untyped(#untyped{})
        -> atom().
inspect_untyped(U) ->
    Field2 = U#untyped.field2,
    eqwalizer:reveal_type(Field2),
    Field2.

-type bin_tuple() :: {
    binary(),
    binary()
}.

-type bin_triple() :: {
    binary(),
    binary(),
    binary()
}.

-type rec() :: misc:my_record_hidden().

-spec atomize2(
    rec() | bin_tuple()
) -> atom().
atomize2({B, _}) ->
    binary_to_atom(B);
atomize2(Rec) ->
    misc:atomize(Rec).

-spec atomize3(
    rec() | bin_triple()
) -> atom().
atomize3({B, _, _}) ->
    binary_to_atom(B);
atomize3(Rec) ->
    misc:atomize(Rec).

-spec rec_unsound(#foo{})
        -> types1:foo_rec().
rec_unsound(X) ->
    X.

-record(two_fields, {
  id1 :: atom(),
  id2 :: atom()
}).

-spec fields_equal(#two_fields{})
  -> boolean().
fields_equal(X) 
  when X =:= #two_fields{_ = id} ->
  true;
fields_equal(_) ->
  false.

-record(two_ref, {
    a :: eqwalizer:refinable(term()),
    b :: eqwalizer:refinable(term())
}).

-type two_ref1() :: #two_ref{a :: atom(), b :: atom()}.

-spec narrow1(#two_ref{}, atom()) -> two_ref1().
narrow1(F, A) ->
    F1 = F#two_ref{a = A, b = A},
    F1.

-spec narrow2(#two_ref{}, atom()) -> two_ref1().
narrow2(F, A) ->
    F1 = F#two_ref{a = A},
    F2 = F1#two_ref{b = A},
    F2.
    
-type two_ref2() :: #two_ref{a :: integer(), b :: integer()}.
-type two_ref3() :: #two_ref{b :: boolean()}.

-spec narrow3({two_ref1(), two_ref2()}) -> ok.
narrow3({X, X}) -> X.

-spec narrow4({two_ref1(), two_ref3()}) -> {atom(), boolean()}.
narrow4({X, X}) -> {X#two_ref.a, X#two_ref.b}.

-spec narrow5_neg({two_ref1(), two_ref3()}) -> ok.
narrow5_neg({X, X}) -> X.

-spec narrow6(two_ref1() | two_ref2()) -> (two_ref1() | two_ref2()).
narrow6(R = #two_ref{a = _, b = _}) -> R.

-spec mk_foo() -> #foo{} | none().
mk_foo() ->
    #foo{id = 1, name = name}.

-spec un_foo() -> atom().
un_foo() ->
    Rec = mk_foo(),
    #foo{id = Id} = Rec,
    Id.

-spec mk_two_ref() -> #two_ref{} | none().
mk_two_ref() ->
    #two_ref{}.

-spec un_two_ref1() -> atom().
un_two_ref1() ->
    Rec = mk_two_ref(),
    Rec#two_ref.a.

-spec un_two_ref2() -> atom().
un_two_ref2() ->
    Rec = mk_two_ref(),
    #two_ref{a = A} = Rec,
    A.
