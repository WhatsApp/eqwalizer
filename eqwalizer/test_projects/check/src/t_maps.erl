%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(t_maps).

-compile([export_all, nowarn_export_all]).

-export_type([
    a/0,
    n/0
]).

-type b() :: boolean().
-type n() :: number().
-type a() :: atom().

-spec empty_map_01() -> #{}.
empty_map_01() -> #{}.

-spec empty_map_02() -> map().
empty_map_02() -> #{}.

-spec empty_map_03() -> term().
empty_map_03() -> #{}.

-spec dict_map_01() ->
    #{integer() => atom()}.
dict_map_01() ->
    #{0 => zero, 1 => one}.

-spec dict_map_02_neg() ->
    #{integer() => atom()}.
dict_map_02_neg() ->
    #{0 => 1, 1 => 2}.

-spec dict_map_03_neg() ->
    #{integer() => atom()}.
dict_map_03_neg() ->
    #{0 => zero, 1 => 2}.

-spec dict_map_04() ->
    #{integer() => atom() | number()}.
dict_map_04() ->
    #{0 => zero, 1 => 2}.

-spec shape_01() ->
    #{zero := number(), one => number()}.
shape_01() ->
    #{zero => 0, one => 1}.

-spec shape_02_neg() ->
    #{zero := number(), one => number()}.
shape_02_neg() ->
    #{zero => 0, one => one}.

% only atom keys can be updated
% unconditionally
-spec update_req_non_atom_neg
    (map()) -> map().
update_req_non_atom_neg(M) ->
    M#{1 := 1}.

% it is not allowed to use
% a dict for unconditional update
-spec dict_update_req_neg
    (map()) -> map().
dict_update_req_neg(M) ->
    M#{one := 1}.

-spec empty_update1
    (map()) -> map().
empty_update1(M) ->
    M#{}.

-spec empty_update2_neg
    (term()) -> map().
empty_update2_neg(M) ->
    M#{}.

-spec empty_update3
    () -> #{}.
empty_update3() ->
    (#{})#{}.

-spec dict_update1
    (#{atom() => true})
  -> #{atom() => boolean()}.
dict_update1(D) ->
    D#{foo => false}.

-spec dict_update2
    (#{atom() => true})
  -> map().
dict_update2(D) ->
    D#{1 => bar}.

-spec shape_update1
    (#{foo => b(), bar => n()})
    -> #{foo => b(), bar => b()}.
shape_update1(S) ->
    S#{bar => true}.

-spec shape_update2
    (#{foo => b(), bar := n()})
        -> #{foo => b(), bar := b()}.
shape_update2(S) ->
    S#{bar := true}.

-spec shape_update3_neg
    (#{foo => b(), bar => n()})
        -> #{foo => b(), bar := b()}.
shape_update3_neg(S) ->
    S#{bar := true}.

-spec shape_update4_neg
    (term()) -> #{bar := b()}.
shape_update4_neg(S) ->
    S#{bar := true}.

-spec meet_dict1
    (#{b() | n() => term()}, #{a() => term()}) ->
     #{b() => term()}.
meet_dict1(D, D) -> D.

-spec meet_dict2_neg
    (#{b() | n() => term()}, #{a() => term()}) ->
    #{n() => term()}.
meet_dict2_neg(D, D) -> D.

-spec meet_shape1
    (#{a := a()}, #{a := b()}) ->
    #{a := b()}.
meet_shape1(S, S) -> S.

-spec meet_shape2_neg
    (#{a := a()}, #{a := b()}) ->
    #{a := n()}.
meet_shape2_neg(S, S) -> S.

-spec meet_shape3_neg
    (#{a := a()}, #{a := b()}) ->
    #{a => n()}.
meet_shape3_neg(S, S) -> S.

-spec meet_shape4
    (#{a => a()}, #{a => b() | n()}) ->
    #{a => b()}.
meet_shape4(S, S) -> S.

-spec meet_shape5_neg
    (#{a => a()}, #{a => b() | n()}) ->
    #{a := b()}.
meet_shape5_neg(S, S) -> S.

-spec meet_shape6
    (#{a := a()}, #{a => b() | n()}) ->
    #{a := b()}.
meet_shape6(S, S) -> S.

-spec meet_shape7
    (#{a := a()}, #{b := b()}) ->
    none().
meet_shape7(S, S) -> S.

% this seems to be really cool
-spec u_shape1
    (#{a := a()} | #{a := b(), b := b()}) ->
    (#{a := n()} | #{a := n(), b := b()}).
u_shape1(S) ->
    S#{a := 1}.

-spec u_shape2_neg
    (#{a := a()}) ->
    (#{a := a(), b := a()}).
u_shape2_neg(S) ->
    S#{b := foo}.

-spec u_shape3
    (#{a := a()}) ->
    (#{a := a(), b := a()}).
u_shape3(S) ->
    S#{b => foo}.

-spec u_empty1
    (#{}) ->
    (#{n() => a()}).
u_empty1(S) ->
    S#{1 => one}.

-spec shape@dict1
    (#{a := a()}, a(), a()) ->
    (#{a() => a()}).
shape@dict1(S, K, V) ->
    S#{K => V}.

-spec shape@dict2_neg
    (#{a := a()}, n(), a()) ->
    (#{a() => a()}).
shape@dict2_neg(S, K, V) ->
    S#{K => V}.

-spec shape@dict3_neg
    (#{a := a()}, n(), a()) ->
    (#{n() => a()}).
shape@dict3_neg(S, K, V) ->
    S#{K => V}.

-spec shape@dict4_neg
    (Shape, n(), a()) ->
    (Dict) when
    Shape :: #{a := a()},
    Dict :: #{n() => a()}.
shape@dict4_neg(S, K, V) ->
    S#{K => V}.

-spec shape@dict5_neg
    (Shape, n(), a()) ->
    (Dict) when
    Shape :: #{a => a()},
    Dict :: #{n() => a()}.
shape@dict5_neg(S, K, V) ->
    S#{K => V}.

-type foo_bar(F, B) ::
    #{foo := F, bar := B}.

-type foo_bar_opt(F, B) ::
    #{foo => F, bar => B}.

-spec foo_bar_u
    (foo_bar(F1, B1), F1, B1) ->
    foo_bar(F1, B1).
foo_bar_u(FB, F1, B1) ->
    FB#{foo := F1, bar := B1}.

-spec foo_bar_u_neg
    (foo_bar(F1, B1), F1, B1) ->
    foo_bar(B1, F1).
foo_bar_u_neg(FB, F1, B1) ->
    FB#{foo := F1, bar := B1}.

-spec foo_bar_u_opt
    (foo_bar_opt(F1, B1), F1, B1) ->
    foo_bar(F1, B1).
foo_bar_u_opt(FB, F1, B1) ->
    FB#{foo => F1, bar => B1}.

-spec foo_bar_u_opt_neg
    (foo_bar_opt(F1, B1), F1, B1) ->
    foo_bar_opt(B1, F1).
foo_bar_u_opt_neg(FB, F1, B1) ->
    FB#{foo => F1, bar => B1}.

-type kv(K, V) :: #{K => V}.

-spec kvs(kv(K1, V1), K2, V2) ->
    kv(K1 | K2, V1 | V2).
kvs(Dict, K2, V2) ->
    Dict#{K2 => V2}.

-spec kvs_neg(kv(K1, V1), K2, V2) ->
    kv(K1 | K2, V1 | V2).
kvs_neg(Dict, K2, V2) ->
    Dict#{V2 => K2}.

-spec lit_type(a) ->
    #{a => number()}.
lit_type(A) -> #{A => 3}.

-spec needs_shape_a
    (#{a := term()}) -> ok.
needs_shape_a(_) -> ok.

-spec needs_shape_ab
    (#{a := term(), b := term()}) -> ok.
needs_shape_ab(_) -> ok.

-spec shapeab_neg(#{a := 3}) -> ok.
shapeab_neg(X) ->
    needs_shape_a(X),
    needs_shape_a(X#{b => hello}).

-spec shape_ab(#{a := term()}) -> ok.
shape_ab(X) ->
    needs_shape_a(X),
    needs_shape_ab(X#{b => hello}).

-spec slice_map(#{a() => n()} | [a()])
    -> #{a() => n()}.
slice_map(#{} = M) -> M;
slice_map(_) -> #{}.

-spec get_kv(term(), #{a() => n()})
    -> {a(), n()}.
get_kv(K, M) ->
    case M of
        #{K := V} -> {K, V};
        _ -> {not_found, 0}
    end.

-spec get_kv_neg(term(), #{a() => n()})
        -> {n(), a()}.
get_kv_neg(K, M) ->
    case M of
        #{K := V} -> {K, V};
        _ -> {0, not_found}
    end.

-spec f_shape1(#{a => a(), n => n()})
    -> {n(), a()}.
f_shape1(#{a := A, n := N}) ->
    {N, A};
f_shape1(#{a := A}) -> {0, A};
f_shape1(#{n := N}) -> {N, n}.

-spec f_shape2_neg(#{a => a(), n => n()})
        -> {n(), a()}.
f_shape2_neg(#{a := A, n := N}) ->
    {A, N}.

-spec to_map1(term()) -> map().
to_map1(#{} = M) -> M;
to_map1(_) -> #{}.

-spec to_map2(#{K => V} | {K, V})
    -> #{K => V}.
to_map2(#{} = M) -> M;
to_map2({K, V}) -> #{K => V}.

-spec to_map3_neg(#{V => K} | {K, V})
        -> #{K => V}.
to_map3_neg(#{} = M) -> M;
to_map3_neg({K, V}) -> #{K => V}.

-spec to_map4(
    #{a() => n()}
    | #{n() => a()}
    | #{id => id | no_id}
    | {}
) -> #{a() | n() => a() | n()}.
to_map4(#{} = M) -> M.

-spec to_map5_neg(
    #{a() => n()}
    | #{n() => a()}
    | #{id => id | no_id}
    | {}
) -> #{a() | n() => a()}.
to_map5_neg(#{} = M) -> M.

-spec no_map(
    [] | {}
) -> none().
no_map(#{} = M) -> M.

-spec no_prop(
    #{a := a(), b := b()}
) -> none().
no_prop(#{foo := V}) -> V.

-spec shape_atom_key(
    term(), #{a := a(), b := b()}
) -> {a(), a()}.
shape_atom_key(K, Shape) ->
    case Shape of
        #{K := V} -> {K, V};
        _ -> {undef, undef}
    end.

-spec shape_atom_key_neg(
    term(), #{a := a(), b := b()}
) -> {n(), a()}.
shape_atom_key_neg(K, Shape) ->
    case Shape of
        #{K := V} -> {K, V};
        _ -> {0, undef}
    end.

-spec no_kv_neg(
    K, [{K, V}]
) -> {K, V}.
no_kv_neg(K, Props) ->
    case Props of
        #{K := V} -> {K, V}
    end.

-spec k_union1(
    term(), #{pid() => term()} | #{number() => term()}
) -> pid() | number() | undefined.
k_union1(K, Dict) ->
    case Dict of
        #{K := _} -> K;
        _ -> undefined
    end.

-spec no_key(
    term(), a() | [{a(), term()}]
) -> undefined.
no_key(K, Dict) ->
    case Dict of
        #{K := _} -> K;
        _ -> undefined
    end.

-spec no_val(
    term(), a() | [{a(), term()}]
) -> undefined.
no_val(K, Dict) ->
    case Dict of
        #{K := V} -> V;
        _ -> undefined
    end.

-spec val1(
    #{a := a()} | #{a := n()}
) -> a() | n().
val1(#{a := V}) -> V.

-spec val2(
    #{a := a()} | #{b := n()}
) -> a().
val2(#{a := V}) -> V.

-spec val3(
    #{a1 := a()} | #{b1 := n()}
) -> none().
val3(#{a := V}) -> V.

-spec val4(
    {a(), a()}
) -> none().
val4(#{a := V}) -> V.

-spec val5(
    a(), {a()} | {a(), a()}
) -> none().
val5(K, M) ->
    case M of
        #{K := V} -> V
    end.

-spec guard1
    (#{a() => a()} | a())
    -> #{a() => a()}.
guard1(M) when is_map(M) -> M;
guard1(A) when is_atom(A) -> #{A => A}.

-spec guard2(a()) -> none().
guard2(M) when is_map(M) -> M.

-spec guard3
    (term()) -> map().
guard3(A) when is_map(A#{}) -> A.

-spec guard4
    (term()) -> map().
guard4(A) when is_map(A#{a := 1}) -> A.

-spec guard5
    (term(), term()) -> {map(), map()}.
guard5(M1, M2) when
    M1#{} == M2#{} -> {M1, M2}.

-spec guard6
    (term(), term()) -> {map(), map()}.
guard6(M1, M2) when
    M1#{a := 1} == M2#{a := 1}
    -> {M1, M2}.

-spec guard7
    (term()) -> map().
guard7(M) when M#{a => 1} == #{a => 1}
    -> M.

-spec refine(
    #{a := term(), b => atom()},
    #{a => atom(), b := term()}
) -> #{a := atom(), b := atom()}.
refine(S1, S1) -> S1.

%% checking constraints
-spec c_map1() -> #{term() => Atom}
    when Atom :: atom().
c_map1() -> #{ok => ok}.

-spec c_map2() -> #{Key => Atom}
    when Key :: atom(), Atom :: atom().
c_map2() -> #{key => ok}.

-spec update_none1(none()) -> none().
update_none1(N) -> N#{a := 1}.

-spec update_none2(none()) -> none().
update_none2(N) -> N#{a => 1}.

%% "smaller maps"
-spec sub_1(#{}) -> #{a => atom()}.
sub_1(M) -> M.

-spec sub_2(#{a := atom()}) ->
    #{a => atom(), n => number()}.
sub_2(M) -> M.

-spec sub_3_neg(#{a := atom()}) ->
    #{a => atom(), n := number()}.
sub_3_neg(M) -> M.

-spec test_update_with_1(
    #{string() => binary()},
    fun((binary()) -> atom())
) -> #{string() => binary() | atom()}.
test_update_with_1(Map, F) ->
    maps:update_with("", F, Map).

-spec test_update_with_2(
    #{a := b},
    fun((b) -> c)
) -> #{a => b | c }.
test_update_with_2(Map, F) ->
    maps:update_with(a, F, Map).

-spec test_update_with_3(
    #{string() => binary()},
    fun((binary()) -> atom())
) -> #{string() => binary() | atom()}.
test_update_with_3(Map, F) ->
    maps:update_with("", F, a, Map).

-spec test_update_with_4(
    #{a := b},
    fun((b) -> c)
) -> #{a => b | c }.
test_update_with_4(Map, F) ->
    maps:update_with(a, F, a, Map).

-type rec_shape() :: #{
    item := rec_shape() | undefined
}.

-type rec_shape_v2() :: #{
  item_v2 := rec_shape_v2() | undefined
}.

-spec rec_shape_1(
    #{
        item := #{
            item :=
                undefined
        }
    }
) -> rec_shape().
rec_shape_1(X) ->
    X.

-spec rec_shape_2(rec_shape()) ->
    undefined | #{
    item := undefined | #{
        item :=
            undefined | rec_shape()
        }
    }.
rec_shape_2(X) ->
    X.

-spec rec_shape_3_neg(
    rec_shape()
) -> rec_shape_v2().
rec_shape_3_neg(X) ->
    X.

-type gen_shape(T) :: #{
    item := T | gen_shape(T)
}.

-type gen_shape_v2(T) :: #{
  item_v2 := T | gen_shape_v2(T)
}.

-type gen_shape_expanded(T) :: #{
    item := T | #{
        item := T | gen_shape_expanded(T)
    }
}.

-spec gen_shape_1(
    #{ item := a }
) -> gen_shape(a | b).
gen_shape_1(X) ->
    X.

-spec gen_shape_2(
    #{ item := a }
) -> gen_shape(a | b).
gen_shape_2(X) ->
    X.

-spec gen_shape_3(gen_shape(a)) ->
    #{
        item := a | #{
            item :=
                a | gen_shape(a)
        }
    }.
gen_shape_3(X) ->
    X.

-spec gen_shape_4(
    #{
        item := a | #{
            item :=
                a | gen_shape(a)
        }
    }
) -> gen_shape(a).
gen_shape_4(X) ->
    X.

-spec gen_shape_5_neg
(gen_shape(a | b)) ->
    a | #{
        item := a | #{
            item :=
                a | gen_shape(a)
        }
    }.
gen_shape_5_neg(X) ->
    X.

-spec gen_shape_6
    (gen_shape_expanded(a)) ->
    gen_shape(a | b).
gen_shape_6(X) ->
    X.

-spec gen_shape_7_neg
    (gen_shape_v2(a)) ->
    gen_shape(a).
gen_shape_7_neg(X) ->
    X.

-spec extra_1_neg(
    #{a => va, b => vb, c => vc, d => vd}
) ->
    #{a => va, b => vb, c => vc}.
extra_1_neg(X) ->
    X.

-spec extra_2_neg(
    #{
        a => va,
        b => vb,
        c => vc,
        d => vd,
        e => ve
    }
) ->
    #{a => ka, b => kb, c => kc}.
extra_2_neg(X) ->
    X.

-spec missing_1_neg(
    #{a := va}
) ->
    #{a := va}.
missing_1_neg(X) ->
    X.

-spec missing_2_neg(
    #{ }
) ->
    #{a => va, b => vb, c := vc}.
missing_2_neg(X) ->
    X.

-spec expected_required_got_opt_1_neg(
    #{a => ka, b => kb, c => kc}
) ->
    #{a => ka, b := kb, c => kc}.
expected_required_got_opt_1_neg(X) ->
    X.

-spec expected_required_got_opt_2_neg(
    #{a => ka, b => kb, c => kc}
) ->
    #{a => ka, b := kb, c => kc}.
expected_required_got_opt_2_neg(X) ->
    X.

-spec misc_mismatch_1_neg(
   #{
       k_ok => term(),
       k_wrong1 => pid(),
       k_wrong2 => pid(),
       k_req1 => term(),
       k_req2 => term(),
       k_extra => term()
   }
) ->
    #{
        k_ok => term(),
        k_wrong1 => atom(),
        k_wrong2 => atom(),
        k_req1 := atom(),
        k_req2 := atom(),
        k_req3 := atom()
    }.
misc_mismatch_1_neg(X) ->
    X.

-spec map_k_pattern(
    #{term() => atom()}
) -> atom().
map_k_pattern(
    #{#{key => val} := I}
) -> I.

-spec dict_to_shape_1(
    #{a | b => boolean()}
) -> #{a => boolean(), b => term()}.
dict_to_shape_1(M) -> M.

-spec dict_to_shape_2(
    #{a | b => boolean()}
) -> #{a => boolean(), b => term(), c => boolean()}.
dict_to_shape_2(M) -> M.

-spec dict_to_shape_neg_1(
    #{a | b => boolean()}
) -> #{a => true, b => boolean()}.
dict_to_shape_neg_1(M) -> M.

-spec dict_to_shape_neg_2(
    #{a | b => boolean()}
) -> #{a => boolean(), b := term()}.
dict_to_shape_neg_2(M) -> M.

-spec dict_to_shape_neg_3(
    #{a | b => boolean()}
) -> #{a => boolean()}.
dict_to_shape_neg_3(M) -> M.
