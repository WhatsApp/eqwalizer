%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(overloaded).

-compile([export_all, nowarn_export_all]).

-export_type([
    prop/2,
    props/2
]).

-spec swap
    (atom()) -> binary();
    (binary()) -> atom().
swap(A) when is_atom(A) ->
    atom_to_binary(A);
swap(B) when is_binary(B) ->
    binary_to_atom(B).

-spec swap_neg
    (atom()) -> binary();
    (binary()) -> atom().
swap_neg(A) when is_atom(A) ->
    atom_to_binary(A);
swap_neg(B) when is_binary(B) ->
    B.

-spec fancy_gen_first
    ({A}) -> A;
    ({A, term()}) -> A;
    ({A, term(), term()}) -> A.
fancy_gen_first({A}) -> A;
fancy_gen_first({A, _}) -> A;
fancy_gen_first({A, _, _}) -> A.

-spec swap_proxy_1
    (atom()) -> binary();
    (binary()) -> atom().
swap_proxy_1(X) -> swap(X).

-spec swap_proxy_2
    (atom()) -> binary();
    (binary()) -> atom().
swap_proxy_2(X) ->
    Res = swap(X),
    Res.

-spec swap_proxy_3
    (atom()) -> binary();
    (binary()) -> atom().
swap_proxy_3(X) ->
    overloaded:swap(X).

-spec swap_proxy_4
    (atom()) -> binary();
    (binary()) -> atom().
swap_proxy_4(X) ->
    Res = overloaded:swap(X),
    Res.

-spec swap_proxy_gen_1(
    atom() | binary()
) -> atom() | binary().
swap_proxy_gen_1(AB) ->
    swap_proxy_1(AB).

-spec swap_proxy_gen_2(
    atom() | binary()
) -> atom() | binary().
swap_proxy_gen_2(AB) ->
    swap_proxy_2(AB).

-spec swap_proxy_gen_3(
    atom() | binary()
) -> atom() | binary().
swap_proxy_gen_3(AB) ->
    overloaded:swap_proxy_3(AB).

-spec swap_proxy_gen_4(
    atom() | binary()
) -> atom() | binary().
swap_proxy_gen_4(AB) ->
    overloaded:swap_proxy_4(AB).

-spec app_tuple
    ({fun(() -> Res)}) -> Res;
    ({fun((A) -> Res), A}) -> Res;
    ({fun((A, B) -> Res), A, B}) -> Res.
app_tuple({F}) -> F();
app_tuple({F, A}) -> F(A);
app_tuple({F, A, B}) -> F(A, B).

-spec bar(fun((a) -> b)) -> b;
    (fun((a) -> c)) -> c.
bar(_) -> throw(err).

-spec test_bar1() -> b.
test_bar1() ->
    Res = bar(fun(a) -> a end),
    Res.

-spec test_bar2(fun((a) -> b)) -> b.
test_bar2(F) ->
    Res = bar(F),
    Res.

-spec test_bar3(fun((a) -> c)) -> b.
test_bar3(F) ->
    Res = bar(F),
    Res.

-spec test_bar4(fun((a) -> z)) -> z.
test_bar4(F) ->
    Res = bar(F),
    Res.

-spec inner_bar({fun((a) -> b)}) -> b;
    ({fun((a) -> c)}) -> c.
inner_bar(_) -> throw(err).

-spec test_inner_bar() -> b.
test_inner_bar() ->
    Res = bar({fun(a) -> a end}),
    Res.

% adapted from maps:filter/2 that has
% union with type vars
-spec maps_filter(Pred, Map) -> Map when
      Pred :: fun((Key, Value) ->
          boolean()),
      Map :: #{Key => Value};
    (Pred, Iter) -> Map when
      Pred :: fun((Key, Value) ->
          boolean()),
      Iter :: maps:iterator(Key, Value),
      Map :: #{Key => Value}.
maps_filter(_, Map) when is_map(Map) ->
    Map.

-spec test_overloaded_generic(boolean())
        -> #{number() => number()}.
test_overloaded_generic(B) ->
    M = #{1 => 1, 2 => 3},
    It = maps:iterator(M),
    F = fun erlang:'=:='/2,
    case B of
        true -> maps_filter(F, M);
        false -> maps_filter(F, It)
    end.

-type prop(K, V) :: {K, V}.
-type props(K, V) :: [prop(K, V)].

% adapted from wa_props spec that has
% union with type vars
% -spec get_list(
%   [prop(K, V) | V], props(K, V)
% ) -> [V].
-spec get_list
    ([prop(K, V)], props(K, V)) -> [V];
    ([V], props(_, V)) -> [V].
get_list(KeysDefaults, List) ->
    get_list(KeysDefaults, List, []).

-spec pget(K, props(K, V), V) -> V.
pget(_, _, _) -> throw(not_implemented).

-spec pget(K, props(K, V)) -> V.
pget(_, _) -> throw(not_implemented).

% adapted from wa_props spec that has
% union with type vars:
% -spec get_list(
%   [prop(K, V) | V], props(K, V), [V]
% ) -> [V].
-spec get_list
    ([prop(K, V)], props(K, V), [V]) ->
        [V];
    ([K], props(K, V), [V]) ->
        [V].
get_list([], _List, Acc) ->
    lists:reverse(Acc);
get_list(
    [{Key, Default} | T], List, Acc
) ->
    get_list(T, List,
        [pget(Key, List, Default)|Acc]
    );
get_list([Key | T], List, Acc) ->
    get_list(T, List,
        [pget(Key, List)|Acc]).

-spec overlap_neg_1(atom()) -> number();
                 (T) -> {T}.
overlap_neg_1(X) when is_atom(X) -> 1;
overlap_neg_1(X) -> {X}.

-spec overlap_neg_2(atom()) -> number();
    (T) -> {T}.
overlap_neg_2(X) when is_atom(X) -> 1;
overlap_neg_2(_X) -> 1.

-spec get_list_atoms
    ([prop(K, V)], props(K, V), [V]) ->
    [V] when K :: atom(), V :: atom();
    ([K], props(K, V), [V]) ->
    [V] when K :: atom(), V :: atom().
get_list_atoms(_, _, _) ->
    throw(not_implemented).

-spec get_list_any
    ([prop(K, V)], props(K, V), [V]) ->
    [V] when K :: term(), V :: term();
    ([K], props(K, V), [V]) ->
    [V] when K :: term(), V :: term().
get_list_any(_, _, _) ->
    throw(not_implemented).

%% errors when overlapped domains
%% are discovered at call site
-spec non_overlap_atoms1() -> [atom()].
non_overlap_atoms1() ->
    ABs = [{a, b}],
    get_list_atoms(ABs, ABs, [b]).

-spec non_overlap_atoms2() -> [atom()].
non_overlap_atoms2() ->
    get_list_atoms([b], [{a, b}], [b]).

-spec overlap_any1() -> [atom()].
overlap_any1() ->
    ABs = [{a, b}],
    get_list_any(ABs, ABs, [b]).

-spec non_overlap_any2() -> [atom()].
non_overlap_any2() ->
    % selects the second branch and
    % reports an error
    get_list_any([b], [{a, b}], [b]).

-spec take_ok_or_any(term()) -> error;
    (ok) -> ok.
take_ok_or_any(ok) -> ok;
take_ok_or_any(_) -> error.

-spec overlap_any_neg(term()) -> error.
overlap_any_neg(Any) ->
    take_ok_or_any(Any).

-spec over
  (integer()) -> atom();
  (atom()) -> integer().
over(_X) -> throw(not_implemented).

-spec use_over(integer() | {}) -> atom().
use_over(X) -> over(X).

-type invalid() :: _T.

-spec use_invalid_neg(a) -> a;
                 (err) -> invalid().
use_invalid_neg(a) -> a.

-spec no_match_neg() -> term().
no_match_neg() ->
    _ = swap(""),
    ok.

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

-spec swap2_neg() -> number().
swap2_neg() ->
    swap(ok).

-spec reachable_1
    (a) -> a;
    (b | c) -> b.
reachable_1(a) -> a;
reachable_1(O) when O =/= a -> b.

-spec reachable_2
    (a, atom()) -> atom();
    (b, binary()) -> atom().
reachable_2(a, A) -> A;
reachable_2(O, B) when O =/= a -> binary_to_atom(B).

-spec reachable_3
    (a) -> a;
    ([atom()]) -> b.
reachable_3(I) when not is_list(I) -> a;
reachable_3(_) -> b.
