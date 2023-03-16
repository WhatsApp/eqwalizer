%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(generics_with_unions).
-compile([export_all, nowarn_export_all]).

% union with type vars allowed in return
-spec test01_pos(T, U) -> T | U.
test01_pos(X, Y) -> union(X, Y).

% TypeScript and Racket each
% error in the body (not the spec).
% TS's error message is good
-spec test02_neg(T | U) -> {T, U}.
test02_neg(X) -> {X, X}.

-spec test03(T, U, [T|U]) -> [T|U].
test03(_X, _Y, XYs) -> XYs.

-spec test04(T | U, T, U) -> ok.
test04(_XY, _X, _Y) -> ok.

-spec test05({{{{T}}} | 3}, T) -> nok.
test05(_, _) -> nok.




-spec test07({T} | {T, T}) -> T.
test07({X}) -> X;
test07({X, X}) -> X.

-spec test06(#{K => V} | {K, V})
    -> #{K => V}.
test06(#{} = M) -> M.


% copied from Check tests that passed
% before we introduced the error for
% unions with type variables


-spec k_union1(
    term(), #{K1 => term()} | #{K2 => term()}
) -> K1 | K2 | undefined.
k_union1(K, Dict) ->
    case Dict of
        #{K := _} -> K;
        _ -> undefined
    end.

-spec union_with_type_vars(T) -> T.
union_with_type_vars(T) ->
    T2 = case ok of
        ok -> T;
        _ -> T
    end,
    Res = id(T2),
    Res.

-spec union(T, U) -> T | U.
union(X, _) -> X.

-spec id(T) -> T.
id(T) -> T.

-spec test_ret_neg() -> ok.
test_ret_neg() -> union(a, b).

-type ty_var_from_nowhere() :: _T.

-spec test_uses_ty_var_from_nowhere
    (pid() | ty_var_from_nowhere())
    -> nok.
test_uses_ty_var_from_nowhere(X) -> X.

-type prop(K, V) :: {K, V}.
-type props(K, V) :: [prop(K, V)].

% inspired by wa_props:get_list/2
-spec get_list([prop(K, V) | V],
    props(K, V)) -> [V].
get_list(_, _) ->
    throw(not_implemented).

-spec use_get_list_1() -> [number()].
use_get_list_1() ->
    get_list([{a, 2}], [{a, 5}]).

-spec use_get_list_2() -> [a | number()].
use_get_list_2() ->
    get_list([3], [{a, 5}]).

% slightly weird behavior,
% but does allow getting
% data from a list of unknown
% contents
-spec use_get_list_3() ->
    [pid() | number()].
use_get_list_3() ->
    get_list([self()], [{a, 5}]).

% inspired by wa_props:set_list/2
-spec set_list([prop(K, V) | {K}],
    props(K, V)) -> props(K, V).
set_list([], List) ->
    List;
set_list([{Key}|T], List) ->
    NewList = lists:keydelete(
        Key, 1, List),
    set_list(T, NewList);
set_list([{Key, Value}|T], List) ->
    NewList = lists:keystore(
        Key, 1, List, {Key, Value}),
    set_list(T, NewList).

-spec use_set_list() ->
    [{a, number()}].
use_set_list() ->
    set_list([{a, 5}], [{a, 5}]).

-spec use_set_list_2() ->
    [{pid() | a, number()}].
use_set_list_2() ->
    set_list([{self(), 5}], [{a, 5}]).

-spec use_set_list_neg() ->
    wrong_ret.
use_set_list_neg() ->
    set_list([{a, 5}], [{a, 5}]).

% inspired by wa_props:
% collect_nondefault/1
-spec collect_nondefault([prop(K, V)
    | {K, V, V} | {K, V, V, atom()}])
    -> props(K, V).
collect_nondefault(_) ->
    throw(not_implemented).

% inspired by wa_props:
% create_nondefault/1
-spec create_nondefault([prop(K, V) |
    {K, V, V}]) -> props(K, V).
create_nondefault(KeysValuesDefaults) ->
    create_nondefault(
        KeysValuesDefaults, []).

-spec use_create_nondefault() ->
    [{k, v}].
use_create_nondefault() ->
    create_nondefault([{k, v}]).

-spec use_create_nondefault_2() ->
    [{k, v}].
use_create_nondefault_2() ->
    create_nondefault([{k, v, v}]).

% inspired by wa_props:
% create_nondefault/2
-spec create_nondefault([prop(K, V) |
{K, V, V}], props(K, V)) -> props(K, V).
create_nondefault([], Acc) ->
    lists:reverse(Acc);
create_nondefault([{K,V}|T], Acc) ->
    create_nondefault(
        T, [{K,V}|Acc]);
create_nondefault([{_,V,V}|T], Acc) ->
    create_nondefault(T, Acc);
create_nondefault([{K,V,_}|T], Acc) ->
    create_nondefault(T, [{K,V}|Acc]).

% inspired by rel_db:process/6
-spec extract({Key, Meta, New} |
    {Key, Meta, New, SubData}) ->
        {Key, Meta, New | SubData}.
extract({Key, Meta, New}) ->
    {Key, Meta, New};
extract({Key, Meta, _New, SubData}) ->
    {Key, Meta, SubData}.

-spec use_extract_1() -> {a, b, c}.
use_extract_1() ->
    extract({a, b, c}).

-spec use_extract_2() -> {a, b, c | d}.
use_extract_2() ->
    extract({a, b, c, d}).

% inspired by % smc_observer_thrift_utils:
% rpc_with_backoff/4
-spec handle_res(
    fun((term()) -> {ok, Response}
        | {error, term()})
) -> Response.
handle_res(F) ->
    case F(2) of
        {ok, Res} -> Res;
        {error, Reason} -> throw(Reason)
    end.

-spec use_handle_res() -> ok.
use_handle_res() ->
    handle_res(fun(_) -> {ok, ok} end).

-spec ret_ok_a_or_error(_) ->
    {ok, a} | {error, b}.
ret_ok_a_or_error(_) ->
    {ok, a}.

-spec use_handle_res_2() -> a.
use_handle_res_2() ->
    handle_res(fun ret_ok_a_or_error/1).

-spec overlap_1([T] | T) -> T.
overlap_1([T]) -> T;
overlap_1(T) -> T.

-spec overlap_2(T | [T]) -> T.
overlap_2(T) -> T;
overlap_2([T]) -> T.

-spec use_overlap_1() -> ok.
use_overlap_1() ->
    overlap_1([ok]).

-spec use_overlap_2() -> [ok].
use_overlap_2() ->
    overlap_2([ok]).

-spec trick(A, A | {B}) -> B.
trick(_, {B}) -> B.

-spec use_trick(term(), term()) -> none().
use_trick(A, B) -> trick(A, B).

-spec evidence() -> none().
evidence() ->
    use_trick(ok, {ok}).

-spec unwrap(undefined | Value, Default)
        -> Value | Default.
unwrap('undefined', Default) -> Default;
unwrap(Value, _Default) -> Value.

-spec unwrap_bool(undefined | boolean())
        -> boolean().
unwrap_bool(B) ->
    unwrap(B, false).

-spec unwrap_override
    (undefined, Default) -> Default;
    (Value, _) -> Value.
unwrap_override(undefined, Default) ->
    Default;
unwrap_override(Value, _) ->
    Value.

-spec unwrap_override_bool
    (undefined | boolean())
        -> boolean().
unwrap_override_bool(B) ->
    unwrap(B, false).

-spec unwrap_order2
    (Value | undefined, Default)
        -> Value | Default.
unwrap_order2('undefined', Default) ->
    Default;
unwrap_order2(Value, _Default) ->
    Value.

-spec unwrap_bool_order2
    (undefined | boolean())
        -> boolean().
unwrap_bool_order2(B) ->
    unwrap(B, false).

-spec to_float1(string(), D)
        -> float() | D.
to_float1(S, D) ->
    Res = to_float2(S, D),
    Res.

-spec to_float2(string() | binary(), D)
        -> float() | D.
to_float2(_, _) ->
    1.0.

-spec to_float3(string(), D)
        -> float() | D.
to_float3(S, D) ->
    Res = to_float4(S, D),
    Res.

-spec to_float4(string(), D)
        -> float() | D.
to_float4(_, _) ->
    1.0.

-spec to_float5(atom(), D)
        -> float() | D.
to_float5(S, D) ->
    Res = to_float6(S, D),
    Res.

-spec to_float6(atom() | binary(), D)
        -> float() | D.
to_float6(_, _) ->
    1.0.

-spec convert1(fun(() -> atom()), D)
        -> atom() | D.
convert1(F, D) -> convert(F, D).

-spec convert(
    fun(() -> atom()) | undefined,
    D
) -> atom() | D.
convert(F, _) when is_function(F) ->
    F();
convert(undefined, D) -> D.

-spec overlapping_union_neg(
    [K] | [[K]]
) -> [K].
overlapping_union_neg(X) -> X.

-spec use_bad_union_neg() -> term().
use_bad_union_neg() ->
    _ = overlapping_union_neg([]),
    ok.

-type perform_opts(Config, R) :: {ok, R} | {Config, R}.

-spec perform(perform_opts(ok, R)) ->
    perform_opts(ok, R).
perform(Opts) ->
    perform_impl(ok, Opts).

-spec perform_impl(Config, perform_opts(Config, R)) ->
    perform_opts(Config, R).
perform_impl(_, F) -> F.
