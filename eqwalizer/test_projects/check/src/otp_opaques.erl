%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(otp_opaques).

-compile([export_all, nowarn_export_all]).

-spec to_gb_set1([atom()]) ->
    gb_sets:set(atom()).
to_gb_set1(Atoms) ->
    gb_sets:from_list(Atoms).

-spec to_gb_set2_neg([atom()]) ->
    gb_sets:set(number()).
to_gb_set2_neg(Atoms) ->
    gb_sets:from_list(Atoms).

-spec to_gb_set3([A]) ->
    gb_sets:set(A).
to_gb_set3(Elems) ->
    gb_sets:from_list(Elems).

-spec to_gb_set4([atom()]) ->
    gb_sets:set(atom() | number()).
to_gb_set4(Atoms) ->
    gb_sets:from_list(Atoms).

-spec uniques1([A]) ->
    [A].
uniques1(Elems) ->
    Set = gb_sets:from_list(Elems),
    List = gb_sets:to_list(Set),
    List.

-spec uniques2_neg([[A]]) ->
    [A].
uniques2_neg(Elems) ->
    Set = gb_sets:from_list(Elems),
    List = gb_sets:to_list(Set),
    List.

%% "Proper" version
-spec union(Set1, Set2) -> Set3
    when
        Set1 :: gb_sets:set(A),
        Set2 :: gb_sets:set(B),
        Set3 :: gb_sets:set(A | B).
union(_, _) -> gb_sets:from_list([]).

-spec uniques3([A], [B]) -> [A | B].
uniques3(Elems1, Elems2) ->
    Set1 = gb_sets:from_list(Elems1),
    Set2 = gb_sets:from_list(Elems2),
    UnionSet = gb_sets:union(Set1, Set2),
    UnionList = gb_sets:to_list(UnionSet),
    UnionList.

-spec uniques4([A], [B]) -> [A | B]
    when A :: atom(), B :: number().
uniques4(Elems1, Elems2) ->
    Set1 = gb_sets:from_list(Elems1),
    Set2 = gb_sets:from_list(Elems2),
    UnionSet = gb_sets:union(Set1, Set2),
    UnionList = gb_sets:to_list(UnionSet),
    UnionList.

-spec empty() -> gb_sets:set(term()).
%% gb_set:empty() is unbound
%% because it have an unbound type var
empty() -> gb_set:empty().

-spec is_member(
    atom(), [boolean()]
) -> boolean().
is_member(A, Bs) ->
    Set = gb_sets:from_list(Bs),
    gb_sets:is_member(A, Set).

% Dialyzer errors here
-spec test1() -> term().
test1() ->
    case sets:new() of
        {X} -> X
    end.

% Dialyzer ignores this opacity violation
% https://github.com/erlang/otp/issues/5118
-spec test2() -> {term(), term()} | a.
test2() ->
    case rand:uniform(2) of
        1 -> gb_sets:new();
        2 -> a
    end.

-spec test3_neg(maps:iterator(k, v)) ->
    sets:set(a).
test3_neg(X) -> X.

-spec ts(T, sets:set(a)) -> T.
ts(X, X) -> X.

-spec test4_neg(maps:iterator(k, v))
        -> nok.
test4_neg(X) ->
    ts(X, X),
    nok.

% We skip unions in opacity
% checks.
-spec test5_wip(
    maps:iterator(k, v) |
    sets:set(a)
) -> ok.
test5_wip([_]) ->
    ok.

-spec test6_wip(
    maps:iterator(k, v) |
    sets:set(a)
) -> nok.
test6_wip(X) ->
    X().

-spec test7_neg(
    sets:set(a)
) -> nok.
test7_neg(X) ->
    X().
