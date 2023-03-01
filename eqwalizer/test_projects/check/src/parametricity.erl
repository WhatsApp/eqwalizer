%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(parametricity).

-compile([export_all, nowarn_export_all]).

-spec lmap
    (fun((A) -> B), [A]) -> [B].
lmap(_F, []) -> [];
lmap(F, [H|T]) -> [F(H) | lmap(F, T)].

-spec lmap_comp
    (fun((A) -> B), [A]) -> [B].
lmap_comp(F, L) -> [F(Elem) || Elem <- L].

-spec member(Elem, List) -> boolean() when
      Elem :: T,
      List :: [T].
member(_Elem, []) -> false;
member(Elem, [Elem | _]) -> true;
member(Elem, [_ | T]) -> member(Elem, T).

-spec append(List1, List2) -> List3 when
      List1 :: [T],
      List2 :: [T],
      List3 :: [T].
append([], L2) ->
    L2;
append([H1|T1], L2) ->
    [H1 | append(T1, L2)].

-spec append(ListOfLists) -> List1 when
      ListOfLists :: [List],
      List :: [T],
      List1 :: [T].
append([E]) -> E;
append([H|T]) -> append(H, append(T));
append([]) -> [].

-spec subtract(L1, L2) -> L3 when
      L1 :: [T],
      L2 :: [T],
      L3 :: [T].
subtract([], _) -> [];
subtract([H1 | T1], L2) ->
    case member(H1, L2) of
        true ->
            subtract(T1, L2);
        false ->
            [H1 | subtract(T1, L2)]
    end.

-spec subtract_comp(L1, L2) -> L3 when
      L1 :: [T],
      L2 :: [T],
      L3 :: [T].
subtract_comp(L1, L2) ->
    [E || E <- L1, not member(E, L2)].

-spec prefix(L1, L2) -> boolean() when
      L1 :: [T],
      L2 :: [T].
prefix([X|PreTail], [X|Tail]) ->
    prefix(PreTail, Tail);
prefix([], _) -> true;
prefix([_|_], _) -> false.

-spec zipwith(Combine, L1, L2) -> L3 when
      Combine :: fun((X, Y) -> T),
      L1 :: [X],
      L2 :: [Y],
      L3 :: [T].
zipwith(F, [X | Xs], [Y | Ys]) ->
    [F(X, Y) | zipwith(F, Xs, Ys)];
zipwith(_, [], []) ->
    [].

-spec zipwith3(Combine, L1, L2, L3) -> L4
    when
      Combine :: fun((X, Y, Z) -> T),
      L1 :: [X],
      L2 :: [Y],
      L3 :: [Z],
      L4 :: [T].

zipwith3(F, [X | Xs], [Y | Ys], [Z | Zs])
    -> [F(X, Y, Z)
       | zipwith3(F, Xs, Ys, Zs)];
zipwith3(_, [], [], [])
    -> [].

-spec all(Pred, List) -> boolean() when
      Pred :: fun((T) -> boolean()),
      List :: [T].
all(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
        true -> all(Pred, Tail);
        false -> false
    end;
all(Pred, [])
    when is_function(Pred, 1) -> true.

-spec any(Pred, List) -> boolean() when
      Pred :: fun((T) -> boolean()),
      List :: [T],
      T :: term().
any(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
        true -> true;
        false -> any(Pred, Tail)
    end;
any(Pred, [])
    when is_function(Pred, 1) -> false.

-spec foldl(Fun, Acc, List) -> Acc when
      Fun :: fun((T, Acc) -> Acc),
      List :: [T].
% OTP version constrains T :: term()

foldl(F, Acc, [Hd|Tail]) ->
    foldl(F, F(Hd, Acc), Tail);
foldl(_F, Acc, []) -> Acc.

-spec foldr(Fun, Acc, List) -> Acc when
      Fun :: fun((T, Acc) -> Acc),
      List :: [T].

foldr(F, Acc, [Hd|Tail]) ->
    F(Hd, foldr(F, Acc, Tail));
foldr(_F, Acc, []) -> Acc.

-spec filter(Pred, List1) -> List2 when
      Pred :: fun((T) -> boolean()),
      List1 :: [T],
      List2 :: [T].
filter(Pred, List) ->
    [ E || E <- List, Pred(E) ].

-spec mapfoldl(Fun, Acc, List1) -> {List2, Acc} when
      Fun :: fun((A, Acc) -> {B, Acc}),
      List1 :: [A],
      List2 :: [B].
mapfoldl(F, Accu0, [Hd|Tail]) ->
    {R,Accu1} = F(Hd, Accu0),
    {Rs,Accu2} = mapfoldl(F, Accu1, Tail),
    {[R|Rs],Accu2};
mapfoldl(_F, Accu, []) -> {[],Accu}.

-spec takewhile(Pred, L1) -> L2 when
      Pred :: fun((T) -> boolean()),
      L1 :: [T],
      L2 :: [T].
takewhile(Pred, [H|T]) ->
    case Pred(H) of
        true -> [H|takewhile(Pred, T)];
        false -> []
    end;
takewhile(_Pred, []) ->
    [].

-spec search(Pred, List) -> {value, Value} | false when
      Pred :: fun((T) -> boolean()),
      List :: [T],
      Value :: T.

search(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
        true -> {value, Hd};
        false -> search(Pred, Tail)
    end;
search(_Pred, []) ->
    false.

-spec refine01(A, _B) -> A.
refine01(X, X) -> X.

-spec refine02(_A, B) -> B.
refine02(X, X) -> X.

-spec refine03(A, B, {A, B}) ->
    term().
refine03(X, X, _) ->
    [Elem || Elem <- X].

% adapted from wa_lists.erl
-spec uniquify(List1) -> List2 when
      List1 :: [Elem],
      List2 :: [Elem].
uniquify(List) ->
    {Set, Len} = lists:foldl(
        fun(E, {S, L}) ->
            {gb_sets:add(E, S), L+1} end,
        {gb_sets:new(), 0}, List),
    case gb_sets:size(Set) of
        Len -> List;
        _   -> gb_sets:to_list(Set)
    end.

% overloaded parametricity
-spec select
(normal, Normal, _) -> Normal;
(extended, _, Extended) -> Extended.
select(normal, Normal, _) -> Normal;
select(extended, _, Extended) -> Extended.

-spec select_tagged
(normal) -> integer();
(extended) -> binary().
select_tagged(T) -> select(T, 0, <<>>).

-spec select_tagged_neg1
    (normal) -> integer();
    (extended) -> binary().
select_tagged_neg1(T) ->
  select(T, <<>>, 0).

-spec select_tagged_neg2
    (normal) -> integer();
    (extended) -> binary().
select_tagged_neg2(T) ->
  select(T, 0, 0).

-spec unwrap
    (undefined, Default) -> Default;
    (Value, _) -> Value.
unwrap(undefined, Default) -> Default;
unwrap(Value, _)
  when Value =/= undefined ->
  Value.
