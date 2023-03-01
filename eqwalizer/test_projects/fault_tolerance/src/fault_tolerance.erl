%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(fault_tolerance).

-compile([export_all, nowarn_export_all]).

% eqwalizer:ignore
-behaviour(gen_server).

-spec b_to_n(boolean()) ->
  number().
b_to_n(false) ->
  0;
b_to_n(true) ->
  1.

-spec b_to_n1(boolean()) ->
  number().
b_to_n1(true) ->
  true;
b_to_n1(false) ->
  false.

-spec b_to_n2(boolean()) ->
  number().
b_to_n2(true) ->
  1;
b_to_n2(A) ->
  A.

-spec b_to_n3(boolean()) ->
  number().
b_to_n3(true) ->
  true;
b_to_n3(A) ->
  A.

-spec opacity_tolerance
    (atom(), misc:o()) -> atom().
opacity_tolerance(a, _) ->
  1;
opacity_tolerance(b, _) ->
  2;
opacity_tolerance(_, {_}) ->
  3;
opacity_tolerance(c, _) ->
  4.

-spec too_many_errors
    (atom()) -> atom().
too_many_errors(a) ->
  1;
too_many_errors(b) ->
  2;
too_many_errors(c) ->
  3;
too_many_errors(d) ->
  4;
too_many_errors(e) ->
  5;
too_many_errors(f) ->
  6;
too_many_errors(g) ->
  7;
too_many_errors(h) ->
  8;
too_many_errors(i) ->
  9;
too_many_errors(j) ->
  10.

-spec visible_cast
    (number()) -> number().
visible_cast(N) ->
  erlang:atom_to_binary(
    b_to_n(
      N
    )
  ).

-spec expected_fun(any()) ->
  number().
expected_fun(F) ->
  X = F(),
  Y = F(),
  {X, Y}.

-spec lambda_arity([number()], atom()) ->
  number().
lambda_arity(Ns, A) ->
  X = lists:map(fun(_X, _Y) -> 1 end, Ns),
  Y =
    X +
      A,
  Y.

%% SUBTLE/INTERESTING CASES %%
%% More errors are reported comparing
%% to "sentinel" approach.

-spec lists_append
    (map(), map()) -> map().
lists_append(M1, M2) ->
  Res = lists:append(
    M1,
    M2
  ),
  Res.

-spec maps_update1
    (term(), atom(), atom()) -> atom().
maps_update1(M, K, V) ->
  M1 = M#{
    K :=
      V +
        V},
  M1.

% Similar
-spec maps_update2
    (term(), atom(), atom()) -> atom().
maps_update2(M, K, V) ->
  M1 = M#{
    K =>
    V +
      V},
  M1.

% Similar
-spec lists_flatten
    (term(), any()) -> atom().
lists_flatten(L1, L2) ->
  Res =
    lists:flatten(
      L1,
      L2
    ),
  Res.

%% Code coverage

-spec block
    (term(), any()) -> atom().
block(X, Y) ->
  begin
    Z =
      X +
        Y,
    Z
  end.

-spec cons
    (term(), any()) -> [atom()].
cons(H, T) ->
  Res = lists:append(
    [H |
      T],
    [T |
      H]
  ),
  Res.

-spec 'case'
    (boolean(), atom()) -> number().
'case'(F, A) ->
  case F() of
    true -> A + 1;
    false -> A - 1
  end.

-spec 'if'
    (boolean(), atom()) -> number().
'if'(F, A) ->
  if
    F -> A + 1;
    true -> A - 1
  end.

-spec dyn_call
    (module(), atom()) -> ok.
dyn_call(M, F) ->
  M:F(
    F + 1,
    -M
  ).

-spec comprehensions1() -> ok.
comprehensions1() ->
  [X ||
    X <- #{
      a => b,
      c => d + 1
    },
    atom_to_binary(
      {X}
    )].

-spec comprehensions2([term()]) -> ok.
comprehensions2(L) ->
  Res = [
    atom_to_binary(Y) ||
    <<Y>> <= L
  ],
  Res.

-spec comprehensions3(binary()) -> ok.
comprehensions3(B) ->
  Res =
    <<Y || Y <-
      B
    >>,
  Res.

-spec maps(map()) ->
  ok.
maps(M) ->
  M1 = M
    #{id := a + 1},
  M2 = M
    #{id := b + 2},
  M1 + M2.

-spec binary1(atom(), atom()) -> atom().
binary1(A, S) ->
  B = <<
    A:
    S
  >>,
  B + 1.

-spec 'catch'(atom()) -> atom().
'catch'(F) ->
  Res =
    catch F(),
  Res.

-spec try1(atom()) -> atom().
try1(A) ->
  Res = try
    A()
  catch _ ->
    []
  after
    1 + a
  end,
  Res.

-spec try2(atom()) -> atom().
try2(A) ->
  Res = try
          A()
        of
          {_} ->
            A + 1
        catch _ ->
          []
        after
          1 + a
        end,
  Res.

-record(rec, {
  id :: integer(),
  pid :: pid()
}).

-spec rec1(pid(), integer()) -> atom().
rec1(Pid, Id) ->
  Res = #rec{
    id = Pid,
    pid = Id
  },
  Res.

-spec rec2(#rec{}, integer()) -> pid().
rec2(Rec, Id) ->
  Res = Rec#rec{
    pid = Id
  },
  Res#rec.id.

-spec rec3() -> atom().
rec3() ->
  Res = #rec{
    _ = <<>>
  },
  Res.

-spec receive1()
      -> number().
receive1() ->
  receive
    {N} ->
      atom_to_binary(
        [N]
          ++ a
      );
    N ->
      atom_to_binary(
        <<N>>
          ++ b
      )
  end.

-spec receive2()
      -> number().
receive2() ->
  receive
    {N} ->
      atom_to_binary(
        [N]
      );
    N ->
      atom_to_binary(
        <<N>>
      )
  after 10 ->
    []
  end.

-spec same_line() -> number().
same_line() ->
  a + b.

% eqwalizer:ignore
-type loop() :: loop().

-spec use_invalid(loop()) -> none().
use_invalid(Dyn) ->
  Dyn.

-spec fixmes(atom()) -> ok.
fixmes(Atom) ->
  % eqwalizer:fixme
  _ = Atom + 2,
  % eqwalizer:fixme
  erlang:list_to_binary(Atom),
  % eqwalizer:fixme
  error.

-spec fixmes_neg(atom()) -> ok.
fixmes_neg(Atom) ->
  % eqwalizer:fixme
  _ = Atom + 2,
  error.

-spec redundant_fixme() -> ok.
redundant_fixme() ->
  % eqwalizer:fixme
  ok.

% Error tolerance with fun args
-spec make_pair(K, number()) -> {K, number()}.
make_pair(K, V) -> {K, V}.

-spec map_pair([{K, V}], fun ((K, V) -> {K, A})) -> [{K, A}].
map_pair(_, _) -> error(unimplemented).

-spec apply_map([{atom(), term()}]) -> [{atom(), number()}].
apply_map(L) ->
    map_pair(L, fun make_pair/2).
