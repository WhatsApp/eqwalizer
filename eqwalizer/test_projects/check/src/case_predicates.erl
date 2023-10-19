%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(case_predicates).
-compile([export_all, nowarn_export_all]).

-spec my_apply(term(), term())
      -> ok | not_ok.
my_apply(X, Y) ->
  case {is_atom(X), is_atom(Y)} of
    {true, true} -> aa(X, Y);
    {true, _} -> a(X);
    {_, true} -> a(Y);
    _ -> not-ok
  end.

-spec aa(atom(), atom()) -> ok.
aa(_, _) -> ok.

-spec a(atom()) -> ok.
a(_) -> ok.

-spec is_registered(term()) -> boolean().
is_registered(X) ->
  case is_atom(X) of
    true -> undefined =/= whereis(X);
    false -> false
  end.

-spec a_zero1(
    atom() | fun((atom()) -> atom()),
    atom()
) -> atom().
a_zero1(AF, A) ->
  case is_function(AF) of
    true -> AF(A);
    false -> A
  end.

-spec a_zero1a(
    atom() | fun((atom()) -> atom()),
    atom()
) -> atom().
a_zero1a(AF, A) ->
  Res =
    case is_function(AF) of
      true -> AF(A);
      false -> A
    end,
  Res.

-spec a_zero2(
    atom() | fun((atom()) -> atom()),
    atom()
) -> atom().
a_zero2(AF, A) ->
  case is_function(AF) of
    false -> A;
    true -> AF(A)
  end.

-spec a_zero2a(
    atom() | fun((atom()) -> atom()),
    atom()
) -> atom().
a_zero2a(AF, A) ->
  Res =
    case is_function(AF) of
      false -> A;
      _ -> AF(A)
    end,
  Res.

-spec a_zero3(
    atom() | fun((atom()) -> atom()),
    atom()
) -> atom().
a_zero3(AF, A) ->
  case is_function(AF, 1) of
    true -> AF(A);
    false -> A
  end.

-spec a_zero3a(
    atom() | fun((atom()) -> atom()),
    atom()
) -> atom().
a_zero3a(AF, A) ->
  Res =
    case is_function(AF, 1) of
      true -> AF(A);
      false -> A
    end,
  Res.

-spec wrap1(
    atom() | pid()
) -> {a, atom()} | {p, pid()}.
wrap1(X) ->
  Res =
    case is_atom(X) of
      true -> {a, X};
      false -> {p, X}
    end,
  Res.

-spec wrap2(
    atom() | pid()
) -> {a, atom()} | {p, pid()}.
wrap2(X) ->
  case is_atom(X) of
    true -> {a, X};
    false -> {p, X}
  end.

-spec wrap3_neg(
    atom() | pid() | reference()
) -> {a, atom()} | {p, pid()}.
wrap3_neg(X) ->
  case is_atom(X) of
    true -> {a, X};
    false -> {p, X}
  end.

-spec wrap4(
    atom() | pid()
) -> {a, atom()} | {p, pid()}.
wrap4(X) ->
  case is_pid(X) of
    true -> {p, X};
    false -> {a, X}
  end.

-type child() :: undefined | pid().

-spec wrap5(
    child() | restarting
) -> {p, pid()} | undefined.
wrap5(X) ->
  case is_pid(X) of
    true -> {p, X};
    _ -> X
  end.

-spec andalso1(
    integer() | undefined
) -> pos_integer().
andalso1(X) ->
  case is_number(X) andalso X > 0 of
    true -> X;
    false -> 1
  end.
