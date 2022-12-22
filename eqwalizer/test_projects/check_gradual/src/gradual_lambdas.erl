%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(gradual_lambdas).
-compile([export_all, nowarn_export_all]).

-spec n_lambda_1
    () -> done.
n_lambda_1() ->
  DoWork =
    fun Loop(N) ->
      case N >= 10 of
        true -> done;
        false -> Loop(N + 1)
      end
    end,
  DoWork(1).

-spec n_lambda_2
    () -> number().
n_lambda_2() ->
  DoWork =
    fun Loop(N) ->
      case N >= 10 of
        true -> done;
        false -> Loop(N + 1)
      end
    end,
  DoWork(1).

-spec n_lambda_3_neg
    (atom()) -> number().
n_lambda_3_neg(A) ->
  DoWork =
    fun Loop(N) ->
      case N >= 10 of
        true -> A + 1;
        false -> Loop(N + 1)
      end
    end,
  DoWork(1).

-spec n_lambda_4_neg
    () -> number().
n_lambda_4_neg() ->
  DoWork =
    fun Loop(N) ->
      case N >= 10 of
        true -> done;
        false -> Loop(N + 1)
      end
    end,
  DoWork().

-spec lambda_app() -> number().
lambda_app() ->
  Res = (fun
    Fib(N) when N < 2 ->
      1;
    Fib(N) ->
      Fib(N - 2) + Fib(N - 1)
  end)(4),
  Res.

-spec ex1_neg() ->
  fun ((string()) -> string()).
ex1_neg() ->
  fun(X) -> X + 1 end.

-spec ex2() ->
  fun((term()) -> term()) |
  fun ((string()) -> string()).
ex2() ->
  fun(X) -> X + 1 end.

-spec hd_invariant([fun((A) -> A)]) -> (fun((A) -> A)).
hd_invariant([F | _]) -> F.

-spec test_invariant() -> (fun((atom()) -> atom())).
test_invariant() -> hd_invariant([]).
