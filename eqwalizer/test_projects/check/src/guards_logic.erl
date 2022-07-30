%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(guards_logic).

-compile([export_all, nowarn_export_all]).

-spec test01(atom()) -> boolean().
test01(X) when X -> X;
test01(_) -> false.

-spec test02(atom()) -> boolean().
test02(X) when not X -> X;
test02(_) -> false.

-spec test03(atom(), atom()) -> boolean().
test03(X, _) when X -> X;
test03(_, Y) when Y -> Y.

-spec test04(atom(), atom()) -> boolean().
test04(X, Y) when X orelse Y -> X.

-spec test05(atom(), atom()) -> boolean().
test05(X, Y) when X andalso Y -> Y.

-spec test06(atom(), atom()) -> true.
test06(X, Y) when X andalso Y -> Y.

-spec test07_neg(atom(), atom()) -> false.
test07_neg(X, Y) when X andalso Y -> Y.

-spec test08_neg(any()) -> number().
test08_neg(X) when not is_number(X) -> X.

% We don't try to process
% logic connectives in a smart way
% in guards so far.
% Recommendation for users:
% rewrite it via true syntax of guards
-spec test09_neg (any())
              -> number() | atom().
test09_neg(X)
  when is_number(X) or is_atom(X) -> X.

-spec test10 (any())
      -> number() | atom().
test10(X)
  when is_number(X); is_atom(X) -> X.

-spec test11_neg (any())
      -> number() | atom().
test11_neg(X)
  when is_number(X);
       is_atom(X);
       is_pid(X)-> X.

-spec test12(any(), any()) -> number().
test12(X, _) when X + 1 > 0 -> X;
test12(_, Y) when Y / 3 > 1 -> Y.

-spec test13 (any(), any())
          -> {number(), number()}.
test13(X, Y)
    when X + 1 > 0, Y / 3 > 1 ->
    {X, Y}.

-spec test14_neg (any(), any())
        -> {number(), number()}.
test14_neg(X, Y)
    when X + 1 > 0; Y / 3 > 1 ->
    {X, Y}.

-spec test15 (any(), any())
         -> {number(), number()}
          | {boolean(), boolean()}.
test15(X, Y) when X + Y > 0 -> {X, Y};
test15(X, Y) when X, Y -> {X, Y}.

-spec test16(term()) -> list().
test16(X) when true and is_list(X) ->
  X.

-spec test17(atom(), atom()) -> boolean().
test17(X, Y) when X and Y == false -> Y.

-spec test18(atom(), atom()) -> boolean().
test18(X, Y) when X or Y -> Y.
