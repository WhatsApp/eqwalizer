%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(pinned).
-spec pin([number()], [number()]) -> boolean().
pin(L, N) ->
  X = 1,
  Z = 2,
  case N of
    _ -> true;
    Z -> false
  end,
  case L of
  [Y | _] -> true;
  [X] -> false;
  _ -> false
  end.
-spec pin([number()]) -> boolean().
pin(L) ->
  X = 1,
  case L of
    X -> true;
    [X | _] -> false;
    _ -> false
  end.
-spec pin2([number()], number(), [number()]) -> boolean().
pin2(List, Head, Tail) ->
  case List of
    % Head and Tail are already defined -> they are "pinned" here
    [Head | Tail] -> true;
    _ -> false
  end.
-spec pin3([number()], number()) -> atom().
pin3(List, Val) ->
 % Head and Tail are now bound
 [Head | Tail] = List,
 case Val of
   % Matching against a bound var
   Head -> head;
   % Matching against a bound var
   Tail -> tail
 end.
-spec pin4([number()], number()) -> atom().
pin4(List, Val) ->
 case List of
   % Head and Tail are now bound
   [Head | Tail] ->
     case Val of
       % Matching against a bound var
       Head -> head;
       % Matching against a bound var
       Tail -> tail
     end
 end.
