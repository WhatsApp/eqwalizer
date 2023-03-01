%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(elab_clause).

-compile([export_all, nowarn_export_all]).

-spec foo(term()) -> {atom(), atom()}.
foo(_) -> {any, any}.

-spec bar(term()) ->
    {atom(), atom()} | {number()}.
bar(true) -> {any, any};
bar(_) -> {0}.

-spec app_foo(term()) -> atom().
app_foo(X) ->
    Res = case foo(X) of
              {Y, _} -> Y
          end,
    Res.

-spec app_foo_neg(term()) -> binary().
app_foo_neg(X) ->
    Res = case foo(X) of
              {_, Y} -> Y
          end,
    Res.

-spec app_bar(term()) ->
    {atom()} | number().
app_bar(X) ->
    Res = case bar(X) of
              {Y, _} -> {Y};
              {N} -> N
          end,
    Res.

-spec app_bar_neg(term()) ->
    {atom()} | number().
app_bar_neg(X) ->
    Res = case bar(X) of
              {Y, _} -> Y;
              {N} -> {N}
          end,
    Res.

-spec catch_foo(term()) -> atom().
catch_foo(X) ->
    Res =
        try foo(X)
        of {Y, _} -> Y
        catch
            A:_  -> A
        end,
    Res.

-spec catch_foo1(term()) -> atom().
catch_foo1(X) ->
    try foo(X)
    of {Y, _} -> Y
    catch
        A:_  -> A
    end.

-spec catch_foo1_neg(term()) -> number().
catch_foo1_neg(X) ->
    Res =
        try foo(X)
        of {_Y, _} -> 1
        catch
            A:_  -> A
        end,
    Res.

-spec catch_foo2_neg(term()) -> number().
catch_foo2_neg(X) ->
    Res =
        try foo(X)
        of {_Y, _} -> 1
        catch
            _:_:Stack  -> Stack
        end,
    Res.

-spec catch_foo3_neg(term()) -> number().
catch_foo3_neg(X) ->
    try foo(X)
    of {_Y, _} -> 1
    catch
        A:_  -> A
    end.

-spec catch_foo4_neg(term()) -> number().
catch_foo4_neg(X) ->
    try foo(X)
    of {_Y, _} -> 1
    catch
        Class:_:Stack  -> {Class, Stack}
    end.
