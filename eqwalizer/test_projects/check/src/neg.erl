%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(neg).

-compile([export_all, nowarn_export_all]).

%% Gradualizer:
%% https://git.io/JLjXd
-spec foo(A) -> A.
foo(X) -> {X, X}.

-spec atom_id_e(atom()) -> number().
atom_id_e(X) ->
    X.

-spec foo(term(), A) -> A.
foo(X, _) -> X.

-spec match(term()) -> {atom(), atom()}.
match(X) ->
    {ok, _} = X.

-spec concat1() -> list().
concat1() -> [1] ++ 1.

-spec concat2(X, X) -> list().
concat2(X, X) -> [1] ++ X.

-spec concat3(X, X) -> list().
concat3(X, X) -> [1] ++ X.
