%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(other).

-compile([export_all, nowarn_export_all]).

% occurrence typing
-spec clauses(term()) ->
    atom() | number().
clauses(X) ->
    case X of
        ok -> X;
        1 -> X
    end.

-spec inter
    (ok) -> nok;
    (nok) -> ok.
inter(ok) -> nok;
inter(nok) -> ok.

-spec slice_inter1(ok) -> nok.
slice_inter1(ok) -> inter(ok).

-spec slice_inter2(ok) -> nok.
slice_inter2(ok) ->
    other:inter(ok).

-spec get_inter1() ->
    term().
get_inter1() ->
    fun inter/1.

-spec get_inter2() ->
    term().
get_inter2() ->
    fun other:inter/1.

% logger:metadata() is automatically
% approximated
-spec any_as_metadata_neg(term()) ->
    logger:metadata().
any_as_metadata_neg(M) -> M.

-spec elab_var_scope(term()) -> term().
elab_var_scope(_) ->
    (begin X = 3, X, fun () -> ok end end)(),
    X.

-spec map_with_dup_keys(#{ak => av1, bk => bv1, ak => av2}) -> #{ak => av1, bk => bv1}.
map_with_dup_keys(M) -> M.
