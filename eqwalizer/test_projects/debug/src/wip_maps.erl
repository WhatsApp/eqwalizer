%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(wip_maps).

-compile([export_all, nowarn_export_all]).

% only atom keys can be updated
% unconditionally
-spec update_req_non_atom_neg
    (map()) -> map().
update_req_non_atom_neg(M) ->
    M#{1 := 1}.

% "mixed" updates are not supported
% (they are not used in WA codebase)
-spec bad_mixed_update1
    (#{a := term()}) -> #{a := a, b := b}.
bad_mixed_update1(M) ->
    M#{a := a, b => b}.

-spec bad_mixed_update2
    (#{a := term()}) -> #{a := a, b := b}.
bad_mixed_update2(M) ->
    M#{b => b, a := a}.

-spec bad_mixed_update3
    (any()) -> term().
bad_mixed_update3(M)
    when is_map(M#{a := a, b => b}) -> M.

-spec bad_mixed_update4
    (any()) -> term().
bad_mixed_update4(M)
    when is_map(M#{b => b, a := a}) -> M.
