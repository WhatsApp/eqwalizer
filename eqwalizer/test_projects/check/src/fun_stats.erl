%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(fun_stats).
% wy = well_typed yes
% wn = well-typed no

% ey = exported yes
% en = exported no

% sy = specced yes
% sn = specced no

-export([
  wy_ey_sy/0,
  wy_ey_sn/0,
  wn_ey_sy/0,
  wn_ey_sn/0,
  spec_dep/1
]).
-export_type([alias/0]).

% should be ignored in stats
-include("my_header.hrl").


-spec wy_ey_sy() -> ok.
wy_ey_sy() -> ok.

wy_ey_sn() -> ok.

-spec wn_ey_sy() -> ok.
wn_ey_sy() ->
  wn_ey_sy() + 3.

wn_ey_sn() ->
  _ = wy_en_sy(),
  _ = wy_en_sn(),
  _ = wn_en_sy(),
  _ = wn_en_sn(),
  wy_ey_sy() + 3.

wn_en_sy() ->
  wy_ey_sy() + 3.

-spec wy_en_sy() -> ok.
wy_en_sy() ->
  ok.

wy_en_sn() -> ok.

wn_en_sn() ->
  wy_ey_sy() + 3.

% for 'typeDependencies' stats
-record(rec, {
  field :: any_fun_type:f0()
}).

-type alias() :: opaque:opair(ok, ok).

-spec spec_dep({
    types1:my_opaque(),
    types2:recur()
}) -> ok.
spec_dep(_) ->
  _ = fun_stats2:errors(ok),
  ok.
