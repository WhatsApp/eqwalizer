%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(eqwater_maps).

-compile([export_all, nowarn_export_all]).

-spec map_occ_01(#{a := integer()} | ok) -> ok.
map_occ_01(#{a := I}) when is_integer(I) -> ok;
map_occ_01(V) -> V.

-spec map_occ_02(#{a := term(), b := integer()})
    -> #{a := integer(), b := integer()}.
map_occ_02(M = #{a := I}) when is_integer(I) -> M;
map_occ_02(M) -> M#{a => 0}.

-spec map_occ_03(#{a => term(), b => integer()})
    -> #{a := integer(), b => integer()}.
map_occ_03(M = #{a := I}) when is_integer(I) -> M;
map_occ_03(M) -> M#{a => 0}.

-spec map_occ_04_neg(#{a => term(), b => integer()})
    -> #{a := integer(), b := integer()}.
map_occ_04_neg(M = #{a := I}) when is_integer(I) -> M;
map_occ_04_neg(M) -> M#{a => 0}.

-spec map_occ_05(#{a => integer(), b => integer()})
    -> #{a := integer(), b := integer()}.
map_occ_05(M = #{a := _, b := _}) -> M;
map_occ_05(M) -> M#{a => 0, b => 0}.

-spec map_occ_06_neg(#{a => integer()} | ok) -> ok.
map_occ_06_neg(#{a := I}) when is_integer(I) -> ok;
map_occ_06_neg(V) -> V.

-spec map_occ_07_neg
    (#{a := integer()} | #{c := integer()})
    -> #{b := integer()}.
map_occ_07_neg(#{a := _}) -> #{b => 0};
map_occ_07_neg(M) -> M.

-type foo() :: #{} | #{required := binary(), optional => binary()}.

-spec add_optional(foo(), binary()) -> foo().
add_optional(Foo = #{required := _R1}, Optional) ->
  Foo#{optional => Optional};
add_optional(Z, _) ->
  Z.
