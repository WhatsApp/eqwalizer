%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(eqwater_aliases).

-compile([export_all, nowarn_export_all]).

-type occ01_in() :: a | b.
-type occ01_out() :: b.

-spec occ01
(occ01_in()) -> occ01_out().
occ01(Arg) ->
    case Arg of
        a -> b;
        B -> B
    end.

-spec occ01_cl
(occ01_in()) -> occ01_out().
occ01_cl(a) -> b;
occ01_cl(B) -> B.


-type occ02_in() :: a | {b, integer()}.
-type occ02_out() :: integer().

-spec occ02
(occ02_in()) -> occ02_out().
occ02(Arg) ->
    case Arg of
        a -> 0;
        {_, I} -> I
    end.

-spec occ02_cl
    (occ02_in()) -> occ02_out().
occ02_cl(a) -> 0;
occ02_cl({_, I}) -> I.
