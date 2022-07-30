%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(booleans).
-compile([export_all, nowarn_export_all]).

-spec only_false() -> false.
only_false() -> false.

-spec only_true() -> true.
only_true() -> true.

-spec b() -> boolean().
b() -> false.

-spec dyn() -> eqwalizer:dynamic().
dyn() -> false.

-spec andalso1() -> false.
andalso1() ->
  only_false() andalso only_false().

-spec andalso2() -> false.
andalso2() ->
  only_false() andalso only_true().

-spec andalso3() -> false.
andalso3() ->
  only_true() andalso only_false().

-spec andalso4() -> true.
andalso4() ->
  only_true() andalso only_true().

-spec andalso5() -> 1.
andalso5() ->
  only_true() andalso 1.

-spec andalso6() -> false.
andalso6() ->
  only_false() andalso 1.

-spec andalso7_neg() -> boolean().
andalso7_neg() ->
  1 andalso b().

-spec andalso8() -> false.
andalso8() ->
  dyn() andalso only_false().

-spec andalso9() -> false.
andalso9() ->
  only_false() andalso dyn().

-spec andalso10_neg() -> true.
andalso10_neg() ->
  dyn() andalso only_true().

-spec andalso11() -> true.
andalso11() ->
  only_true() andalso dyn().

-type trill() :: true.

-spec make_trill() -> trill().
make_trill() -> true andalso true.

-spec andalso12() -> true.
andalso12() ->
  make_trill() andalso make_trill().
