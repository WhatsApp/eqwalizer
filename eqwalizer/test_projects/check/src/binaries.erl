%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(binaries).

-compile([export_all, nowarn_export_all]).

-spec test01_pos() -> binary().
test01_pos() ->
    <<>>.

-spec test02_pos(binary())
    -> {number(), binary()}.
test02_pos(<<X, Y/binary>>) -> {X, Y}.

-spec test03_pos(binary())
    -> binary().
test03_pos(<<N, _:N, Rest/binary>>) ->
    Rest.

-spec test04_neg() -> list(term()).
test04_neg() -> <<>>.

-spec test05_pos(binary())
        -> {binary(), binary()}.
test05_pos(<<H:42/binary, Rest/binary>>) ->
    {H, Rest}.

-spec test05_neg(atom()) -> binary().
test05_neg(A) -> <<A/signed>>.

-spec test06_neg(atom(), integer()) -> binary().
test06_neg(A, S) -> <<A:S>>.

-spec test07_neg(atom()) -> binary().
test07_neg(A) -> [A].

-spec test08_neg(binary()) -> float().
test08_neg(<<F/float, _R/binary>>) -> F.

-spec test09(integer(), integer())
        -> binary().
test09(I1, I2) ->
    X = <<I1, I2>>,
    X.

-spec string_lit() -> binary().
string_lit() ->
    <<"binary">>.

-spec string_lit_neg() -> binary().
string_lit_neg() ->
    <<"binary"/binary>>.

-spec special_syntax1() -> <<>>.
special_syntax1() -> <<>>.

-spec special_syntax2() -> <<_:_*8>>.
special_syntax2() -> <<>>.

-spec special_syntax3() -> <<_:8, _:_*8>>.
special_syntax3() -> <<"binary">>.

-spec convert(binary()) -> number().
convert(Bits) ->
    <<X:(bit_size(Bits))>> = Bits,
    X.

-spec convert_neg1
    (binary(), atom()) -> number().
convert_neg1(Bits, A) ->
    <<X:A>> = Bits,
    X.

-spec convert_neg2
    (binary()) -> number().
convert_neg2(Bits) ->
    <<X:(self())>> = Bits,
    X.

-spec string_lit2() -> binary().
string_lit2() ->
    <<"">>.

-spec empty_list_neg() -> binary().
empty_list_neg() ->
    <<[]>>.
