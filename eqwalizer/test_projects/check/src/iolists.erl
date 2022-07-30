%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(iolists).

-compile([export_all, nowarn_export_all]).

-spec mk_io_list1(
    byte() | binary() | iolist()
) -> iolist().
mk_io_list1(X) ->
    [X].

-spec first(iolist()) ->
byte() | binary() | iolist().
first(IoList)
    when is_binary(IoList) -> IoList;
first([H|_]) -> H.

-spec refine_as_list(iolist()) ->
byte() | binary() | iolist().
refine_as_list(IoList)
    when is_list(IoList) ->
    IoList;
refine_as_list(IoList)
    when is_binary(IoList) ->
    binary_to_list(IoList).

-spec refine1([any()], iolist()) ->
[byte() | binary() | iolist()].
refine1(X, X) -> X.

-spec refine2(iolist(), [any()]) ->
    [byte() | binary() | iolist()].
refine2(X, X) -> X.

-spec refine3(any(), iolist()) ->
    [byte() | binary() | iolist()].
refine3(X, X) -> X.

-spec refine4(iolist(), any()) ->
    [byte() | binary() | iolist()].
refine4(X, X) -> X.

-spec refine5(
    iolist(), [atom() | binary()]
) -> [binary()].
refine5(X, X) -> X.

-spec refine6_neg(
    iolist(), [atom() | binary()]
) -> [atom()].
refine6_neg(X, X) -> X.

-spec refine_to_empty1(
    iolist(), [atom()]
) -> [].
refine_to_empty1(X, X) -> X.

-spec refine_to_empty2(
    [atom()], iolist()
) -> [].
refine_to_empty2(X, X) -> X.

-spec head_or([A], A) -> A.
head_or([A], _) -> A;
head_or([_], A) -> A.

-spec io_list_head(iolist()) ->
    binary() | iolist() | number().
io_list_head(X) when is_list(X)
    -> head_or(X, X).

-spec ioio(iolist(), A) -> A.
ioio(_, A) ->  A.

-spec test() -> atom().
test() -> ioio([<<>>], ok).

-spec test2_neg(iolist()) -> wrong_ret.
test2_neg(X) -> X.