%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(redundant_guards).

-compile([export_all, nowarn_export_all]).

-spec redundant_guard_1(ok) -> ok.
redundant_guard_1(X) ->
    (is_atom(X) orelse error(fail)),
    X.

-spec redundant_guard_2(ok) -> ok.
redundant_guard_2(X) ->
    (X =:= ok orelse error(fail)),
    X.
    
-spec redundant_guard_3(ok) -> ok.
redundant_guard_3(X) ->
    ((is_atom(X) andalso X =:= ok) orelse error(fail)),
    X.

-spec redundant_guard_4(ok) -> ok.
redundant_guard_4(X) ->
    ((ok =:= X andalso is_atom(X)) orelse error(fail)),
    X.

-spec non_redundant_guard(atom()) -> ok.
non_redundant_guard(X) ->
    (X =:= ok orelse error(fail)),
    X.
