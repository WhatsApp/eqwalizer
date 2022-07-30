%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(vars1).
-compile([export_all, nowarn_export_all]).

-spec remove(Elem, [Elem]) -> [Elem].
remove(Elem, List) ->
  remove(Elem, List, false).

-spec remove
    (Elem, [Elem], false) -> [Elem];
    (Elem, [Elem], true) -> {notfound | found, [Elem]}.
remove(Elem, List, WithStatus) ->
  remove(Elem, List, WithStatus, []).

-spec remove
    (Elem, [Elem], false, [Elem]) -> [Elem];
    (Elem, [Elem], true, [Elem]) -> {notfound | found, [Elem]}.
remove(_Elem, [],       true,       Acc) -> {notfound, lists:reverse(Acc)};
remove(_Elem, [],       false,      Acc) ->            lists:reverse(Acc);
remove( Elem, [Elem|T], true,       Acc) -> {found,    lists:reverse(Acc, T)};
remove( Elem, [Elem|T], false,      Acc) ->            lists:reverse(Acc, T);
remove( Elem, [I|T],    WithStatus, Acc) -> remove(Elem, T, WithStatus, [I|Acc]).
