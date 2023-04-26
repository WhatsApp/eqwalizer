%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(result).

-export_type([result/2]).

-export([with_default/2, with_default/1]).

-export([map/2, map/1, map2/3, map2/1]).

-export([and_then/2,
         and_then/1,
         map_error/2,
         map_error/1]).

-export([to_maybe/1, from_maybe/2, from_maybe/1]).

-export([is_ok/1]).

-import_type({'maybe', [{'maybe', 1}]}).

-type result(Error, Value) :: {'$#result:result.ok',
                               Value} |
                              {'$#result:result.err', Error}.

-spec with_default(A, result(_, A)) -> A.

with_default(_Def, {'$#result:result.ok', A}) -> A;
with_default(Def, {'$#result:result.err', _E}) -> Def.

-spec with_default(A) -> fun((result(_, A)) -> A).

with_default(Def) ->
    fun (Res) -> with_default(Def, Res) end.

-spec map(fun((A) -> B), result(X, A)) -> result(X, B).

map(Func, {'$#result:result.ok', A}) ->
    {'$#result:result.ok', Func(A)};
map(_Func, {'$#result:result.err', E}) ->
    {'$#result:result.err', E}.

-spec map(fun((A) -> B)) -> fun((result(X,
                                        A)) -> result(X, B)).

map(Func) -> fun (Res) -> map(Func, Res) end.

-spec map2(fun((A, B) -> C), result(X, A),
           result(X, B)) -> result(X, C).

map2(_Func, {'$#result:result.err', X}, _) ->
    {'$#result:result.err', X};
map2(_Func, {'$#result:result.ok', _},
     {'$#result:result.err', X}) ->
    {'$#result:result.err', X};
map2(Func, {'$#result:result.ok', A},
     {'$#result:result.ok', B}) ->
    {'$#result:result.ok', Func(A, B)}.

-spec map2(fun((A, B) -> C)) -> fun((result(X, A),
                                     result(X, B)) -> result(X, C)).

map2(Func) ->
    fun (ResA, ResB) -> map2(Func, ResA, ResB) end.

-spec and_then(fun((A) -> result(X, B)),
               result(X, A)) -> result(X, B).

and_then(Callback, {'$#result:result.ok', Value}) ->
    Callback(Value);
and_then(_Callback, {'$#result:result.err', Msg}) ->
    {'$#result:result.err', Msg}.

-spec and_then(fun((A) -> result(X,
                                 B))) -> fun((result(X, A)) -> result(X, B)).

and_then(Callback) ->
    fun (Res) -> and_then(Callback, Res) end.

-spec map_error(fun((X) -> Y),
                result(X, A)) -> result(Y, A).

map_error(_F, {'$#result:result.ok', V}) ->
    {'$#result:result.ok', V};
map_error(F, {'$#result:result.err', E}) ->
    {'$#result:result.err', F(E)}.

-spec map_error(fun((X) -> Y)) -> fun((result(X,
                                              A)) -> result(Y, A)).

map_error(F) -> fun (Res) -> map_error(F, Res) end.

-spec to_maybe(result(_, A)) -> 'maybe':'maybe'(A).

to_maybe({'$#result:result.ok', V}) ->
    {'$#maybe:maybe.just', V};
to_maybe({'$#result:result.err', _}) ->
    {'$#maybe:maybe.nothing'}.

-spec from_maybe(X, 'maybe':'maybe'(A)) -> result(X, A).

from_maybe(_Err, {'$#maybe:maybe.just', V}) ->
    {'$#result:result.ok', V};
from_maybe(Err, {'$#maybe:maybe.nothing'}) ->
    {'$#result:result.err', Err}.

-spec from_maybe(X) -> fun(('maybe':'maybe'(A)) -> result(X,
                                                      A)).

from_maybe(Err) ->
    fun (Maybe) -> from_maybe(Err, Maybe) end.

-spec is_ok(result(_, _)) -> boolean().

is_ok({'$#result:result.ok', _}) -> true;
is_ok({'$#result:result.err', _}) -> false.



