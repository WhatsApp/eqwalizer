%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(basics).

-eqwalizer_unchecked([]).

-export([add/2, add/1, sub/1, mul/2, mul/1, idiv/1]).

-export([eq/2, eq/1, neq/2, neq/1]).

-export_type([order/0]).

-export([lt/2,
         lt/1,
         gt/2,
         gt/1,
         le/2,
         le/1,
         ge/2,
         ge/1,
         min/2,
         min/1,
         max/2,
         max/1,
         compare/2,
         compare/1]).

-export(['not'/1,
         'and'/2,
         'and'/1,
         'or'/2,
         'or'/1,
         'xor'/2,
         'xor'/1]).

-export([mod_by/2,
         mod_by/1,
         remainder_by/2,
         remainder_by/1,
         negate/1,
         abs/1,
         clamp/3]).

-export_type([never/0]).

-export([composeL/2,
         composeL/1,
         composeR/2,
         composeR/1,
         apR/2,
         apR/1,
         apL/2,
         apL/1,
         identity/1,
         always/2,
         always/1,
         never/1]).

-spec add(integer(), integer()) -> integer().

add(X1, X2) -> X1 + X2.

-spec add(integer()) -> fun((integer()) -> integer()).

add(X1) -> fun (X2) -> add(X1, X2) end.

-spec sub(integer(), integer()) -> integer().

sub(X1, X2) -> X1 - X2.

-spec sub(integer()) -> fun((integer()) -> integer()).

sub(X1) -> fun (X2) -> sub(X1, X2) end.

-spec mul(integer(), integer()) -> integer().

mul(X1, X2) -> X1 * X2.

-spec mul(integer()) -> fun((integer()) -> integer()).

mul(X1) -> fun (X2) -> mul(X1, X2) end.

-spec idiv(integer(), integer()) -> integer().

idiv(X1, X2) -> X1 div X2.

-spec idiv(integer()) -> fun((integer()) -> integer()).

idiv(X1) -> fun (X2) -> idiv(X1, X2) end.

-spec eq(A, A) -> boolean().

eq(X1, X2) -> X1 == X2.

-spec eq(A) -> fun((A) -> boolean()).

eq(X1) -> fun (X2) -> eq(X1, X2) end.

-spec neq(A, A) -> boolean().

neq(X1, X2) -> X1 =/= X2.

-spec neq(A) -> fun((A) -> boolean()).

neq(X1) -> fun (X2) -> neq(X1, X2) end.

-spec lt(A, A) -> boolean().

lt(X1, X2) -> X1 < X2.

-spec lt(A) -> fun((A) -> boolean()).

lt(X1) -> fun (X2) -> lt(X1, X2) end.

-spec gt(A, A) -> boolean().

gt(X1, X2) -> X1 > X2.

-spec gt(A) -> fun((A) -> boolean()).

gt(X1) -> fun (X2) -> gt(X1, X2) end.

-spec le(A, A) -> boolean().

le(X1, X2) -> X1 =< X2.

-spec le(A) -> fun((A) -> boolean()).

le(X1) -> fun (X2) -> le(X1, X2) end.

-spec ge(A, A) -> boolean().

ge(X1, X2) -> X1 >= X2.

-spec ge(A) -> fun((A) -> boolean()).

ge(X1) -> fun (X2) -> ge(X1, X2) end.

-spec min(A, A) -> A.

min(X1, X2) ->
    case lt(X1, X2) of
        true -> X1;
        false -> X2
    end.

-spec min(A) -> fun((A) -> A).

min(X1) -> fun (X2) -> basics:min(X1, X2) end.

-spec max(A, A) -> A.

max(X1, X2) ->
    case gt(X1, X2) of
        true -> X1;
        false -> X2
    end.

-spec max(A) -> fun((A) -> A).

max(X1) -> fun (X2) -> basics:max(X1, X2) end.

-type order() :: {'$#basics:order.lt'} |
                 {'$#basics:order.eq'} |
                 {'$#basics:order.gt'}.

-spec compare(A, A) -> order().

compare(X1, X2) ->
    case lt(X1, X2) of
        true -> {'$#basics:order.lt'};
        false ->
            case eq(X1, X2) of
                true -> {'$#basics:order.eq'};
                false -> {'$#basics:order.gt'}
            end
    end.

-spec compare(A) -> fun((A) -> order()).

compare(X1) -> fun (X2) -> compare(X1, X2) end.

-spec 'not'(boolean()) -> boolean().

'not'(B) -> not B.

-spec 'and'(boolean(), boolean()) -> boolean().

'and'(B1, B2) -> B1 and B2.

-spec 'and'(boolean()) -> fun((boolean()) -> boolean()).

'and'(B1) -> fun (B2) -> 'and'(B1, B2) end.

-spec 'or'(boolean(), boolean()) -> boolean().

'or'(B1, B2) -> B1 or B2.

-spec 'or'(boolean()) -> fun((boolean()) -> boolean()).

'or'(B1) -> fun (B2) -> 'or'(B1, B2) end.

-spec 'xor'(boolean(), boolean()) -> boolean().

'xor'(B1, B2) -> B1 xor B2.

-spec 'xor'(boolean()) -> fun((boolean()) -> boolean()).

'xor'(B1) -> fun (B2) -> 'xor'(B1, B2) end.

-spec mod_by(integer(), integer()) -> integer().

mod_by(X1, X2) -> X2 div X1.

-spec
     mod_by(integer()) -> fun((integer()) -> integer()).

mod_by(X1) -> fun (X2) -> mod_by(X1, X2) end.

-spec remainder_by(integer(), integer()) -> integer().

remainder_by(X1, X2) -> X2 rem X1.

-spec
     remainder_by(integer()) -> fun((integer()) -> integer()).

remainder_by(X1) ->
    fun (X2) -> remainder_by(X1, X2) end.

-spec negate(integer()) -> integer().

negate(X) -> -X.

-spec abs(integer()) -> integer().

abs(X) ->
    case lt(X, 0) of
        true -> -X;
        false -> X
    end.

-spec clamp(integer(), integer(),
            integer()) -> integer().

clamp(Low, High, Num) ->
    case lt(Num, Low) of
        true -> Low;
        false ->
            case gt(Num, High) of
                true -> High;
                false -> Num
            end
    end.

-spec composeL(fun((B) -> C),
               fun((A) -> B)) -> fun((A) -> C).

composeL(G, F) -> fun (X) -> G(F(X)) end.

-spec
     composeL(fun((B) -> C)) -> fun((fun((A) -> B)) -> fun((A) -> C)).

composeL(G) -> fun (F) -> composeL(G, F) end.

-spec composeR(fun((A) -> B),
               fun((B) -> C)) -> fun((A) -> C).

composeR(F, G) -> fun (X) -> G(F(X)) end.

-spec
     composeR(fun((A) -> B)) -> fun((fun((B) -> C)) -> fun((A) -> C)).

composeR(F) -> fun (G) -> composeR(F, G) end.

-spec apR(A, fun((A) -> B)) -> B.

apR(X, F) -> F(X).

-spec apR(A) -> fun((fun((A) -> B)) -> B).

apR(X) -> fun (F) -> apR(X, F) end.

-spec apL(fun((A) -> B), A) -> B.

apL(F, X) -> F(X).

-spec apL(fun((A) -> B)) -> fun((A) -> B).

apL(F) -> fun (X) -> apL(F, X) end.

-spec identity(A) -> A.

identity(X) -> X.

-spec always(A, _) -> A.

always(A, _) -> A.

-spec always(A) -> fun((_) -> A).

always(A) -> fun (X) -> always(A, X) end.

-type never() :: {'$#basics:never.just_one_more',
                  never()}.

-spec never(never()) -> _.

never({'$#basics:never.just_one_more', Nvr}) ->
    never(Nvr).



