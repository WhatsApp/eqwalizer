%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(eqwalizer).

-export_type([dynamic/0, dynamic/1, refinable/1]).
-export([dynamic_cast/1, fix_me/1, reveal_type/1]).

%% @doc
%% This type is intended to help with code being transitioned
%% from untyped/unchecked mode to gradual mode.
%% Values of dynamic types slip through
%% the fingers of eqWAlizer in GRADUAL mode.
%% dynamic() is a special type which is a subtype and a supertype of all types.
%% It is defined as an alias to term() to be friendly
%% for other tooling (as dialyzer).
%% In strict mode dynamic() is treated by eqWAlizer as an alias to term().
%% @end
-type dynamic() :: term().

%% @doc
%% This type is a more precise version of dynamic/0, that introduces
%% some degree of soundness while retaining the flexibility of dynamic/0.
%% dynamic(T) is a subtype of everything, but a supertype of T
%% (and its subtypes) only.
%% It behaves as "T in, dynamic() out".
%% For other tooling, dynamic(T) is defined as an alias to T.
%% @end
-type dynamic(T) :: T.

%% @doc
%% "Cast" function to convert values to eqwalizer:dynamic() type.
%% It communicates the intent: "I know that the value would be of the right type".
%% @end
-spec dynamic_cast(term()) -> eqwalizer:dynamic().
dynamic_cast(X) -> X.

%% @doc
%% "Cast" function to convert values to eqwalizer:dynamic() type.
%% It communicates the intent: "This code should be fixed" (later).
%% @end
-spec fix_me(term()) -> eqwalizer:dynamic().
fix_me(X) -> X.

%% @doc
%% A utility function for helping debug type-related errors.
%% You can use eqwalizer:reveal_type(Expr) to ask eqWAlizer to display
%% the inferred static type of an expression.
%% This can be useful when you don't quite understand how
%% eqWAlizer handles a particular piece of code.
%% @end
-spec reveal_type(term()) -> none().
reveal_type(_Expr) -> error(eqwalizer_reveal_type).

%% This type can be used in a record type declaration to specify
%% that the type of a field can be further refined.
%% For example, -record(rec, {id :: eqwalizer:refinable(any()), id2 :: string()})
%% creates a record type rec where the field id can be refined later, by
%% writing for example #rec{id :: atom()} in a spec.
%% While this is not mandatory, using this type will make the type-checker
%% smarter about refined record types.
-type refinable(A) :: A.
