%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(eqwalizer).

-export_type([dynamic/0]).
-export([dynamic_cast/1, fix_me/1]).

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
%% "Cast" function to convert values to eqwalizer:dynamic() type.
%% It communicates the intent: "I know that the value would be of the right type".
%% @end
-spec dynamic_cast(term()) -> dynamic().
dynamic_cast(X) -> X.

%% @doc
%% "Cast" function to convert values to eqwalizer:dynamic() type.
%% It communicates the intent: "This code should be fixed" (later).
%% @end
-spec fix_me(term()) -> dynamic().
fix_me(X) -> X.
