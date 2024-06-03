%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(gradual_misc).

-compile([export_all, nowarn_export_all]).

% code is unsafe
-spec use_invalid_opaque_1(
    opaque:contravariant(a)
) -> opaque:contravariant(a | b).
use_invalid_opaque_1(X) ->
  X.

% code is safe but violates opacity
-spec use_invalid_opaque_2(
    opaque:contravariant(a | b)
) -> opaque:contravariant(a).
use_invalid_opaque_2(X) ->
  X.

-spec f(map()) -> ok.
f(#{(#{} =/= a) := _}) ->
  ok.
  
-spec g(map()) -> ok.
g(#{(a #{ b => c }) := _})  -> 
    ok.

-spec fuzz01() -> ok.
fuzz01() when #{(true andalso false) => {}} ->
    ok.

-spec fuzz02() -> ok.
fuzz02() ->
    <<X || X <- [], (X ++ X) >>.

-spec fuzz03(term()) -> ok.
fuzz03([_ | {}]) -> 
    ok.

-spec refine_tuple_neg(a | {b, c})
    -> a | {none()}.
refine_tuple_neg(T) when is_tuple(T) -> T;
refine_tuple_neg(T) -> T.

-record(rpc, {module :: module()}).

-type rpc_call() ::
  #rpc{} |
  {rpc, module(), atom()} |
  {rpc, module(), atom(), [term()]}.

-type call() :: binary() | {call, binary()} | rpc_call().

-spec maybe_rpc_call(call()) -> rpc_call() | undefined.
maybe_rpc_call(Call) when is_tuple(Call), element(1, Call) == rpc ->
  Call;
maybe_rpc_call(_) ->
  undefined.
