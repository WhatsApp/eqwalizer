%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(types2).

-export_type([
    my_remote_number/0,
    unknown_remote_number/0,
    my_boolean_unary_fun/0,
    o/1,
    recur/0,
    recur_invalid/1
]).

-type my_remote_number() ::
    forms1:my_number().

-type unknown_remote_number() ::
    unknown:my_number().

-type my_boolean_unary_fun() ::
types1:my_unary_fun(boolean(), boolean()).

-opaque o(X) :: {X}.

-type recur() :: {recur()} | o(x).

-type recur_invalid(X) :: o(
  recur_invalid(recur_invalid(X))
).
