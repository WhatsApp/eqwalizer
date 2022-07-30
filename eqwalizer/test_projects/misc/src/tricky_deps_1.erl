%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(tricky_deps_1).

-export([id_dep/1]).
-export_type([dep/0]).

-type dep() :: ok.

-spec id_dep(dep()) -> dep().
id_dep(ok) -> ok.


