%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(app_env).

-compile([export_all, nowarn_export_all]).

-include_lib("check/include/my_header.hrl").

get_key_dynamic(App) ->
    application:get_env(App, key).

get_mine() ->
    application:get_env(misc, key).

steal() ->
    application:get_env(debug, key).
