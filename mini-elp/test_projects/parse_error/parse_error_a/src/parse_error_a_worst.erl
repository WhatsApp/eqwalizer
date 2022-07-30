%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(parse_error_a_worst).
-compile([export_all, nowarn_export_all]).

-record(server_opts,
{port, ip = "127.0.0.1", max_connections = 10}).

Opts1 = #server_opts{port=80}. %syntax error before: Opts1