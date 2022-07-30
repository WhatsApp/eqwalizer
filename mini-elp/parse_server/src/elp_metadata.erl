%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(elp_metadata).
-export([insert_metadata/2]).

insert_metadata(Metadata, Forms) ->
    Attr = {attribute, {1, 1}, elp_metadata, Metadata},
    insert_attr_after_module_attr(Attr, Forms).

% invariants tested by parse_server_verify
insert_attr_after_module_attr(Attr, Forms) ->
    Reversed = lists:foldl(
        fun (ModuleAttr = {attribute, _, module, _}, Acc) ->
            [Attr, ModuleAttr] ++ Acc;
        (Form, Acc) ->
            [Form | Acc]
        end,
        [],
        Forms
    ),
    lists:reverse(Reversed).
