%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(records_wip).

-compile([export_all, nowarn_export_all]).
-export_type([
    func_rec1/0
    , int_box/0, aliases_fields/0, ty_var_in_field/0, ty_var_in_field2/0, ty_var_in_field3/0]).

-record(func_rec, {
    func :: fun(() -> 3)
}).

-record(func_rec1, {
    func =
        fun() -> 3 end
        :: fun(() -> 3)
}).
-type func_rec1() :: #func_rec1{}.

-spec fun_call(#func_rec{}) -> number().
fun_call(X) -> (X#func_rec.func)().

-record(any_box, {inner :: term()}).

%% "Refined" record type
-type int_box() ::
    #any_box{inner :: integer()}.

-record(many_fields, {
    f1 :: atom(),
    f2 :: atom(),
    f3 :: atom()
}).

-spec mk_many_fields() ->
    #many_fields{}.
mk_many_fields() ->
    #many_fields{f1 = a, _ = b}.

-spec many_fields_pats
    (#many_fields{}) -> ok.
many_fields_pats(#many_fields{_ = b}) ->
    ok.

-record(aliased_fields, {
    field1 :: {
        First :: term(),
        Second :: term()
    }
}).
-type aliases_fields() :: #aliased_fields{}.

% These should be last in the
% file, they have long messages
-record(ty_var_in_field, {
    field :: _TyVar
}).
-type ty_var_in_field() :: #ty_var_in_field{}.

-record(ty_var_in_field2, {
    field ::
        _TyVar
}).
-type ty_var_in_field2() :: #ty_var_in_field2{}.

-type var_from_nowhere() :: _T.

-record(ty_var_in_field3, {
    field ::
        var_from_nowhere()
}).
-type ty_var_in_field3() :: #ty_var_in_field3{}.
