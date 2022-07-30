%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

%%%-------------------------------------------------------------------
%%% @doc
%%% This module provides a means to override specs from standard OTP
%%% libraries for better type-checking with eqWAlizer.
%%% @end
%%%-------------------------------------------------------------------
-module(eqwalizer_specs).
-compile(warn_missing_spec).
-compile([export_all, nowarn_export_all]).

%% -------- application --------

-spec 'application:get_env'(Param :: atom()) -> undefined | {ok, eqwalizer:dynamic()}.
'application:get_env'(_) -> error(eqwalizer_specs).

-spec 'application:get_env'(App :: atom(), Param :: atom()) -> undefined | {ok, eqwalizer:dynamic()}.
'application:get_env'(_, _) -> error(eqwalizer_specs).

-spec 'application:get_env'(App :: atom(), Param :: atom(), Default :: term()) -> eqwalizer:dynamic().
'application:get_env'(_, _, _) -> error(eqwalizer_specs).

-spec 'application:get_key'(Key :: atom()) -> undefined | {ok, eqwalizer:dynamic()}.
'application:get_key'(_) -> error(eqwalizer_specs).

-spec 'application:get_key'(App :: atom(), Key :: atom()) -> undefined | {ok, eqwalizer:dynamic()}.
'application:get_key'(_, _) -> error(eqwalizer_specs).

%% -------- code --------

-spec 'code:priv_dir'(atom()) -> file:filename().
'code:priv_dir'(_) -> error(eqwalizer_specs).

%% -------- crypto --------

-type crypto_key_integer() :: integer() | binary().
-type crypto_dh_params() :: [crypto_key_integer()].
-type crypto_ecdh_params() :: [crypto:ec_named_curve() | x25519 | x448].
-type crypto_rsa_params() :: {integer(), crypto_key_integer()}.
-spec 'crypto:generate_key'
    (dh, crypto_dh_params()) -> {binary(), binary()};
    (ecdh, crypto_ecdh_params()) -> {binary(), binary()};
    (rsa, crypto_rsa_params()) -> {[binary()], [binary()]}.
'crypto:generate_key'(_, _) -> error(eqwalizer_specs).

%% -------- erlang --------

-spec 'erlang:abs'(number()) -> number().
'erlang:abs'(_) -> error(eqwalizer_specs).

-spec 'erlang:apply'(Fun, Args) -> eqwalizer:dynamic() when
    Fun :: function(),
    Args :: [term()].
'erlang:apply'(_, _) -> error(eqwalizer_specs).

-spec 'erlang:apply'(Module, Function, Args) -> eqwalizer:dynamic() when
    Module :: module(),
    Function :: atom(),
    Args :: [term()].
'erlang:apply'(_, _, _) -> error(eqwalizer_specs).

-spec 'erlang:binary_to_term'(binary()) -> eqwalizer:dynamic().
'erlang:binary_to_term'(_) -> error(eqwalizer_specs).

-spec 'erlang:element'(pos_integer(), tuple()) -> eqwalizer:dynamic().
'erlang:element'(_, _) -> error(eqwalizer_specs).

-spec 'erlang:hd'([A, ...]) -> A.
'erlang:hd'(_) -> error(eqwalizer_specs).

-spec 'erlang:max'(A, B) -> A | B.
'erlang:max'(_, _) -> error(eqwalizer_specs).

-spec 'erlang:min'(A, B) -> A | B.
'erlang:min'(_, _) -> error(eqwalizer_specs).

-spec 'erlang:system_time'() -> pos_integer().
'erlang:system_time'() -> error(eqwalizer_specs).

-spec 'erlang:system_time'(erlang:time_unit()) -> pos_integer().
'erlang:system_time'(_) -> error(eqwalizer_specs).

-spec 'erlang:tuple_to_list'(tuple()) -> [eqwalizer:dynamic()].
'erlang:tuple_to_list'(_) -> error(eqwalizer_specs).

-spec 'erlang:get'() -> [{eqwalizer:dynamic(), eqwalizer:dynamic()}].
'erlang:get'() -> error(eqwalizer_specs).

-spec 'erlang:get'(term()) -> eqwalizer:dynamic().
'erlang:get'(_) -> error(eqwalizer_specs).

-spec 'erlang:put'(term(), term()) -> eqwalizer:dynamic().
'erlang:put'(_, _) -> error(eqwalizer_specs).

-spec 'erlang:erase'() -> [{eqwalizer:dynamic(), eqwalizer:dynamic()}].
'erlang:erase'() -> error(eqwalizer_specs).

-spec 'erlang:erase'(term()) -> eqwalizer:dynamic().
'erlang:erase'(_) -> error(eqwalizer_specs).

-spec 'erlang:raise'(Class, Reason, Stacktrace) -> none() when
    Class :: 'error' | 'exit' | 'throw',
    Reason :: term(),
    Stacktrace :: [term()].
'erlang:raise'(_, _, _) -> error(eqwalizer_specs).

-spec 'erlang:send'(erlang:send_destination(), Msg) -> Msg.
'erlang:send'(_, _) -> error(eqwalizer_specs).

-spec 'erlang:tl'([A]) -> [A].
'erlang:tl'(_) -> error(eqwalizer_specs).

%% -------- ets --------

-spec 'ets:first'(ets:tab()) -> eqwalizer:dynamic().
'ets:first'(_) -> error(eqwalizer_specs).

-spec 'ets:next'(ets:tab(), term()) -> eqwalizer:dynamic().
'ets:next'(_, _) -> error(eqwalizer_specs).

-spec 'ets:last'(ets:tab()) -> eqwalizer:dynamic() | '$end_of_table'.
'ets:last'(_) -> error(eqwalizer_specs).

-spec 'ets:lookup'(ets:tab(), term()) -> [eqwalizer:dynamic()].
'ets:lookup'(_, _) -> error(eqwalizer_specs).

-spec 'ets:lookup_element'(ets:tab(), term(), pos_integer()) -> eqwalizer:dynamic().
'ets:lookup_element'(_, _, _) -> error(eqwalizer_specs).

-spec 'ets:match'(ets:tab(), ets:match_pattern()) -> [eqwalizer:dynamic()].
'ets:match'(_, _) -> error(eqwalizer_specs).

-spec 'ets:select'(ets:tab(), ets:match_spec()) -> [eqwalizer:dynamic()].
'ets:select'(_, _) -> error(eqwalizer_specs).

-spec 'ets:tab2list'(ets:tab()) -> [eqwalizer:dynamic()].
'ets:tab2list'(_) -> error(eqwalizer_specs).

-spec 'ets:take'(ets:tab(), term()) -> [eqwalizer:dynamic()].
'ets:take'(_, _) -> error(eqwalizer_specs).

%% -------- file --------

-spec 'file:consult'(Filename) -> {ok, Terms} | {error, Reason} when
    Filename :: file:name_all(),
    Terms :: [eqwalizer:dynamic()],
    Reason ::
        file:posix()
        | badarg
        | terminated
        | system_limit
        | {Line :: integer(), Mod :: module(), Term :: term()}.
'file:consult'(_) -> error(eqwalizer_specs).

%% -------- filename --------

-spec 'filename:join'([file:name_all()]) -> eqwalizer:dynamic().
'filename:join'(_) -> error(eqwalizer_specs).

-spec 'filename:join'(file:name_all(), file:name_all()) -> eqwalizer:dynamic().
'filename:join'(_, _) -> error(eqwalizer_specs).

%% -------- gen_server --------

-spec 'gen_server:call'(gen_server:server_ref(), term()) -> eqwalizer:dynamic().
'gen_server:call'(_, _) -> error(eqwalizer_specs).

-spec 'gen_server:call'(gen_server:server_ref(), term(), timeout()) -> eqwalizer:dynamic().
'gen_server:call'(_, _, _) -> error(eqwalizer_specs).

%% -------- gb_sets --------

-spec 'gb_sets:empty'() -> gb_sets:set(none()).
'gb_sets:empty'() -> error(eqwalizer_specs).

-spec 'gb_sets:new'() -> gb_sets:set(none()).
'gb_sets:new'() -> error(eqwalizer_specs).

%% -------- jsone --------

-spec 'jsone:decode'(binary()) -> eqwalizer:dynamic().
'jsone:decode'(_) -> error(eqwalizer_specs).

-spec 'jsone:decode'(binary(), [jsone:decode_option()]) -> eqwalizer:dynamic().
'jsone:decode'(_, _) -> error(eqwalizer_specs).

%% -------- lists --------

-spec 'lists:all'(fun((T) -> boolean()), [T]) -> boolean().
'lists:all'(_, _) -> error(eqwalizer_specs).

-spec 'lists:any'(fun((T) -> boolean()), [T]) -> boolean().
'lists:any'(_, _) -> error(eqwalizer_specs).

-spec 'lists:append'([[T]]) -> [T].
'lists:append'(_) -> error(eqwalizer_specs).

-spec 'lists:append'([T], [T]) -> [T].
'lists:append'(_, _) -> error(eqwalizer_specs).

-spec 'lists:delete'(T, [T]) -> [T].
'lists:delete'(_, _) -> error(eqwalizer_specs).

-spec 'lists:droplast'([T]) -> [T].
'lists:droplast'(_) -> error(eqwalizer_specs).

-spec 'lists:dropwhile'(fun((T) -> boolean()), [T]) -> [T].
'lists:dropwhile'(_, _) -> error(eqwalizer_specs).

-spec 'lists:duplicate'(non_neg_integer(), T) -> [T].
'lists:duplicate'(_, _) -> error(eqwalizer_specs).

-spec 'lists:enumerate'([A]) -> [{integer(), A}].
'lists:enumerate'(_) -> error(eqwalizer_specs).

-spec 'lists:filter'(fun((T) -> boolean()), [T]) -> [T].
'lists:filter'(_, _) -> error(eqwalizer_specs).

-spec 'lists:filtermap'(fun((T) -> boolean() | {'true', X}), [T]) -> [(T | X)].
'lists:filtermap'(_, _) -> error(eqwalizer_specs).

-spec 'lists:flatmap'(fun((A) -> [B]), [A]) -> [B].
'lists:flatmap'(_, _) -> error(eqwalizer_specs).

-spec 'lists:flatlength'([any()]) -> non_neg_integer().
'lists:flatlength'(_) -> error(eqwalizer_specs).

-spec 'lists:foldl'(fun((T, Acc) -> Acc), Acc, [T]) -> Acc.
'lists:foldl'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:foldr'(fun((T, Acc) -> Acc), Acc, [T]) -> Acc.
'lists:foldr'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:foreach'(fun((T) -> any()), [T]) -> ok.
'lists:foreach'(_, _) -> error(eqwalizer_specs).

-spec 'lists:join'(T, [T]) -> [T].
'lists:join'(_, _) -> error(eqwalizer_specs).

-spec 'lists:keydelete'(Key :: term(), N :: pos_integer(), [Tuple]) -> [Tuple].
'lists:keydelete'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:keyfind'(Key :: term(), N :: pos_integer(), [Tuple]) -> Tuple | false.
'lists:keyfind'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:keyreplace'(Key :: term(), N :: pos_integer(), [Tuple], Tuple) -> [Tuple].
'lists:keyreplace'(_, _, _, _) -> error(eqwalizer_specs).

-spec 'lists:keysearch'(Key :: term(), N :: pos_integer(), [Tuple]) -> {value, Tuple} | false.
'lists:keysearch'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:keytake'(Key :: term(), N :: pos_integer(), [Tuple]) -> {value, Tuple, [Tuple]} | false.
'lists:keytake'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:last'([T]) -> T.
'lists:last'(_) -> error(eqwalizer_specs).

-spec 'lists:map'(fun((A) -> B), [A]) -> [B].
'lists:map'(_, _) -> error(eqwalizer_specs).

-spec 'lists:mapfoldl'(fun((A, Acc) -> {B, Acc}), Acc, [A]) -> {[B], Acc}.
'lists:mapfoldl'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:mapfoldr'(fun((A, Acc) -> {B, Acc}), Acc, [A]) -> {[B], Acc}.
'lists:mapfoldr'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:max'([T]) -> T.
'lists:max'(_) -> error(eqwalizer_specs).

-spec 'lists:member'(T, [T]) -> boolean().
'lists:member'(_, _) -> error(eqwalizer_specs).

-spec 'lists:merge'([[T]]) -> [T].
'lists:merge'(_) -> error(eqwalizer_specs).

-spec 'lists:merge'([X], [Y]) -> [X | Y].
'lists:merge'(_, _) -> error(eqwalizer_specs).

-spec 'lists:merge'(fun((A, B) -> boolean()), [A], [B]) -> [A | B].
'lists:merge'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:merge3'([X], [Y], [Z]) -> [X | Y | Z].
'lists:merge3'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:min'([T]) -> T.
'lists:min'(_) -> error(eqwalizer_specs).

-spec 'lists:nth'(pos_integer(), [T]) -> T.
'lists:nth'(_, _) -> error(eqwalizer_specs).

-spec 'lists:nthtail'(pos_integer(), [T]) -> [T].
'lists:nthtail'(_, _) -> error(eqwalizer_specs).

-spec 'lists:partition'(fun((T) -> boolean()), [T]) -> {[T], [T]}.
'lists:partition'(_, _) -> error(eqwalizer_specs).

-spec 'lists:prefix'([T], [T]) -> boolean().
'lists:prefix'(_, _) -> error(eqwalizer_specs).

-spec 'lists:reverse'([T]) -> [T].
'lists:reverse'(_) -> error(eqwalizer_specs).

-spec 'lists:reverse'([T], [T]) -> [T].
'lists:reverse'(_, _) -> error(eqwalizer_specs).

-spec 'lists:rmerge'([X], [Y]) -> [X | Y].
'lists:rmerge'(_, _) -> error(eqwalizer_specs).

-spec 'lists:rmerge3'([X], [Y], [Z]) -> [X | Y | Z].
'lists:rmerge3'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:rumerge'([X], [Y]) -> [X | Y].
'lists:rumerge'(_, _) -> error(eqwalizer_specs).

-spec 'lists:rumerge'(fun((X, Y) -> boolean()), [X], [Y]) -> [(X | Y)].
'lists:rumerge'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:rumerge3'([X], [Y], [Z]) -> [X | Y | Z].
'lists:rumerge3'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:search'(fun((T) -> boolean()), [T]) -> {value, T} | false.
'lists:search'(_, _) -> error(eqwalizer_specs).

-spec 'lists:sort'([T]) -> [T].
'lists:sort'(_) -> error(eqwalizer_specs).

-spec 'lists:sort'(fun((T, T) -> boolean()), [T]) -> [T].
'lists:sort'(_, _) -> error(eqwalizer_specs).

-spec 'lists:split'(non_neg_integer(), [T]) -> {[T], [T]}.
'lists:split'(_, _) -> error(eqwalizer_specs).

-spec 'lists:splitwith'(fun((T) -> boolean()), [T]) -> {[T], [T]}.
'lists:splitwith'(_, _) -> error(eqwalizer_specs).

-spec 'lists:sublist'([T], Len :: non_neg_integer()) -> [T].
'lists:sublist'(_, _) -> error(eqwalizer_specs).

-spec 'lists:sublist'([T], Start :: pos_integer(), Len :: non_neg_integer()) -> [T].
'lists:sublist'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:subtract'([T], [T]) -> [T].
'lists:subtract'(_, _) -> error(eqwalizer_specs).

-spec 'lists:suffix'([T], [T]) -> boolean().
'lists:suffix'(_, _) -> error(eqwalizer_specs).

-spec 'lists:takewhile'(fun((T) -> boolean()), [T]) -> [T].
'lists:takewhile'(_, _) -> error(eqwalizer_specs).

-spec 'lists:umerge'([[T]]) -> [T].
'lists:umerge'(_) -> error(eqwalizer_specs).

-spec 'lists:umerge'([A], [B]) -> [A | B].
'lists:umerge'(_, _) -> error(eqwalizer_specs).

-spec 'lists:umerge'(fun((A, B) -> boolean()), [A], [B]) -> [A | B].
'lists:umerge'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:umerge3'([A], [B], [C]) -> [A | B | C].
'lists:umerge3'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:uniq'([A]) -> [A].
'lists:uniq'(_) -> error(eqwalizer_specs).

-spec 'lists:unzip'([{A, B}]) -> {[A], [B]}.
'lists:unzip'(_) -> error(eqwalizer_specs).

-spec 'lists:unzip3'([{A, B, C}]) -> {[A], [B], [C]}.
'lists:unzip3'(_) -> error(eqwalizer_specs).

-spec 'lists:usort'([T]) -> [T].
'lists:usort'(_) -> error(eqwalizer_specs).

-spec 'lists:usort'(fun((T, T) -> boolean()), [T]) -> [T].
'lists:usort'(_, _) -> error(eqwalizer_specs).

-spec 'lists:zf'(fun((T) -> boolean() | {'true', X}), [T]) -> [(T | X)].
'lists:zf'(_, _) -> error(eqwalizer_specs).

-spec 'lists:zip'([A], [B]) -> [{A, B}].
'lists:zip'(_, _) -> error(eqwalizer_specs).

-spec 'lists:zip3'([A], [B], [C]) -> [{A, B, C}].
'lists:zip3'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:zipwith'(fun((X, Y) -> T), [X], [Y]) -> [T].
'lists:zipwith'(_, _, _) -> error(eqwalizer_specs).

-spec 'lists:zipwith3'(fun((X, Y, Z) -> T), [X], [Y], [Z]) -> [T].
'lists:zipwith3'(_, _, _, _) -> error(eqwalizer_specs).

%% -------- logger_formatter --------

-spec 'logger_formatter:check_config'(Config) -> ok | {error, term()} when
    Config :: map().
'logger_formatter:check_config'(_) -> error(eqwalizer_specs).

%% -------- maps --------

-spec 'maps:find'(Key, #{Key => Value}) -> {ok, Value} | error.
'maps:find'(_, _) -> error(eqwalizer_specs).

-spec 'maps:from_list'([{Key, Value}]) -> #{Key => Value}.
'maps:from_list'(_) -> error(eqwalizer_specs).

-spec 'maps:groups_from_list'(fun((T) -> Key), [T]) -> #{Key => [T]}.
'maps:groups_from_list'(_, _) -> error(eqwalizer_specs).

-spec 'maps:groups_from_list'(fun((T) -> Key), fun((T) -> Value), [T]) -> #{Key => [Value]}.
'maps:groups_from_list'(_, _, _) -> error(eqwalizer_specs).

-spec 'maps:merge'(map(), map()) -> map().
'maps:merge'(_, _) -> error(eqwalizer_specs).

-spec 'maps:put'(Key, Value, #{Key => Value}) -> #{Key => Value}.
'maps:put'(_, _, _) -> error(eqwalizer_specs).

-spec 'maps:remove'(Key, #{Key => Value}) -> #{Key => Value}.
'maps:remove'(_, _) -> error(eqwalizer_specs).

-spec 'maps:take'(Key :: term(), map()) -> {Value :: eqwalizer:dynamic(), map()} | error.
'maps:take'(_, _) -> error(eqwalizer_specs).

-spec 'maps:update'(Key :: term(), Value :: term(), map()) -> map().
'maps:update'(_, _, _) -> error(eqwalizer_specs).

-spec 'maps:update_with'(Key :: term(), fun(), map()) -> map().
'maps:update_with'(_, _, _) -> error(eqwalizer_specs).

-spec 'maps:update_with'(Key :: term(), fun(), Init :: term(), map()) -> map().
'maps:update_with'(_, _, _, _) -> error(eqwalizer_specs).

%% -------- orddict --------

-spec 'orddict:new'() -> orddict:orddict(none(), none()).
'orddict:new'() -> error(eqwalizer_specs).

%% -------- persistent_term --------

-spec 'persistent_term:get'(term()) -> eqwalizer:dynamic().
'persistent_term:get'(_) -> error(eqwalizer_specs).

-spec 'persistent_term:get'(term(), term()) -> eqwalizer:dynamic().
'persistent_term:get'(_, _) -> error(eqwalizer_specs).

%% -------- proc_lib --------

-spec 'proc_lib:start_link'(Module, Function, Args) -> Ret when
    Module :: module(),
    Function :: atom(),
    Args :: [term()],
    Ret :: eqwalizer:dynamic().
'proc_lib:start_link'(_, _, _) -> error(eqwalizer_specs).

%% -------- proplists --------

-spec 'proplists:delete'(any(), [A]) -> [A].
'proplists:delete'(_, _) -> error(eqwalizer_specs).

-spec 'proplists:get_all_values'(term(), [term()]) -> [eqwalizer:dynamic()].
'proplists:get_all_values'(_, _) -> error(eqwalizer_specs).

-spec 'proplists:get_keys'([term()]) -> [eqwalizer:dynamic()].
'proplists:get_keys'(_) -> error(eqwalizer_specs).

-spec 'proplists:get_value'(term(), [term()]) -> eqwalizer:dynamic().
'proplists:get_value'(_, _) -> error(eqwalizer_specs).

-spec 'proplists:get_value'(term(), [term()], term()) -> eqwalizer:dynamic().
'proplists:get_value'(_, _, _) -> error(eqwalizer_specs).

-spec 'proplists:from_map'(#{K => V}) -> [{K, V}].
'proplists:from_map'(_) -> error(eqwalizer_specs).

%% -------- public_key --------

-spec 'public_key:der_decode'(public_key:asn1_type(), public_key:der_encoded()) -> eqwalizer:dynamic().
'public_key:der_decode'(_, _) -> error(eqwalizer_specs).

%% -------- queue --------
-spec 'queue:new'() -> queue:queue(none()).
'queue:new'() -> error(eqwalizer_specs).

%% -------- re --------

-spec 're:run'(Subject, RE, Options) ->
    {match, eqwalizer:dynamic()} | match | nomatch | {error, eqwalizer:dynamic()}
when
    Subject :: iodata() | unicode:charlist(),
    RE :: {re_pattern, _, _, _, _} | iodata() | unicode:charlist(),
    Options :: [Option],
    Option ::
        anchored
        | global
        | notbol
        | noteol
        | notempty
        | notempty_atstart
        | report_errors
        | {offset, non_neg_integer()}
        | {match_limit, non_neg_integer()}
        | {match_limit_recursion, non_neg_integer()}
        | {newline, NLSpec}
        | bsr_anycrlf
        | bsr_unicode
        | {capture, ValueSpec}
        | {capture, ValueSpec, Type}
        | CompileOpt,
    Type :: index | list | binary,
    ValueSpec :: all | all_but_first | all_names | first | none | ValueList,
    ValueList :: [ValueID],
    ValueID :: integer() | string() | atom(),
    CompileOpt ::
        unicode
        | anchored
        | caseless
        | dollar_endonly
        | dotall
        | extended
        | firstline
        | multiline
        | no_auto_capture
        | dupnames
        | ungreedy
        | {newline, NLSpec}
        | bsr_anycrlf
        | bsr_unicode
        | no_start_optimize
        | ucp
        | never_utf,
    NLSpec :: cr | crlf | lf | anycrlf | any.

're:run'(_, _, _) ->
    error(eqwalizer_specs).

%% -------- sets --------

-spec 'sets:new'() -> sets:set(none()).
'sets:new'() -> error(eqwalizer_specs).

-spec 'sets:new'(Opts :: [{version, 1..2}]) -> sets:set(none()).
'sets:new'(_) -> error(eqwalizer_specs).

%% -------- string --------

-spec 'string:lexemes'
    (string(), [string:grapheme_cluster()]) -> [string()];
    (unicode:unicode_binary(), [string:grapheme_cluster()]) -> [unicode:unicode_binary()].
'string:lexemes'(_, _) -> error(eqwalizer_specs).

-spec 'string:slice'
    (string(), non_neg_integer()) -> string();
    (unicode:unicode_binary(), non_neg_integer()) -> unicode:unicode_binary().
'string:slice'(_, _) -> error(eqwalizer_specs).

-spec 'string:slice'
    (string(), non_neg_integer(), 'infinity' | non_neg_integer()) -> string();
    (unicode:unicode_binary(), non_neg_integer(), 'infinity' | non_neg_integer()) -> unicode:unicode_binary().
'string:slice'(_, _, _) -> error(eqwalizer_specs).

-spec 'string:trim'
    (string()) -> string();
    (unicode:unicode_binary()) -> unicode:unicode_binary().
'string:trim'(_) -> error(eqwalizer_specs).

%% -------- timer --------

-spec 'timer:tc'(fun(() -> T)) -> {integer(), T}.
'timer:tc'(_) -> error(eqwalizer_specs).

-spec 'timer:tc'(Fun, Arguments) -> {Time, Value} when
    Fun :: function(),
    Arguments :: [term()],
    Time :: integer(),
    Value :: eqwalizer:dynamic().
'timer:tc'(_, _) -> error(eqwalizer_specs).

-spec 'timer:tc'(module(), atom(), [term()]) -> {integer(), eqwalizer:dynamic()}.
'timer:tc'(_, _, _) -> error(eqwalizer_specs).
