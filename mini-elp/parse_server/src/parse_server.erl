%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(parse_server).

-export([main/1]).

-record(state, {io = erlang:group_leader()}).

main(_Args) ->
    erlang:system_flag(backtrace_depth, 20),
    State = #state{},
    io:setopts(State#state.io, [binary]),
    loop(State).

loop(State0) ->
    case io:get_line(State0#state.io, "") of
        eof ->
            erlang:halt(1);
        Line ->
            State = process(binary_part(Line, 0, byte_size(Line) - 1), State0),
            loop(State)
    end.

process(<<"ADD_PATHS ", BinLen/binary>>, State) ->
    add_paths(BinLen, State);
process(<<"COMPILE ", BinLen/binary>>, State) ->
    PostProcess = fun(Forms, _FileName) -> term_to_binary({ok, Forms, []}) end,
    process1(BinLen, State, PostProcess);
process(<<"TEXT ", BinLen/binary>>, State) ->
    PostProcess = fun(Forms, _) ->
        unicode:characters_to_binary(lists:join($\n, [io_lib:format("~p.~n", [Form]) || Form <- Forms]))
    end,
    process1(BinLen, State, PostProcess);
process(<<"EXIT">>, State) ->
    init:stop(),
    State.

add_paths(BinLen, State) ->
    Len = binary_to_integer(BinLen),
    Paths = collect_paths(Len, State),
    code:add_pathsa(Paths),
    State.

collect_paths(0, _State) ->
    [];
collect_paths(Len, State) ->
    case io:get_line(State#state.io, "") of
        eof ->
            [];
        Line ->
            Path = unicode:characters_to_list(string:trim(Line, trailing)),
            [Path | collect_paths(Len - 1, State)]
    end.

process1(BinLen, State, PostProcess) ->
    Len = binary_to_integer(BinLen),
    %% Use file:read/2 since it reads bytes
    {ok, Data} = file:read(State#state.io, Len),
    spawn_link(fun() ->
        {Id, FileName, Options} = binary_to_term(Data),
        try run_process(Id, FileName, Options, State, PostProcess)
        catch
            Class:Reason:StackTrace ->
                Formatted = erl_error:format_exception(Class, Reason, StackTrace),
                ExceptionData = unicode:characters_to_binary(Formatted),
                reply("EXCEPTION", Id, ExceptionData, State)
        end
    end),
    State.

run_process(Id, FileName, Options, State, PostProcess) ->
    Module = epp_module(Options),
    case Module:parse_file(FileName, Options) of
        {ok, Forms0} ->
            Forms1 = case proplists:get_value(elp_metadata, Options) of
                undefined ->
                    Forms0;
                ElpMetadata ->
                    elp_metadata:insert_metadata(ElpMetadata, Forms0)
            end,
            case elp_lint:module(Forms1, FileName, extra_options() ++ Options) of
                {ok, Warnings0} ->
                    case filter_warnings(Warnings0) of
                        [] ->
                            Result = PostProcess(Forms1, FileName),
                            reply(<<"OK">>, Id, Result, State);
                        Warnings ->
                            Result = format_errors(Warnings),
                            reply(<<"ERROR">>, Id, Result, State)
                    end;
                {error, Errors, Warnings} ->
                    Result = format_errors(Errors ++ filter_warnings(Warnings)),
                    reply(<<"ERROR">>, Id, Result, State)
            end;
        {error, Reason} ->
            Msg = unicode:characters_to_binary(file:format_error(Reason)),
            reply(<<"ERROR">>, Id, Msg, State)
    end.

epp_module(Options) ->
    case proplists:get_value(location, Options) of
        offset -> elp_epp;
        _ -> epp
    end.

filter_warnings([{Path, Warnings} | Rest]) ->
    case do_filter_warnings(Warnings) of
        [] -> filter_warnings(Rest);
        Warnings1 -> [{Path, Warnings1} | filter_warnings(Rest)]
    end;
filter_warnings([]) ->
    [].

do_filter_warnings(Warnings) ->
    [Warning || Warning <- Warnings, keep_warning(Warning)].

%% We're only interested in some warnings for EqWAlizer, others we ignore
%% See https://fburl.com/code/vdlcwjyh for documentation of what checks are important
keep_warning({_, elp_lint, {exported_var, _, _}}) -> true;
keep_warning({_, elp_lint, {shadowed_var, _, _}}) -> true;
keep_warning({_, elp_lint, {call_to_redefined_bif, _}}) -> true;
keep_warning({_, elp_lint, {redefine_bif_import, _}}) -> true;
keep_warning(_) -> false.

extra_options() ->
    [
        nowarn_unused_vars,
        nowarn_underscore_match,
        nowarn_export_all,
        nowarn_unused_import,
        nowarn_unused_function,
        nowarn_unused_type,
        nowarn_unused_record,
        nowarn_deprecated_function,
        nowarn_deprecated_type,
        nowarn_obsolete_guard,
        nowarn_untyped_record,
        nowarn_missing_spec,
        nowarn_missing_spec_all,
        nowarn_removed,
        nowarn_nif_inline,
        nowarn_format
    ].

format_errors(Warnings) ->
    Formatted = [
        format_error(Path, Warning)
     || {Path, FileWarnings} <- Warnings,
        Warning <- FileWarnings
    ],
    term_to_binary(Formatted).

-spec format_error(
    unicode:latin1_chardata() | unicode:chardata() | unicode:external_chardata(),
    elp_lint:error_info()
) -> string().
format_error(Path, {Line, Mod, Reason}) when is_integer(Line) ->
    {
        unicode:characters_to_list(Path),
        none,
        unicode:characters_to_list(io_lib:format("~p: ~ts", [Line, Mod:format_error(Reason)]))
    };
format_error(Path, {Location, Mod, Reason}) ->
    {
        unicode:characters_to_list(Path),
        % Location would be {Line, Col} for a erlc compiler error/warning,
        % but {ByteStart, ByteEnd} for an eqwalizer diagnostic.
        % This is deciphered on elp side.
        Location,
        unicode:characters_to_list(Mod:format_error(Reason))
    }.

reply(Msg, Id, Data, State) ->
    %% Use file:write/2 since it writes bytes
    Size = integer_to_binary(byte_size(Data)),
    BinId = integer_to_binary(Id),
    file:write(State#state.io, [Msg, $\s, BinId, $\s, Size, $\n, Data]),
    ok.
