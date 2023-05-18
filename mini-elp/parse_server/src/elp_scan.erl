%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2019. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%% Erlang token scanning functions of io library.

%% For handling ISO 8859-1 (Latin-1) we use the following type
%% information:
%%
%% 000 - 037    NUL - US        control
%% 040 - 057    SPC - /         punctuation
%% 060 - 071    0 - 9           digit
%% 072 - 100    : - @           punctuation
%% 101 - 132    A - Z           uppercase
%% 133 - 140    [ - `           punctuation
%% 141 - 172    a - z           lowercase
%% 173 - 176    { - ~           punctuation
%% 177          DEL             control
%% 200 - 237                    control
%% 240 - 277    NBSP - ¿        punctuation
%% 300 - 326    À - Ö           uppercase
%% 327          ×               punctuation
%% 330 - 336    Ø - Þ           uppercase
%% 337 - 366    ß - ö           lowercase
%% 367          ÷               punctuation
%% 370 - 377    ø - ÿ           lowercase
%%
%% Many punctuation characters have special meaning:
%%  $\s, $_, $", $$, $%, $', $.
%% DEL is a punctuation.
%%
%% Must watch using × \327, very close to x \170.

-module(elp_scan).

%%% External exports

-export([
    tokens/3, tokens/4,
    line/1,
    location/1,
    symbol/1,
    format_error/1,
    reserved_word/1
]).

-export_type([
    error_info/0,
    options/0,
    return_cont/0,
    token/0,
    tokens/0,
    tokens_result/0
]).

%%%
%%% Defines and type definitions
%%%

-define(OFFSET(O), (is_integer(O) andalso O >= 0)).
-define(STRING(S), is_list(S)).
-define(RESWORDFUN(F), is_function(F, 1)).

-type offset() :: non_neg_integer().
-type location() :: {offset(), offset()}.
-type category() :: atom().
-type resword_fun() :: fun((atom()) -> boolean()).
-type option() :: {'reserved_word_fun', resword_fun()}.
-type options() :: option() | [option()].
-type symbol() :: atom() | float() | integer() | string().
-type token() ::
    {category(), location(), symbol()}
    | {category(), location()}.
-type tokens() :: [token()].
-type error_description() :: term().
-type error_info() :: {offset(), module(), error_description()}.

%%% Local record.
-record(erl_scan, {
    resword_fun = fun reserved_word/1 :: resword_fun()
}).

%%----------------------------------------------------------------------------

-spec format_error(ErrorDescriptor) -> string() when
    ErrorDescriptor :: error_description().
format_error({string, Quote, Head}) ->
    lists:flatten([
        "unterminated " ++ string_thing(Quote) ++
            " starting with " ++
            io_lib:write_string(Head, Quote)
    ]);
format_error({illegal, Type}) ->
    lists:flatten(io_lib:fwrite("illegal ~w", [Type]));
format_error(char) ->
    "unterminated character";
format_error({base, Base}) ->
    lists:flatten(io_lib:fwrite("illegal base '~w'", [Base]));
format_error(Other) ->
    lists:flatten(io_lib:write(Other)).

-type char_spec() :: string() | 'eof'.
-type cont_fun() :: fun(
    (
        char_spec(),
        #erl_scan{},
        offset(),
        tokens(),
        any()
    ) -> any()
).
-opaque return_cont() ::
    {erl_scan_continuation, string(), offset(), tokens(), #erl_scan{}, any(), cont_fun()}.
-type tokens_result() ::
    {'ok', Tokens :: tokens(), EndOffset :: offset()}
    | {'eof', EndOffset :: offset()}
    | {'error', ErrorInfo :: error_info(), EndOffset :: offset()}.

-spec tokens(Continuation, CharSpec, StartOffset) -> Return when
    Continuation :: return_cont() | [],
    CharSpec :: char_spec(),
    StartOffset :: offset(),
    Return ::
        {'done', Result :: tokens_result(), LeftOverChars :: char_spec()}
        | {'more', Continuation1 :: return_cont()}.
tokens(Cont, CharSpec, StartOffset) ->
    tokens(Cont, CharSpec, StartOffset, []).

-spec tokens(Continuation, CharSpec, StartOffset, Options) -> Return when
    Continuation :: return_cont() | [],
    CharSpec :: char_spec(),
    StartOffset :: erl_anno:location(),
    Options :: options(),
    Return ::
        {'done', Result :: tokens_result(), LeftOverChars :: char_spec()}
        | {'more', Continuation1 :: return_cont()}.
tokens([], CharSpec, Off, Options) when ?OFFSET(Off) ->
    tokens1(CharSpec, options(Options), Off, [], fun scan/5, []);
tokens({erl_scan_continuation, Cs, Off, Toks, St, Any, Fun}, CharSpec, _Off, _Opts) ->
    tokens1(Cs ++ CharSpec, St, Off, Toks, Fun, Any).

-spec line(Token) -> erl_anno:line() when
    Token :: token().
line(_Token) ->
    0.

-spec location(Token) -> location() when
    Token :: token().
location(Token) ->
    element(2, Token).

-spec symbol(Token) -> symbol() when
    Token :: token().
symbol({Category, _Anno}) ->
    Category;
symbol({_Category, _Anno, Symbol}) ->
    Symbol;
symbol(T) ->
    erlang:error(badarg, [T]).

%%%
%%% Local functions
%%%

%' Stupid Emacs
string_thing($') -> "atom";
string_thing(_) -> "string".

-define(WHITE_SPACE(C),
    is_integer(C) andalso
        (C >= $\000 andalso C =< $\s orelse C >= $\200 andalso C =< $\240)
).
-define(DIGIT(C), C >= $0 andalso C =< $9).
-define(CHAR(C), is_integer(C), C >= 0).
-define(UNICODE(C),
    is_integer(C) andalso
        (C >= 0 andalso C < 16#D800 orelse
            C > 16#DFFF andalso C < 16#FFFE orelse
            C > 16#FFFF andalso C =< 16#10FFFF)
).

-define(UNI255(C), C >= 0, C =< 16#ff).

options(Opts0) when is_list(Opts0) ->
    Opts = lists:foldr(fun expand_opt/2, [], Opts0),
    [RW_fun] =
        case opts(Opts, [reserved_word_fun], []) of
            badarg ->
                erlang:error(badarg, [Opts0]);
            R ->
                R
        end,
    #erl_scan{
        resword_fun = RW_fun
    };
options(Opt) ->
    options([Opt]).

opts(Options, [Key | Keys], L) ->
    V =
        case lists:keyfind(Key, 1, Options) of
            {reserved_word_fun, F} when ?RESWORDFUN(F) ->
                {ok, F};
            {Key, _} ->
                badarg;
            false ->
                {ok, default_option(Key)}
        end,
    case V of
        badarg ->
            badarg;
        {ok, Value} ->
            opts(Options, Keys, [Value | L])
    end;
opts(_Options, [], L) ->
    lists:reverse(L).

default_option(reserved_word_fun) ->
    fun reserved_word/1.

expand_opt(return, Os) ->
    [return_comments, return_white_spaces | Os];
expand_opt(O, Os) ->
    [O | Os].

tokens1(Cs, St, Off, Toks, Fun, Any) when ?STRING(Cs); Cs =:= eof ->
    case Fun(Cs, St, Off, Toks, Any) of
        {more, {Cs0, Noff, Ntoks, Nany, Nfun}} ->
            {more, {erl_scan_continuation, Cs0, Noff, Ntoks, St, Nany, Nfun}};
        {ok, Toks0, eof, Noff} ->
            Res =
                case Toks0 of
                    [] ->
                        {eof, Noff};
                    _ ->
                        {ok, lists:reverse(Toks0), Noff}
                end,
            {done, Res, eof};
        {ok, Toks0, Rest, Noff} ->
            {done, {ok, lists:reverse(Toks0), Noff}, Rest};
        {{error, _, _} = Error, Rest} ->
            {done, Error, Rest}
    end.

scan(Cs, St, Off, Toks, _) ->
    scan1(Cs, St, Off, Toks).

scan1([$\s | Cs], St, Off, Toks) ->
    skip_white_space(Cs, St, Off, Toks, 1);
scan1([$\n | Cs], St, Off, Toks) ->
    skip_white_space(Cs, St, Off, Toks, 1);
scan1([C | Cs], St, Off, Toks) when C >= $A, C =< $Z ->
    scan_variable(Cs, St, Off, Toks, {[C], 1});
scan1([C | Cs], St, Off, Toks) when C >= $a, C =< $z ->
    scan_atom(Cs, St, Off, Toks, {[C], 1});
%% Optimization: some very common punctuation characters:
scan1([$, | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, ',', 1);
scan1([$( | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '(', 1);
scan1([$) | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, ')', 1);
scan1([${ | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '{', 1);
scan1([$} | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '}', 1);
scan1([$[ | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '[', 1);
scan1([$] | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, ']', 1);
scan1([$; | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, ';', 1);
scan1([$_ = C | Cs], St, Off, Toks) ->
    scan_variable(Cs, St, Off, Toks, {[C], 1});
%% More punctuation characters below.
scan1("%" = Cs, _St, Off, Toks) ->
    {more, {Cs, Off, Toks, [], fun scan/5}};
scan1([$% | Cs], St, Off, Toks) ->
    skip_comment(Cs, St, Off, Toks, 1);
scan1([C | Cs], St, Off, Toks) when ?DIGIT(C) ->
    scan_number(Cs, St, Off, Toks, [C], no_underscore);
scan1("..." ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '...', 3);
scan1(".." = Cs, _St, Off, Toks) ->
    {more, {Cs, Off, Toks, [], fun scan/5}};
scan1(".." ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '..', 2);
scan1("." = Cs, _St, Off, Toks) ->
    {more, {Cs, Off, Toks, [], fun scan/5}};
scan1([$. | Cs], St, Off, Toks) ->
    scan_dot(Cs, St, Off, Toks, 1);
%" Emacs
scan1([$" | Cs], St, Off, Toks) ->
    State0 = {[], [], 1},
    scan_string(Cs, St, Off, Toks, State0);
%' Emacs
scan1([$' | Cs], St, Off, Toks) ->
    State0 = {[], [], 1},
    scan_qatom(Cs, St, Off, Toks, State0);
scan1([$$ | Cs], St, Off, Toks) ->
    scan_char(Cs, St, Off, Toks, 1);
scan1([C | Cs], St, Off, Toks) when C >= $ß, C =< $ÿ, C =/= $÷ ->
    scan_atom(Cs, St, Off, Toks, {[C], 2});
scan1([C | Cs], St, Off, Toks) when C >= $À, C =< $Þ, C /= $× ->
    scan_variable(Cs, St, Off, Toks, {[C], 2});
scan1([$\t | Cs], St, Off, Toks) ->
    skip_white_space(Cs, St, Off, Toks, 1);
scan1([C | Cs], St, Off, Toks) when ?WHITE_SPACE(C) ->
    skip_white_space(Cs, St, Off, Toks, char_byte_size(C));
%% Punctuation characters and operators, first recognise multiples.
%% ?= for the maybe ... else ... end construct
scan1("?=" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '?=', 2);
scan1("?" = Cs, _St, Off, Toks) ->
    {more, {Cs, Off, Toks, [], fun scan/5}};
%% << <- <=
scan1("<<" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '<<', 2);
scan1("<-" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '<-', 2);
scan1("<=" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '<=', 2);
scan1("<" = Cs, _St, Off, Toks) ->
    {more, {Cs, Off, Toks, [], fun scan/5}};
%% >> >=
scan1(">>" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '>>', 2);
scan1(">=" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '>=', 2);
scan1(">" = Cs, _St, Off, Toks) ->
    {more, {Cs, Off, Toks, [], fun scan/5}};
%% -> --
scan1("->" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '->', 2);
scan1("--" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '--', 2);
scan1("-" = Cs, _St, Off, Toks) ->
    {more, {Cs, Off, Toks, [], fun scan/5}};
%% ++
scan1("++" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '++', 2);
scan1("+" = Cs, _St, Off, Toks) ->
    {more, {Cs, Off, Toks, [], fun scan/5}};
%% =:= =/= =< == =>
scan1("=:=" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '=:=', 3);
scan1("=:" = Cs, _St, Off, Toks) ->
    {more, {Cs, Off, Toks, [], fun scan/5}};
scan1("=/=" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '=/=', 3);
scan1("=/" = Cs, _St, Off, Toks) ->
    {more, {Cs, Off, Toks, [], fun scan/5}};
scan1("=<" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '=<', 2);
scan1("=>" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '=>', 2);
scan1("==" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '==', 2);
scan1("=" = Cs, _St, Off, Toks) ->
    {more, {Cs, Off, Toks, [], fun scan/5}};
%% /=
scan1("/=" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '/=', 2);
scan1("/" = Cs, _St, Off, Toks) ->
    {more, {Cs, Off, Toks, [], fun scan/5}};
%% ||
scan1("||" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '||', 2);
scan1("|" = Cs, _St, Off, Toks) ->
    {more, {Cs, Off, Toks, [], fun scan/5}};
%% :=
scan1(":=" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, ':=', 2);
%% :: for typed records
scan1("::" ++ Cs, St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '::', 2);
scan1(":" = Cs, _St, Off, Toks) ->
    {more, {Cs, Off, Toks, [], fun scan/5}};
%% Optimization: punctuation characters less than 127:
scan1([$= | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '=', 1);
scan1([$: | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, ':', 1);
scan1([$| | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '|', 1);
scan1([$# | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '#', 1);
scan1([$/ | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '/', 1);
scan1([$? | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '?', 1);
scan1([$- | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '-', 1);
scan1([$+ | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '+', 1);
scan1([$* | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '*', 1);
scan1([$< | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '<', 1);
scan1([$> | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '>', 1);
scan1([$! | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '!', 1);
scan1([$@ | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '@', 1);
scan1([$\\ | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '\\', 1);
scan1([$^ | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '^', 1);
scan1([$` | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '`', 1);
scan1([$~ | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '~', 1);
scan1([$& | Cs], St, Off, Toks) ->
    tok2(Cs, St, Off, Toks, '&', 1);
%% End of optimization.
scan1([C | Cs], St, Off, Toks) when ?UNI255(C) ->
    tok2(Cs, St, Off, Toks, list_to_atom([C]), char_byte_size(C));
scan1([C | Cs], _St, Off, _Toks) when ?CHAR(C) ->
    scan_error({illegal, character}, Off, Off + 1, Cs);
scan1([] = Cs, _St, Off, Toks) ->
    {more, {Cs, Off, Toks, [], fun scan/5}};
scan1(eof = Cs, _St, Off, Toks) ->
    {ok, Toks, Cs, Off}.

scan_atom(Cs0, St, Off, Toks, {Ncs0, N0}) ->
    case scan_name(Cs0, Ncs0, N0) of
        {more, Ncs, N} ->
            {more, {[], Off, Toks, {Ncs, N}, fun scan_atom/5}};
        {done, Wcs, Cs, N} ->
            case catch list_to_atom(Wcs) of
                Name when is_atom(Name) ->
                    case (St#erl_scan.resword_fun)(Name) of
                        true ->
                            tok2(Cs, St, Off, Toks, Name, N);
                        false ->
                            tok3(Cs, St, Off, Toks, atom, Name, N)
                    end;
                _Error ->
                    scan_error({illegal, atom}, Off, Off + N, Cs)
            end
    end.

scan_variable(Cs0, St, Off, Toks, {Ncs0, N0}) ->
    case scan_name(Cs0, Ncs0, N0) of
        {more, Ncs, N} ->
            {more, {[], Off, Toks, {Ncs, N}, fun scan_variable/5}};
        {done, Wcs, Cs, N} ->
            case catch list_to_atom(Wcs) of
                Name when is_atom(Name) ->
                    tok3(Cs, St, Off, Toks, var, Name, N);
                _Error ->
                    scan_error({illegal, var}, Off, Off + N, Cs)
            end
    end.

scan_name([C | Cs], Ncs, N) when C >= $a, C =< $z ->
    scan_name(Cs, [C | Ncs], N + 1);
scan_name([C | Cs], Ncs, N) when C >= $A, C =< $Z ->
    scan_name(Cs, [C | Ncs], N + 1);
scan_name([$_ = C | Cs], Ncs, N) ->
    scan_name(Cs, [C | Ncs], N + 1);
scan_name([C | Cs], Ncs, N) when ?DIGIT(C) ->
    scan_name(Cs, [C | Ncs], N + 1);
scan_name([$@ = C | Cs], Ncs, N) ->
    scan_name(Cs, [C | Ncs], N + 1);
scan_name([C | Cs], Ncs, N) when C >= $ß, C =< $ÿ, C =/= $÷ ->
    scan_name(Cs, [C | Ncs], N + 2);
scan_name([C | Cs], Ncs, N) when C >= $À, C =< $Þ, C =/= $× ->
    scan_name(Cs, [C | Ncs], N + 2);
scan_name([], Ncs, N) ->
    {more, Ncs, N};
scan_name(Cs, Ncs, N) ->
    {done, lists:reverse(Ncs), Cs, N}.

scan_dot([$% | _] = Cs, _St, Off, Toks, N) ->
    {ok, [{dot, {Off, Off + N}} | Toks], Cs, Off + N};
scan_dot([$\n | Cs], _St, Off, Toks, N) ->
    {ok, [{dot, {Off, Off + N}} | Toks], Cs, Off + N + 1};
scan_dot([C | Cs], _St, Off, Toks, N) when ?WHITE_SPACE(C) ->
    {ok, [{dot, {Off, Off + N}} | Toks], Cs, Off + N + char_byte_size(C)};
scan_dot(eof = Cs, _St, Off, Toks, N) ->
    {ok, [{dot, {Off, Off + N}} | Toks], Cs, Off + N};
scan_dot(Cs, St, Off, Toks, N) ->
    tok2(Cs, St, Off, Toks, '.', N).

%% TODO: optimise for common chars
skip_white_space([C | Cs], St, Off, Toks, N) when ?WHITE_SPACE(C) ->
    skip_white_space(Cs, St, Off, Toks, N + char_byte_size(C));
skip_white_space([] = Cs, _St, Off, Toks, N) ->
    {more, {Cs, Off, Toks, N, fun skip_white_space/5}};
skip_white_space(Cs, St, Off, Toks, N) ->
    scan1(Cs, St, Off + N, Toks).

scan_char([$\\ | Cs], St, Off, Toks, N0) ->
    case scan_escape(Cs, N0 + 1) of
        more ->
            {more, {[$\\ | Cs], Off, Toks, N0, fun scan_char/5}};
        {error, Ncs, Error, N} ->
            scan_error(Error, Off, Off + N, Ncs);
        {eof, N} ->
            scan_error(char, Off, Off + N, eof);
        {Val, _Str, Ncs, N} ->
            Ntoks = [{char, {Off, Off + N}, Val} | Toks],
            scan1(Ncs, St, Off + N, Ntoks)
    end;
scan_char([$\n = C | Cs], St, Off, Toks, N) ->
    scan1(Cs, St, Off + N + 1, [{char, {Off, Off + N + 1}, C} | Toks]);
scan_char([C | Cs], St, Off, Toks, N) when ?UNICODE(C) ->
    Noff = Off + N + char_byte_size(C),
    scan1(Cs, St, Noff, [{char, {Off, Noff}, C} | Toks]);
scan_char([C | _Cs], _St, Off, _Toks, N) when ?CHAR(C) ->
    scan_error({illegal, character}, Off, Off + N + char_byte_size(C), eof);
scan_char([], _St, Off, Toks, N) ->
    {more, {[], Off, Toks, N, fun scan_char/5}};
scan_char(eof, _St, Off, _Toks, N) ->
    scan_error(char, Off, Off + N, eof).

scan_string(Cs, St, Off, Toks, {Wcs, Str, N0}) ->
    case scan_string0(Cs, St, N0, $\", Str, Wcs) of
        {more, Ncs, N, Nstr, Nwcs} ->
            State = {Nwcs, Nstr, N},
            {more, {Ncs, Off, Toks, State, fun scan_string/5}};
        {char_error, Ncs, Error, NStart, NEnd} ->
            scan_error(Error, Off + NStart, Off + NEnd, Ncs);
        {error, N, Nwcs, Ncs} ->
            % Expanded escape chars.
            Estr = string:slice(Nwcs, 0, 16),
            scan_error({string, $\", Estr}, Off, Off + N, Ncs);
        {done, Ncs, Nwcs, N} ->
            tok3(Ncs, St, Off, Toks, string, Nwcs, N)
    end.

scan_qatom(Cs, St, Off, Toks, {Wcs, Str, N0}) ->
    case scan_string0(Cs, St, N0, $\', Str, Wcs) of
        {more, Ncs, N, Nstr, Nwcs} ->
            State = {Nwcs, Nstr, N},
            {more, {Ncs, Off, Toks, State, fun scan_qatom/5}};
        {char_error, Ncs, Error, NStart, NEnd} ->
            scan_error(Error, Off + NStart, Off + NEnd, Ncs);
        {error, N, Nwcs, Ncs} ->
            Estr = string:slice(Nwcs, 0, 16),
            scan_error({string, $\', Estr}, Off, Off + N, Ncs);
        {done, Ncs, Nwcs, N} ->
            case catch list_to_atom(Nwcs) of
                A when is_atom(A) ->
                    tok3(Ncs, St, Off, Toks, atom, A, N);
                _ ->
                    scan_error({illegal, atom}, Off, Off + N, Ncs)
            end
    end.

scan_string0(Cs, St, N, Q, [], Wcs) ->
    scan_string_col(Cs, St, N, Q, Wcs);
scan_string0(Cs, _St, N, Q, Str, Wcs) ->
    scan_string1(Cs, N, Q, Str, Wcs).

scan_string_col([Q | Cs], _St, N, Q, Wcs0) ->
    Wcs = lists:reverse(Wcs0),
    {done, Cs, Wcs, N + 1};
scan_string_col([$\n = C | Cs], St, N, Q, Wcs) ->
    scan_string_col(Cs, St, N + 1, Q, [C | Wcs]);
scan_string_col([C | Cs], St, N, Q, Wcs) when C =/= $\\, ?UNICODE(C) ->
    scan_string_col(Cs, St, N + char_byte_size(C), Q, [C | Wcs]);
scan_string_col(Cs, _St, N, Q, Wcs) ->
    scan_string1(Cs, N, Q, Wcs, Wcs).

%% Note: in those cases when a 'char_error' tuple is returned below it
%% is tempting to skip over characters up to the first Q character,
%% but then the end location of the error tuple would not correspond
%% to the start location of the returned Rest string. (Maybe the end
%% location could be modified, but that too is ugly.)
scan_string1([Q | Cs], N, Q, _Str0, Wcs0) ->
    Wcs = lists:reverse(Wcs0),
    {done, Cs, Wcs, N + 1};
scan_string1([$\n = C | Cs], N, Q, Str, Wcs) ->
    scan_string1(Cs, N + 1, Q, [C | Str], [C | Wcs]);
scan_string1([$\\ | Cs] = Cs0, N0, Q, Str, Wcs) ->
    case scan_escape(Cs, N0 + 1) of
        more ->
            {more, Cs0, N0, Str, Wcs};
        {error, Ncs, Error, N} ->
            {char_error, Ncs, Error, N0, N};
        {eof, N} ->
            {error, N, lists:reverse(Wcs), eof};
        {Val, ValStr, Ncs, N} ->
            Nstr = lists:reverse(ValStr, [$\\ | Str]),
            Nwcs = [Val | Wcs],
            scan_string1(Ncs, N, Q, Nstr, Nwcs)
    end;
scan_string1([C | Cs], N, Q, Str, Wcs) when ?UNICODE(C) ->
    scan_string1(Cs, N + char_byte_size(C), Q, [C | Str], [C | Wcs]);
scan_string1([C | Cs], N, _Q, _Str, _Wcs) when ?CHAR(C) ->
    {char_error, Cs, {illegal, character}, N, N + char_byte_size(C)};
scan_string1([] = Cs, N, _Q, Str, Wcs) ->
    {more, Cs, N, Str, Wcs};
scan_string1(eof, N, _Q, _Str, Wcs) ->
    {error, N, lists:reverse(Wcs), eof}.

-define(OCT(C), C >= $0, C =< $7).
-define(HEX(C),
    C >= $0 andalso C =< $9 orelse
        C >= $A andalso C =< $F orelse
        C >= $a andalso C =< $f
).

%% \<1-3> octal digits
scan_escape([O1, O2, O3 | Cs], N) when ?OCT(O1), ?OCT(O2), ?OCT(O3) ->
    Val = (O1 * 8 + O2) * 8 + O3 - 73 * $0,
    {Val, [O1, O2, O3], Cs, N + 3};
scan_escape([O1, O2], _N) when ?OCT(O1), ?OCT(O2) ->
    more;
scan_escape([O1, O2 | Cs], N) when ?OCT(O1), ?OCT(O2) ->
    Val = (O1 * 8 + O2) - 9 * $0,
    {Val, [O1, O2], Cs, N + 2};
scan_escape([O1], _N) when ?OCT(O1) ->
    more;
scan_escape([O1 | Cs], N) when ?OCT(O1) ->
    {O1 - $0, [O1], Cs, N + 1};
%% \x{<hex digits>}
scan_escape([$x, ${ | Cs], N) ->
    scan_hex(Cs, N + 2, []);
scan_escape([$x], _N) ->
    more;
scan_escape([$x | eof], N) ->
    {eof, N + 1};
%% \x<2> hexadecimal digits
scan_escape([$x, H1, H2 | Cs], N) when ?HEX(H1), ?HEX(H2) ->
    Val = erlang:list_to_integer([H1, H2], 16),
    {Val, [$x, H1, H2], Cs, N + 3};
scan_escape([$x, H1], _Col) when ?HEX(H1) ->
    more;
scan_escape([$x | Cs], N) ->
    {error, Cs, {illegal, character}, N + 1};
%% \^X -> CTL-X
scan_escape([$^ = C0, $\n = C | Cs], N) ->
    {C, [C0, C], Cs, N + 2};
scan_escape([$^ = C0, C | Cs], N) when ?CHAR(C) ->
    Val = C band 31,
    {Val, [C0, C], Cs, N + 1 + char_byte_size(C)};
scan_escape([$^], _N) ->
    more;
scan_escape([$^ | eof], N) ->
    {eof, N + 1};
scan_escape([$\n = C | Cs], N) ->
    {C, [C], Cs, N + 1};
scan_escape([C0 | Cs], N) when ?UNICODE(C0) ->
    C = escape_char(C0),
    {C, [C0], Cs, N + char_byte_size(C0)};
scan_escape([C | Cs], N) when ?CHAR(C) ->
    {error, Cs, {illegal, character}, N + char_byte_size(C)};
scan_escape([], _N) ->
    more;
scan_escape(eof, N) ->
    {eof, N}.

scan_hex([C | Cs], N, Wcs) when ?HEX(C) ->
    scan_hex(Cs, N + 1, [C | Wcs]);
scan_hex(Cs, N, Wcs) ->
    scan_esc_end(Cs, N, Wcs, 16, "x{").

scan_esc_end([$} | Cs], N, Wcs0, B, Str0) ->
    Wcs = lists:reverse(Wcs0),
    case catch erlang:list_to_integer(Wcs, B) of
        Val when ?UNICODE(Val) ->
            {Val, Str0 ++ Wcs ++ [$}], Cs, N + 1};
        _ ->
            {error, Cs, {illegal, character}, N + 1}
    end;
scan_esc_end([], _N, _Wcs, _B, _Str0) ->
    more;
scan_esc_end(eof, N, _Wcs, _B, _Str0) ->
    {eof, N};
scan_esc_end(Cs, N, _Wcs, _B, _Str0) ->
    {error, Cs, {illegal, character}, N}.

% \n = LF
escape_char($n) -> $\n;
% \r = CR
escape_char($r) -> $\r;
% \t = TAB
escape_char($t) -> $\t;
% \v = VT
escape_char($v) -> $\v;
% \b = BS
escape_char($b) -> $\b;
% \f = FF
escape_char($f) -> $\f;
% \e = ESC
escape_char($e) -> $\e;
% \s = SPC
escape_char($s) -> $\s;
% \d = DEL
escape_char($d) -> $\d;
escape_char(C) -> C.

scan_number(Cs, St, Off, Toks, {Ncs, Us}) ->
    scan_number(Cs, St, Off, Toks, Ncs, Us).

scan_number([C | Cs], St, Off, Toks, Ncs, Us) when ?DIGIT(C) ->
    scan_number(Cs, St, Off, Toks, [C | Ncs], Us);
scan_number([$_, Next | Cs], St, Off, Toks, [Prev | _] = Ncs, _Us) when
    ?DIGIT(Next) andalso ?DIGIT(Prev)
->
    scan_number(Cs, St, Off, Toks, [Next, $_ | Ncs], with_underscore);
scan_number([$_] = Cs, _St, Off, Toks, Ncs, Us) ->
    {more, {Cs, Off, Toks, {Ncs, Us}, fun scan_number/5}};
scan_number([$., C | Cs], St, Off, Toks, Ncs, Us) when ?DIGIT(C) ->
    scan_fraction(Cs, St, Off, Toks, [C, $. | Ncs], Us);
scan_number([$.] = Cs, _St, Off, Toks, Ncs, Us) ->
    {more, {Cs, Off, Toks, {Ncs, Us}, fun scan_number/5}};
scan_number([$# | Cs] = Cs0, St, Off, Toks, Ncs0, Us) ->
    Ncs = lists:reverse(Ncs0),
    case catch list_to_integer(remove_digit_separators(Ncs, Us)) of
        B when B >= 2, B =< 1 + $Z - $A + 10 ->
            Bcs = Ncs ++ [$#],
            scan_based_int(Cs, St, Off, Toks, B, [], Bcs, no_underscore);
        B ->
            Len = length(Ncs),
            scan_error({base, B}, Off, Off + Len, Cs0)
    end;
scan_number([] = Cs, _St, Off, Toks, Ncs, Us) ->
    {more, {Cs, Off, Toks, {Ncs, Us}, fun scan_number/5}};
scan_number(Cs, St, Off, Toks, Ncs0, Us) ->
    Ncs = lists:reverse(Ncs0),
    case catch list_to_integer(remove_digit_separators(Ncs, Us)) of
        N when is_integer(N) ->
            tok3(Cs, St, Off, Toks, integer, N, length(Ncs));
        _ ->
            scan_error({illegal, integer}, Off, Off + length(Ncs), Cs)
    end.

remove_digit_separators(Number, no_underscore) ->
    Number;
remove_digit_separators(Number, with_underscore) ->
    [C || C <- Number, C =/= $_].

-define(BASED_DIGIT(C, B),
    ((?DIGIT(C) andalso C < $0 + B) orelse
        (C >= $A andalso B > 10 andalso C < $A + B - 10) orelse
        (C >= $a andalso B > 10 andalso C < $a + B - 10))
).

scan_based_int(Cs, St, Off, Toks, {B, NCs, BCs, Us}) ->
    scan_based_int(Cs, St, Off, Toks, B, NCs, BCs, Us).

scan_based_int([C | Cs], St, Off, Toks, B, Ncs, Bcs, Us) when
    ?BASED_DIGIT(C, B)
->
    scan_based_int(Cs, St, Off, Toks, B, [C | Ncs], Bcs, Us);
scan_based_int([$_, Next | Cs], St, Off, Toks, B, [Prev | _] = Ncs, Bcs, _Us) when
    ?BASED_DIGIT(Next, B) andalso ?BASED_DIGIT(Prev, B)
->
    scan_based_int(Cs, St, Off, Toks, B, [Next, $_ | Ncs], Bcs, with_underscore);
scan_based_int([$_] = Cs, _St, Off, Toks, B, NCs, BCs, Us) ->
    {more, {Cs, Off, Toks, {B, NCs, BCs, Us}, fun scan_based_int/5}};
scan_based_int([] = Cs, _St, Off, Toks, B, NCs, BCs, Us) ->
    {more, {Cs, Off, Toks, {B, NCs, BCs, Us}, fun scan_based_int/5}};
scan_based_int(Cs, St, Off, Toks, B, Ncs0, Bcs, Us) ->
    Ncs = lists:reverse(Ncs0),
    Len = length(Bcs) + length(Ncs),
    case catch erlang:list_to_integer(remove_digit_separators(Ncs, Us), B) of
        N when is_integer(N) ->
            tok3(Cs, St, Off, Toks, integer, N, Len);
        _ ->
            scan_error({illegal, integer}, Off, Off + Len, Cs)
    end.

scan_fraction(Cs, St, Off, Toks, {Ncs, Us}) ->
    scan_fraction(Cs, St, Off, Toks, Ncs, Us).

scan_fraction([C | Cs], St, Off, Toks, Ncs, Us) when ?DIGIT(C) ->
    scan_fraction(Cs, St, Off, Toks, [C | Ncs], Us);
scan_fraction([$_, Next | Cs], St, Off, Toks, [Prev | _] = Ncs, _Us) when
    ?DIGIT(Next) andalso ?DIGIT(Prev)
->
    scan_fraction(Cs, St, Off, Toks, [Next, $_ | Ncs], with_underscore);
scan_fraction([$_] = Cs, _St, Off, Toks, Ncs, Us) ->
    {more, {Cs, Off, Toks, {Ncs, Us}, fun scan_fraction/5}};
scan_fraction([E | Cs], St, Off, Toks, Ncs, Us) when E =:= $e; E =:= $E ->
    scan_exponent_sign(Cs, St, Off, Toks, [E | Ncs], Us);
scan_fraction([] = Cs, _St, Off, Toks, Ncs, Us) ->
    {more, {Cs, Off, Toks, {Ncs, Us}, fun scan_fraction/5}};
scan_fraction(Cs, St, Off, Toks, Ncs, Us) ->
    float_end(Cs, St, Off, Toks, Ncs, Us).

scan_exponent_sign(Cs, St, Off, Toks, {Ncs, Us}) ->
    scan_exponent_sign(Cs, St, Off, Toks, Ncs, Us).

scan_exponent_sign([C | Cs], St, Off, Toks, Ncs, Us) when
    C =:= $+; C =:= $-
->
    scan_exponent(Cs, St, Off, Toks, [C | Ncs], Us);
scan_exponent_sign([] = Cs, _St, Off, Toks, Ncs, Us) ->
    {more, {Cs, Off, Toks, {Ncs, Us}, fun scan_exponent_sign/5}};
scan_exponent_sign(Cs, St, Off, Toks, Ncs, Us) ->
    scan_exponent(Cs, St, Off, Toks, Ncs, Us).

scan_exponent(Cs, St, Off, Toks, {Ncs, Us}) ->
    scan_exponent(Cs, St, Off, Toks, Ncs, Us).

scan_exponent([C | Cs], St, Off, Toks, Ncs, Us) when ?DIGIT(C) ->
    scan_exponent(Cs, St, Off, Toks, [C | Ncs], Us);
scan_exponent([$_, Next | Cs], St, Off, Toks, [Prev | _] = Ncs, _) when
    ?DIGIT(Next) andalso ?DIGIT(Prev)
->
    scan_exponent(Cs, St, Off, Toks, [Next, $_ | Ncs], with_underscore);
scan_exponent([$_] = Cs, _St, Off, Toks, Ncs, Us) ->
    {more, {Cs, Off, Toks, {Ncs, Us}, fun scan_exponent/5}};
scan_exponent([] = Cs, _St, Off, Toks, Ncs, Us) ->
    {more, {Cs, Off, Toks, {Ncs, Us}, fun scan_exponent/5}};
scan_exponent(Cs, St, Off, Toks, Ncs, Us) ->
    float_end(Cs, St, Off, Toks, Ncs, Us).

float_end(Cs, St, Off, Toks, Ncs0, Us) ->
    Ncs = lists:reverse(Ncs0),
    case catch list_to_float(remove_digit_separators(Ncs, Us)) of
        F when is_float(F) ->
            tok3(Cs, St, Off, Toks, float, F, length(Ncs));
        _ ->
            scan_error({illegal, float}, Off, Off + length(Ncs), Cs)
    end.

skip_comment([C | Cs], St, Off, Toks, N) when C =/= $\n, ?CHAR(C) ->
    case ?UNICODE(C) of
        true ->
            skip_comment(Cs, St, Off, Toks, N + char_byte_size(C));
        false ->
            scan_error({illegal, character}, Off, Off + N + char_byte_size(C), Cs)
    end;
skip_comment([] = Cs, _St, Off, Toks, N) ->
    {more, {Cs, Off, Toks, N, fun skip_comment/5}};
skip_comment(Cs, St, Off, Toks, N) ->
    scan1(Cs, St, Off + N, Toks).

tok2(Cs, St, Off, Toks, Item, Len) ->
    Token = {Item, {Off, Off + Len}},
    scan1(Cs, St, Off + Len, [Token | Toks]).

tok3(Cs, St, Off, Toks, Item, Value, Len) ->
    Token = {Item, {Off, Off + Len}, Value},
    scan1(Cs, St, Off + Len, [Token | Toks]).

scan_error(Error, StartOff, EndOff, Rest) ->
    {{error, {{StartOff, EndOff}, ?MODULE, Error}, EndOff}, Rest}.

-spec reserved_word(Atom :: atom()) -> boolean().
reserved_word('after') -> true;
reserved_word('begin') -> true;
reserved_word('case') -> true;
reserved_word('try') -> true;
reserved_word('cond') -> true;
reserved_word('catch') -> true;
reserved_word('andalso') -> true;
reserved_word('orelse') -> true;
reserved_word('end') -> true;
reserved_word('fun') -> true;
reserved_word('if') -> true;
reserved_word('let') -> true;
reserved_word('of') -> true;
reserved_word('receive') -> true;
reserved_word('when') -> true;
reserved_word('bnot') -> true;
reserved_word('not') -> true;
reserved_word('div') -> true;
reserved_word('rem') -> true;
reserved_word('band') -> true;
reserved_word('and') -> true;
reserved_word('bor') -> true;
reserved_word('bxor') -> true;
reserved_word('bsl') -> true;
reserved_word('bsr') -> true;
reserved_word('or') -> true;
reserved_word('xor') -> true;
reserved_word('maybe') -> true;
reserved_word('else') -> true;
reserved_word(_) -> false.

char_byte_size(C) ->
    %% TODO: more efficient implementation
    byte_size(<<C/utf8>>).
