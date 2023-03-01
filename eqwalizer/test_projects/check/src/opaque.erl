%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(opaque).

-export_type([opair/2, o_cycle/0, contravariant/1, o_trans_invalid/0]).

-compile([export_all, nowarn_export_all]).

%% naive handling of opaque types
%% (D27986305)
-opaque opair(A, B) :: {A, B}.

-spec swap(opair(A, B))
      -> opair(B, A).
swap({A, B}) -> {B, A}.

-spec otp_1_neg(sets:set({ok, error})) ->
  sets:set({ok, ok}).
otp_1_neg(X) ->
  X.

-spec test1_neg(misc:o()) -> ok.
test1_neg({X}) -> X.

-spec test2_neg(misc:o()) -> ok.
test2_neg(X=#{a := b}) -> X.

-spec test3_neg(misc:o()) -> tuple().
test3_neg(X) -> X.

-spec to_any(misc:o()) -> term().
to_any(X) -> X.

-spec use_to_any(misc:o()) ->
  tuple().
use_to_any(X) ->
  X2 = to_any(X),
  case X2 of
    X2 when is_tuple(X2) -> X2
  end.

-spec test4(misc:o()) -> none().
test4(X) ->
  _ = case X of
        X2 when is_tuple(X2) ->
          X2
      end,
  throw(not_implemented).

-spec test4_neg(misc:o()) -> none().
test4_neg(X) ->
  case X of
        X2 when is_tuple(X2) ->
          X2
  end.

-spec test5_neg(misc:o()) -> tuple().
test5_neg(X) ->
  Res = X,
  Res.

-spec test6(misc:o()) -> misc:o().
test6(X) -> X.

-record(tbox, {
  field :: misc:o()
}).

-spec test7(misc:o()) -> #tbox{}.
test7(X) ->
  #tbox{field = X}.

-spec test8(#tbox{}) -> misc:o().
test8(#tbox{field = X}) ->
  X.

-spec test9_neg(misc:o_rec()) -> ok.
test9_neg({rec}) -> ok.

-spec test10(
    misc:rec_w_opaque_field()
) -> ok.
test10({rec_w_opaque_field,
  {_, _}}
) -> ok.

-spec test11_neg(
    misc:rec_w_opaque_field()
) -> {ok}.
test11_neg({rec_w_opaque_field,
  {X}}
) -> X.

-spec test12_neg(
    misc:o()
) -> ok.
test12_neg(X) when X =:= #{} -> ok.

-spec test13_neg(misc:o()) -> boolean().
test13_neg(X) when X =:= {ok} -> true;
test13_neg(_) -> false.

-spec test14_neg(misc:o()) -> true.
test14_neg(X) -> X =:= {ok}.

-spec test15(misc:o()) ->
  misc:o() | a.
test15(X) ->
  ttt(X, a).

-spec test16(misc:o() | a) ->
  misc:o() | a.
test16(X) -> X.

-spec test17_neg(misc:o() | a) ->
  {ok} | a.
test17_neg(X) -> X.

-spec test18_neg(
    misc:o() | a
) -> misc:o().
test18_neg(X) -> X.

-spec test19(
    {misc:o() | a}
) -> misc:o() | a.
test19({X}) -> X.

-spec test20_neg(
    misc:o() | a
) -> misc:o().
test20_neg(X) -> X.


-spec ttt(T, T) -> T.
ttt(T, T) -> T.

-spec test21(misc:o()) -> misc:o().
test21(X) ->
  ttt(X, X).

-spec test22(misc:o()) ->
  misc:o() | a.
test22(X) ->
  ttt(X, a).

-opaque o_cycle() :: misc:o_cycle().

-spec use_o_cycle1(misc:o_cycle()) ->
  opaque:o_cycle().
use_o_cycle1(X) -> X.

-spec use_o_cycle2(opaque:o_cycle()) ->
  misc:o_cycle().
use_o_cycle2(X) -> X.

-opaque contravariant(T)
                :: fun((T) -> ok).

-spec use_contra_neg(fun((ok) -> ok))
      -> contravariant(ok).
use_contra_neg(X) -> X.

-spec test23_wip(misc:o()) -> {ok}.
test23_wip(X)
  when element(1, {X})
    =:= element(1, {{ok}}) ->
  X.

-spec test24_neg(misc:o()) -> term().
test24_neg(X)
  when X =:= self() ->
    X.

-spec test25(misc:o()) -> term().
test25(X)
  when {{X}} =:= {self()} ->
  X.

-spec test26_neg(misc:o()) -> term().
test26_neg(X)
  when X < ok ->
  X.

-spec test27_wip(misc:o()) -> term().
test27_wip(X)
  when {} < hd([X]) ->
  X.

-spec test28(misc:o()) -> term().
test28(X)
  when {} < hd([[X]]) ->
  X.

-spec get_stacktrace1() -> term().
get_stacktrace1() ->
  case
    erlang:process_info(
      self(),
      current_stacktrace
    )
  of
    {current_stacktrace, ST} ->
      case ST of
        [
          Z={_,
            _,
            _,
            _}
          | _Tl
        ] -> Z
      end
  end.

-spec get_stacktrace2() -> term().
get_stacktrace2() ->
  case
    erlang:process_info(
      self(),
      current_stacktrace
    )
  of
    {current_stacktrace, [
      Z={_,
        _,
        _,
        _}
      | _Tl]} -> Z
  end.


-spec test29_neg(misc:o()) -> term().
test29_neg(X) when X =:= {1.0} ->
  ok.

-spec test30_neg(misc:o()) -> term().
test30_neg(X) when X =:= "" ->
  ok.

-spec test31_neg(misc:o()) -> term().
test31_neg(X) when X =:= [a|improper] ->
  ok.

-spec test32_neg(misc:o()) -> term().
test32_neg(X) when X =:= #{1 => 1} ->
  ok.

-spec test33_neg(misc:o()) -> term().
test33_neg(X) when X =:= #{1 => 1} ->
  ok.

-spec test34_neg(misc:o()) -> term().
test34_neg(X) when X =:= <<>> ->
  ok.

-record(rec, {field :: string()}).

-spec test35_neg(misc:o()) -> term().
test35_neg(X) when X =:= #rec.field ->
  ok.

-spec ints_o_neg([misc:int_result_o()])
      -> [integer()].
ints_o_neg([]) -> [];
ints_o_neg([F | Fs]) ->
  case F() of
    {ok, I} -> [I | ints_o_neg(Fs)];
    error -> ints_o_neg(Fs)
  end.

% we don't check unions
% for opacity violations
-spec ints_o_neg2([
  misc:int_result_o()
  | misc:int_result_o()
]) ->
  [integer()].
ints_o_neg2([]) -> [];
ints_o_neg2([F | Fs]) ->
  case F() of
    {ok, I} -> [I | ints_o_neg2(Fs)];
    error -> ints_o_neg2(Fs)
  end.

-type invalid() :: _T.

-opaque o_trans_invalid() :: invalid().
