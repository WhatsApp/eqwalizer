%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(recursive_aliases).

-compile([export_all, nowarn_export_all]).
-export_type([
    x_cycle/0,
    opaque_id/1,
    foo4/1,
    foo5/1,
    foo6/1,
    pchainB/1,
    foo7_neg/1,
    bar1/1,
    bar2/1,
    bar3/1,
    test1/0,
    test2/2,
    test3/2,
    test4/0
]).

-type chainA()
    :: nil
     | {a, chainA()}.

-type chainB()
    :: nil
     | {b, chainB()}.

-type chainAB()
    :: nil
     | {a, chainAB()}
     | {b, chainAB()}.

-type pchainA(Elem)
    :: nil
     | {a, Elem, pchainA(Elem)}.

-type pchainB(Elem)
    :: nil
     | {b, Elem, pchainB(Elem)}.

-type pchainAB(Elem)
    :: nil
     | {a, pchainAB(Elem)}
     | {b, pchainAB(Elem)}.

-spec subtype1(chainA()) -> chainAB().
subtype1(C) -> C.

-spec subtype2(chainB()) -> chainAB().
subtype2(C) -> C.

-spec subtype3_neg(chainA()) -> chainB().
subtype3_neg(C) -> C.

-spec subtype4_neg(chainB()) -> chainA().
subtype4_neg(C) -> C.

-spec subtype5_neg(chainAB()) -> chainA().
subtype5_neg(C) -> C.

-spec subtype6_neg(chainAB()) -> chainB().
subtype6_neg(C) -> C.

-type non_prod() :: non_prod().

-spec non_prod_id(non_prod()) ->
    non_prod() | number().
non_prod_id(N) -> N.



-spec subtype_p1_neg(
    pchainA(atom())
) -> pchainAB(atom()).
subtype_p1_neg(C) -> C.

%%

-type mChainA()
    :: nil
     | #{a := mChainA()}.

-type mChainB()
    :: nil
     | #{b := mChainB()}.

-type mChainAB()
    :: nil
     | #{a := mChainAB()}
     | #{b := mChainAB()}.

-spec m_subtype1(mChainA()) -> mChainAB().
m_subtype1(C) -> C.

-spec m_chainA_size(mChainA()) ->
    number().
m_chainA_size(nil) ->
    1;
m_chainA_size(#{a := Chain}) ->
    1 + m_chainA_size(Chain).

-spec m_chainB_size_neg(mChainB()) ->
    number().
m_chainB_size_neg(nil) ->
    1;
m_chainB_size_neg(#{b := Chain}) ->
    1 + m_chainA_size(Chain).

-type fan() :: nil | #{pid() => fan()}.

-spec fan_flatten(fan()) ->
    [fan()].
fan_flatten(nil) ->
    [nil];
fan_flatten(M) when is_map(M) ->
    maps:values(M).

-spec fan_x(fan(), term()) ->
    {pid(), fan()}.
fan_x(Fan, Pid) ->
    case Fan of
        #{Pid := Fan1} -> {Pid, Fan1};
        nil -> {self(), nil}
    end.

-spec fan_y(fan()) ->
    #{atom() | pid() => fan() | number()}.
fan_y(M) when is_map(M) -> M#{a => 1}.

-spec fun_fan(fan()) ->
    atom().
fun_fan(F) -> F().

%%%%

-type xChainAB() :: #{
    a := xChainAB(),
    b => xChainAB()
}.
-spec x_foo(atom(), xChainAB()) ->
    xChainAB().
x_foo(A, C) ->
    case C of
        #{A := C1} -> C1
    end.

-spec y_foo(xChainAB()) ->
    #{a := number(), b => xChainAB()}.
y_foo(C) ->
    C#{a => 1}.

-spec z_foo_1(xChainAB()) ->
    #{a := number(), b => xChainAB()}.
z_foo_1(C) ->
    C#{a := 1}.

-spec z_foo_2_neg(xChainAB()) ->
    #{a := xChainAB(), b := atom()}.
z_foo_2_neg(C) ->
    C#{b := ok}.

%%

-type my_tree() ::
    n | [my_tree()].
-spec left_tree(my_tree()) ->
    my_tree().
left_tree(n) ->
    n;
left_tree([H | _]) ->
    left_tree(H).

-spec subtype_5_pos(
    pchainA(a)
) -> pchainA(a | b).
subtype_5_pos(C) -> C.

-spec subtype_6_neg(
    pchainA(a)
) -> pchainA(b | c).
subtype_6_neg(C) -> C.

-spec un_pchain(pchainA(T)) -> [T].
un_pchain({a, Elem, Chain}) ->
  [Elem|un_pchain(Chain)];
un_pchain(nil) -> [].

-spec use_un_pchain_1(pchainA(T)) -> [T].
use_un_pchain_1(X) ->
  un_pchain(X).

-spec use_un_pchain_2(pchainA(a)) -> [a].
use_un_pchain_2(X) ->
  un_pchain(X).

-type mu_bad() :: mu_bad() | {a}.

-spec to_mu_bad_neg() ->
  mu_bad().
to_mu_bad_neg() -> {a}.

-spec test_mu_bad_neg(mu_bad()) ->
  pid().
test_mu_bad_neg({X}) -> X.

-type mu(A) :: {mu, mu(mu(A))}.

-spec test2_neg(mu(A)) -> mu(mu(A)).
test2_neg(M) -> M.

-type mu2() :: {mu, atom() | mu2()}.

-spec test3() -> mu2().
test3() -> {mu, a}.

-spec test4_neg(mu2()) -> pid().
test4_neg({mu, M}) -> M.

% xml types adapted from wa_types
-type xmlattr()  :: {string(), string()}.
-type xmlattrs() :: [xmlattr()].

-record(el, {
  name       :: string(),
  attrs = [] :: xmlattrs(),
  els = []   :: [el()]
}).
-type el() :: #el{}.

-spec test_el() -> el().
test_el() -> #el{
  name = "",
  attrs = [{"", ""}],
  els = [#el{
    name = "",
    attrs = [{"", ""}],
    els = []
  }]
}.

-type xmlstreamelement() :: el()
  | xmlstreamstart
  | xmlstreamend.

-type xmlstreamelements() ::
  xmlstreamelement()
  | [xmlstreamelement()]
  | [xmlstreamelements()].

% Adapted from xmlstreamelements/0
% by inlining an alias
-type xmlstreamelements_ok() :: #el{}
| xmlstreamstart
| xmlstreamend
| [xmlstreamelement()]
| [xmlstreamelements_ok()].

-spec test_xmlstreamelements_neg() ->
  xmlstreamelements().
test_xmlstreamelements_neg() ->
  #el{name = ""}.

-spec test_xmlstreamelements_ok() ->
  xmlstreamelements_ok().
test_xmlstreamelements_ok() ->
  #el{name = ""}.

% adapted from OTP's erl_parse.beam,
% which is generated from erl_parse.yrl
-type abstract_expr() :: literal |
  chainA() | af_match(abstract_expr()).
-type af_match(T) :: {match, T}.

% Adapted from abstract_expr/0
% by inlining an alias
-type abstract_expr_ok() :: literal |
chainA() | {match, (abstract_expr_ok())}.

-spec test_af_abstract_expr() ->
  abstract_expr().
test_af_abstract_expr() ->
  literal.

-spec test_af_abstract_expr_ok() ->
  abstract_expr_ok().
test_af_abstract_expr_ok() ->
  literal.

-type id(T) :: T | T.
-type mu_bad2() :: id(mu_bad2()).
-type mu_bad2b() :: id(mu_bad2()).

-type mu_bad3() :: id(mu_bad3()).

-spec to_mu_bad3_neg(mu_bad2()) ->
    mu_bad3().
to_mu_bad3_neg(X) -> X.

-spec to_mu_bad2b_neg(mu_bad2()) ->
  mu_bad2b().
to_mu_bad2b_neg(X) -> X.

-type indirect2() :: mutual2().
-type indirect1() :: mutual1().

-type mutual1() :: indirect1()
  | indirect2().
-type mutual2() :: indirect1()
  | indirect2().

-spec test_mutual_neg(mutual1()) ->
    mutual2().
test_mutual_neg(X) -> X.

-type mu_tuple(T) :: {T}
  | {mu_tuple(ok)}.
-type mu4() :: mu_tuple(mu4()) | ok.






-spec test_mu4() -> mu4().
test_mu4() -> ok.

-type tuple(T) :: {T}.
-type mu5() :: tuple(mu5()) | ok.

% similar to mu4() example,
% except tuple/1 is non-recursive
-spec test_mu5() -> mu5().
test_mu5() -> ok.

-opaque opaque_id(T) :: T | T.
-type mu_bad5() :: opaque_id(mu_bad5()).
-type mu_bad5b() :: opaque_id(mu_bad5()).

-spec test_mu_bad5(
    mu_bad5()
) -> mu_bad5b().
test_mu_bad5(X) -> X.

-spec test_non_productive_ty_var_use_neg
    () -> opaque_id(nok).
test_non_productive_ty_var_use_neg() ->
  nok.

-type my_list(A)
:: nil
| {cons, A, my_list(A)}.

-type tree1()
    :: {leaf, integer()}
     | list(tree1()).

-type tree2()
    :: {leaf, integer()}
     | my_list(tree2()).

-type tree3(A)
    :: {leaf, A}
     | {node, list(tree3(A))}.

-type tree4(A)
    :: {leaf, A}
     | {node, my_list(tree4(A))}.

 -spec to_tree1() -> tree1().
to_tree1() ->
  {leaf, 1}.

-spec to_tree2() -> tree2().
to_tree2() ->
  {leaf, 1}.

-spec to_tree3() -> tree3(ok).
to_tree3() ->
  {leaf, ok}.

-spec to_tree4() -> tree4(ok).
to_tree4() ->
  {leaf, ok}.

-type foo(T) :: T | {foo(T)} | b.
-type recur() :: foo(recur()) | a.

% should not infinite-loop, see D30103144
-spec un_recur_neg(recur()) -> ok.
un_recur_neg({X}) ->
  un_recur_neg(X).

-spec term_to_recur_neg(term()) ->
  recur().
term_to_recur_neg(X) -> X.

-type foo4(T) :: {T, T}.

-type foo5(T) :: {unit, T}.

-type foo6(T) :: {T, unit}.

-type foo7_neg(T) ::
  {unit, foo7_neg(foo7_neg(T))}.

-type bar1(T) :: {bar2(T), unit}.

-type bar2(T) :: {bar3(T)}.

-type bar3(T) :: bar1(T).

-type test1() :: misc:o() | {test1()}.
-type test2(X, Y) :: misc:o() | {
  misc:o(),
  test2(X, Y), {},
  {a},
  opaque:opair(a, b),
  fun((ok) -> test3(X, Y)),
  #{string() => v},
  #{string() => v},
  #{k := a, k2 := b }
}.
-type test3(X, Y) :: misc:o() | {
  misc:o(),
  test2(Y, X),
  {a},
  opaque:opair(a, b),
  fun((ok) -> test2(Y, X)),
  #{string() => v},
  #{number() => v},
  #{k := v, k2 => v2}
}.
-type test4() :: {test4()}.

-type x_cycle() :: misc:x_cycle().

-type struct_literal() ::
    {string(), [{string(), thrift_constant()}]}.

-type thrift_constant() ::
    {bool, boolean()}
    | {int, integer()}
    | {float, string()}
    | {string, string()}
    | {identifier, string()}
    | {list, list(thrift_constant())}
    | {map, [{thrift_constant(), thrift_constant()}]}
    | {struct, struct_literal()}.

-spec trick_me
({thrift_constant(),
thrift_constant()},
thrift_constant()) -> none().
trick_me(X, X) -> X.

-type simple_id(X) :: X.

-type t_tree1(Z) ::
  {leaf, Z} |
  {node, t_tree1(Z)}.

%% Typed Racket accepts it:
%% (define-type (Id a) a)
%% (define-type (Tree a)
%%  (Union (List 'leaf a)
%%         (List 'node (Tree (Id a)))))
%% It seems that under the hood Typed Racket
%% separates type aliases into recursive and
%% non-recursive ones and eagerly (call-by-name)
%% unfolds non-recursive one.
-type t_tree2(Z) ::
  {leaf, Z} |
  {node, t_tree2(simple_id(Z))}.

-type t_tree3(Z) ::
  {leaf, Z} |
  {node, t_tree3(t_tree3(Z))}.

-type stream1(Z) ::
  {chunk, stream1(Z)}.

%% Typed Racket accepts it
%% (define-type (Id a) a)
%% (define-type
%%   (Stream2 a)
%%     (List 'chunk (Stream2 (Id a))))
-type stream2(Z) ::
  {chunk, stream2(simple_id(Z))}.

%% Typed Racket rejects it
%% (define-type (DStream a)
%%   (List 'chunk (DStream (List a))))
%% recursive type cannot be applied
%% at a different type in its recursive invocation
%%  type: DStream
%%  new argument name: #<syntax:3-unsaved-editor:11:22 a>
%%  new argument: (List a)
%%  new arguments...: ((List a))
-type d_stream(Z) ::
  {chunk, d_stream([Z])}.

-type invalid_ty() :: non_exist:invalid().

-type invalid_rec() ::
  invalid_ty() | invalid_transitive().

-type invalid_transitive() ::
  {a, invalid_rec()}.

-spec use_invalid
  (invalid_transitive()) -> a.
use_invalid({A, _}) -> A.
