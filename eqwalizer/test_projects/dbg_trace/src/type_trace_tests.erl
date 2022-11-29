%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(type_trace_tests).
-export([run/0]).

-record(rec, {id :: integer(), name :: atom()}).

run() ->
  none_err(),
  term_ok(),
  atom_lit_err1(),
  atom_lit_err2(),
  any_fun_ok(),
  any_fun_err(),
  fun_ok(),
  fun_err(),
  any_tuple_ok(),
  any_tuple_err(),
  tuple_ok(),
  tuple_err1(),
  tuple_err2(),
  nil_ok(),
  nil_err(),
  list_ok(),
  list_err1(),
  list_err2(),
  binary_ok(),
  binary_err(),
  pid_ok(),
  pid_err(),
  number_ok1(),
  number_ok2(),
  number_err(),
  union_ok(),
  union_err(),
  reference_ok(),
  reference_err(),
  record_ok1(),
  record_ok2(),
  record_err1(),
  record_err2(),
  record_err3(),
  dictionary_ok(),
  dictionary_err1(),
  dictionary_err2(),
  dictionary_err3(),
  shape_ok1(),
  shape_ok2(),
  shape_err1(),
  shape_err2(),
  shape_err3(),
  shape_err4(),
  shape_err5(),
  opaque_ok1(),
  opaque_ok2(),
  opaque_err1(),
  opaque_err2(),
  string_ok(),
  ok.

-type union(A, B) :: A | B.
-opaque box() :: {box, term()}.

-spec none_err() -> none().
none_err() -> ok.

-spec term_ok() -> term().
term_ok() -> ok.

-spec atom_lit_err1() -> ok.
atom_lit_err1() -> err.

-spec atom_lit_err2() -> ok.
atom_lit_err2() -> [].

-spec any_fun_ok() -> fun().
any_fun_ok() ->
  fun() -> ok end.

-spec any_fun_err() -> fun().
any_fun_err() ->
  #{}.

-spec fun_ok() -> fun(() -> term()).
fun_ok() ->
  fun() -> ok end.

-spec fun_err() -> fun(() -> term()).
fun_err() ->
  #{}.

-spec any_tuple_ok() -> tuple().
any_tuple_ok() -> {1, {}}.

-spec any_tuple_err() -> tuple().
any_tuple_err() -> << >>.

-spec tuple_ok() -> {integer(), {}}.
tuple_ok() -> {1, {}}.

-spec tuple_err1() -> {integer(), {}}.
tuple_err1() -> {1, 1}.

-spec tuple_err2() -> {ok, ok}.
tuple_err2() -> {ok}.

-spec nil_ok() -> [].
nil_ok() -> [].

-spec nil_err() -> [].
nil_err() -> [1, 2].

-spec list_ok() -> [atom()].
list_ok() -> [a, b, c, d].

-spec list_err1() -> [atom()].
list_err1() -> [a, b, c, {}].

-spec list_err2() -> [atom()].
list_err2() -> {}.

-spec binary_ok() -> binary().
binary_ok() -> <<1, 2, 3>>.

-spec binary_err() -> binary().
binary_err() -> [<<1, 2, 3>>].

-spec pid_ok() -> pid().
pid_ok() -> self().

-spec pid_err() -> pid().
pid_err() -> [<<1, 2, 3>>].

-spec number_ok1() -> number().
number_ok1() -> 1.

-spec number_ok2() -> number().
number_ok2() -> 1.5.

-spec number_err() -> number().
number_err() -> {1.5}.

-spec union_ok() -> union(a, b).
union_ok() -> b.

-spec union_err() -> union(a, b).
union_err() -> c.

-spec reference_ok() -> reference().
reference_ok() -> erlang:make_ref().

-spec reference_err() -> reference().
reference_err() -> erlang:self().

-spec record_ok1() -> #rec{}.
record_ok1() -> #rec{id = 1, name = name}.

-spec record_ok2() -> #rec{}.
record_ok2() -> {rec, 1, name}.

-spec record_err1() -> #rec{}.
record_err1() -> #rec{id = id, name = name}.

-spec record_err2() -> #rec{}.
record_err2() -> {rec, id, name}.

-spec record_err3() -> #rec{}.
record_err3() -> {rec, id}.

-spec dictionary_ok() -> #{atom() => integer()}.
dictionary_ok() -> #{a => 1, b => 2}.

-spec dictionary_err1() -> #{atom() => integer()}.
dictionary_err1() -> #{a => 1, 2 => 2}.

-spec dictionary_err2() -> #{atom() => integer()}.
dictionary_err2() -> #{a => 1, b => b}.

-spec dictionary_err3() -> #{atom() => integer()}.
dictionary_err3() -> 1.

-spec shape_ok1() -> #{id := integer(), name => atom()}.
shape_ok1() -> #{id => 1, name => my_name}.

-spec shape_ok2() -> #{id := integer(), name => atom()}.
shape_ok2() -> #{id => 1}.

-spec shape_err1() -> #{id := integer(), name => atom()}.
shape_err1() -> #{id => id, name => my_name}.

-spec shape_err2() -> #{id := integer(), name => atom()}.
shape_err2() -> #{name => my_name}.

-spec shape_err3() -> #{id := integer(), name => atom()}.
shape_err3() -> #{1 => 1, name => my_name, id => 1}.

-spec shape_err4() -> #{id := integer(), name => atom()}.
shape_err4() -> #{name => my_name, id => 1, extra => ""}.

-spec shape_err5() -> #{id := integer(), name => atom()}.
shape_err5() -> [].

-spec opaque_ok1() -> opaque:opair(atom(), number()).
opaque_ok1() -> {id, 2}.

-spec opaque_ok2() -> box().
opaque_ok2() -> {box, 2}.

-spec opaque_err1() -> opaque:opair(atom(), number()).
opaque_err1() -> {1, 2}.

-spec opaque_err2() -> box().
opaque_err2() -> {box}.

-spec string_ok() -> string().
string_ok() -> "string".
