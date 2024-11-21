# eqWAlizer errors

This page references all eqWAlizer errors.

### incompatible_types

This error indicates that eqWAlizer detected an expression whose type is
incompatible with the context it is used in. This can have several
reasons.

#### Types don't match a spec at all

This is the most basic case, types that don't match at all:

```Erlang
-spec test_neg() -> number().
test_neg() ->
"oops". % Error

%  string_lit.
%  Expression has type:   string()
%  Context expected type: number()
```

eqWAlizer shows the position of the error (a string literal), what type
it deduced for the problematic expression, and what type the context
was expecting.

In the example above, the error can be fixed by changing the spec to say the
function returns `string()` or changing the code to return a `number()`–
it depends on the intent of the code.

#### Type used in a limited way, unknown to eqWAlizer

In another basic case, the type is too broadly defined but is used in a limited
way in a specific portion of the codebase. Due to complexity of or dependencies
on the type, changing spec is impractical.

```Erlang
-type a() :: number() | undefined.

-spec foo(A :: a()) -> number().
foo(A) ->
%% Due to constraints not shown here, I know that A here is always going to be a number
A + 2.
```

Here, modifying the spec would be simple, but in practice complex code paths often make
that impractical, so it is possible to refine the type using dynamic checks. A solution
is to add a guard to the problematic clause of the function:

```Erlang
-type a() :: number() | undefined.

-spec foo(A :: a()) -> number().
    foo(A) when A =/= undefined -> A+2.
```

If this is not possible, one can also add a dynamic check raising an error if `A` is
`undefined`:

```Erlang
-type a() :: number() | undefined.

-spec foo(A :: a()) -> number().
foo(A) ->
    (A =/= undefined) orelse error({invalid_type, "A should never be undefined"}),
    A + 2.
```

#### Types partially compatible

In more complicated cases, the type of an expression may be only partially
compatible with the expected type. In such cases, eqWAlizer will help make
the error message actionable by showing how the type of the expression
is incompatible with the expected type:

```Erlang
-spec test_neg() -> {ok, pid()}.
test_neg() ->
   case rand:uniform(2) of
       1 -> {ok, self()};
       2 -> {error, got_two}
   end.

% {error, got_two}.
% Expression has type:   {'error', 'got_two'}
% Context expected type: {'ok', pid()}
%
% {'error', 'got_two'} is incompatible with {'ok', pid()} because
% 'error' is incompatible with 'ok'.
```

#### Ill-typed application

The examples above involve the return type not matching the return type
given by the spec. Type errors can appear in other places, too, such as
when a function is called with the wrong argument type:

```Erlang
-spec test_neg() -> term().
    test_neg() ->
    list_to_atom(bad). % Error

% bad.
% Expression has type:   'bad'
% Context expected type: string()
```


### not_enough_info_to_branch

This error happens when there is not enough information to know which sub-spec
of an overloaded function should be used to type-check an application.

```Erlang
-spec overloaded
    (a | b) -> ok;
    (b) -> error.
overloaded(_) -> throw(not_implemented).

-spec test_neg() -> term().
test_neg() ->
    overloaded(b).

% overloaded(b). Not enough info to branch. Arg types: 'b'
```

In the code above, the spec of `overloaded/1` does not give enough information
to know whether `ok` or `error` is the return type when `overloaded/1` is called
with `b`.
If you see this error, most likely the problem is with the overloaded function,
not the function that is calling the overloaded function and can be fixed by
ensuring the sub-specs of overloaded functions do not have overlapping domains.
See [Overloaded Specs](./advanced.md#overloaded-specs).


### fun_arity_mismatch

This error indicates that a fun expression is called with the wrong number of arguments.

Example:
```Erlang
-spec test_neg() -> ok.
test_neg() ->
    (fun () -> ok end)(1, 2, 3).

% fun. fun with arity 0 used as fun with 3 arguments
```


### undefined_field

This error occurs when attempting to create a record without specifying the value for a field
when the field type doesn't allow `undefined` values.

Example:
```Erlang
-record(user, {name :: string(), id :: binary()}).

-spec create_user() -> #user{}.
create_user() -> #user{id = <<"000">>}.

% #user{...}: name is undefined
```


### unbound_var

This error indicates an unbound variable. Check the spelling of the variable.


### unbound_record

This error indicates an unbound record type. Check the spelling of the record name.


### opacity_violation

This error indicates that the code is doing something that relies on the definition
of a type defined with `-opaque`, such as comparing it via `<=`, or inspecting it
with a guard function such as `is_atom/1`.

Opacity checks enable safe and modular reasoning about code. For example, OTP team
defined `sets:set/1` as an opaque, which enabled them to change the representation
of sets from records to maps. This change in representation won't break client
code–as long as the client code doesn't violate the opacity of `sets:set/1` by
operating on sets as records.

These are some options for fixing code that violates opacity:

- See if the module that defines the opaque provides helper functions for working
with the type. For example, `sets:add_element/2` enables you to add items to a set
without messing with the underlying record and map representations.
- If such a helper function does not exist, you can add it: move the smallest
operation that relies on the definition of the opaque into the same module where
the opaque type is defined. Code in the same module as the `-opaque` type can rely
on the definition of the opaque.
- If the type doesn't seem like something that should be opaque, you can change
`-opaque` to `-type` where the opaque is defined.


### behaviour_does_not_exist

This error indicates that there is a `-behaviour(some_module)` attribute, where
`some_module` is not found. Check the spelling of the [behaviour](https://www.erlang.org/doc/design_principles/des_princ.html#behaviours).
Not the spelling of "behaviour" itself, that is misspelled by convention.


### missing_cb_implementation

This error indicates that the module is missing an implementation for one of the
callbacks for a [behaviour](https://www.erlang.org/doc/design_principles/des_princ.html#behaviours)
referenced in a `-behaviour` attribute. For example,
`Missing implementation for gen_server handle_info/2` means that a `handle_info/2`
function needs to be added to the module in order to implement `gen_server`.


### incorrect_param_type_in_cb_implementation

This error indicates that one of the parameter types for the function is not
compatible with the type defined in the [behaviour](https://www.erlang.org/doc/design_principles/des_princ.html#behaviours).


### incorrect_return_type_in_cb_implementation

This error indicates that the return type for the function is incompatible
with the return type expected by the [behaviour](https://www.erlang.org/doc/design_principles/des_princ.html#behaviours).


### cannot_locate_source

This error indicates that eqWAlizer cannot find a source file. This indicates
a bug in how eqWAlizer or its dependencies are configured. Oncall is
whatsapp_dev_infra.


### unknown_id

This error indicates that you're referencing something that doesn't exist.
For example:
```Erlang
-spec test() -> nok.
test() ->
    nonexistent:fooooon/9999. % Error
```

### non_exported_id

This error indicates that you're referencing a type ID that exists but is not exported.

Export the type in the module it is declared in using `-export_type([...])` to fix this issue.


### recursive_constraint

eqWAlizer does not understand recursive constraints. If you find a spec with
a recursive constraint, it is likely to be a mistake, like in the function
below:

```Erlang
-spec test() ->
    Recur when Recur :: {rec, Recur}. % Error
test() ->
    {rec, test()}.

% Recursive constraint: Rec
```

In cases like the below where the recursive constraint is really intended,
you can replace the constraint with a recursive type alias:

```Erlang
-type rec_type() :: {rec, rec_type()}.

-spec test() -> rec_type().
test() ->
    {rec, test()}.
```


### unbound_type_var

This error points out that a type variable is not bound, like _Key in the
example below.

```Erlang
-type ops() :: add_op(_Key). % Error

% _Key is unbound
```

This validation is similar to the validation that Just as the erlc does for
expressions: `main() -> _Key % Error: unbound variable`.

The fix depends on what the code should do:

- If the unbound variable should vary, take it as a parameter. For the example
above, that would mean rewriting to `-type ops(Key) :: add_op(Key)`;
- Otherwise, replace the variable with a type. For the example above, this is
one way of giving a meaningful type `-type ops() :: add_op(atom())`.


### type_alias_is_non_productive

The error message indicates that there is a recursive type such that replacing
the alias with its definition doesn't make any progress.

The easiest way to trigger such an error message is like this:
`-type loop(T) :: loop(T)`. Such a definition is helpful neither to machines
nor to humans: all it tells us is that a `loop(a)` is just a `loop(a)`.

An intuition for fixing this kind of error is to think of your type alias as a
function: does it infinite-loop without producing useful info? If so, remove
the infinite loop.


### type_var_in_record_field

Erlang does not allow parameters to -record definitions, so type variables in
record definitions are unbound variables.
```Erlang
-record(bad, {field :: {_Unbound, number()}}). % _Unbound is unbound
```

If you're using type variables in record definitions to give names to things,
try an annotation instead:
```Erlang
-record(good, {field :: {User :: wid(), number()}}). % OK
```

Otherwise, it is also possible to use *refined record fields*:
```Erlang
-record(good, {field :: eqwalizer:refinable{term(), number()}}).

-type good_refined(T) :: #good{field :: {T, number()}}.
```

For more information, see [Records and Tuples](./subtyping.md#records-and-tuples).


### ty_var_with_multiple_constraints

In a spec, in a when clause, it doesn't make sense to constrain something multiple times:
```Erlang
-spec foo(Thing) -> Thing when Thing :: atom(), Thing :: pid().
```

The fix is to pick a single constraint, such as:

```Erlang
-spec foo(Thing) -> Thing when Thing :: atom().
```

or write the type directly, without using constraint syntax:

```Erlang
-spec foo(atom()) -> atom().
```


### bad_map_key

This error indicates that a property of a map type is invalid, which can occur for
two reasons.

First, a property may be marked as required (`:=`)
in a place where only an optional property would make sense (`=>`). For example,
the following code says that `atom()` is required. But code calling the function
cannot know which atom is required, so the information in the spec is not
actionable:

```Erlang
-spec test(_) -> #{
    atom() := pid(),       % Error
}.

% Bad map key
```

Second, the map may contain several "default" associations, i.e., associations
that are not singleton types (such as atoms and tuples of atoms). An example is the
following:

```Erlang
-spec test(_) -> #{
    atom() => pid(),
    integer() => binary()       % Error
}.

% Bad map key
```

While it may be sound in some examples such as the one above, in the general case,
the default keys (here `atom()` and `integer()`) may overlap, leading to confusing
signal. As such, only one default association is allowed per map.


### dyn_remote_fun

This error indicates use of a dynamic remote function call. These aren't supported in
eqWAlizer because there is no way to support them safely.

Examples of dynamic remote functions:

```Erlang
fun M:F/A,
M:F(),
my_module:F/1,
my_module:foo/Arity
```

You can try rewriting to use a specific function identifier:

```Erlang
fun foo/1,
foo(Arg), % etc
```

### type_var_in_parameter_position

The following opaque type alias contains a type variable in parameter position:

```Erlang
-opaque bad_alias(T) -> fun((T) -> undefined).
```

Such opaques are not allowed because they break a common property assumed in
Erlang code: that if `T` is a subtype of `U` then `alias(T)` is a subtype of
`alias(U)`. For example, `sets:set(a)` is a subtype of `sets:set(a | b)`.
But `fun((a) -> undefined)` is **not** a subtype of `fun((a | b) -> undefined)`.
Put differently, aliases are usually covariant but function types are
contravariant in their parameters.

eqWAlizer is able to manipulate contravariant type aliases as long as they are
not opaque, but will throw this error if it detects a contravariant opaque.

This error is extremely rare in practice. If you get this error, the clearest
thing to do for code readers just not use an alias: write out the full type.
That way consumers of your API will see clearly that function types are involved.


### reference_to_invalid_type

This error indicates that a spec, type, record or alias references a type with
a bad definition. The way to fix such problems is to fix the definition of
the type you are referring to.

For example:

```Erlang
-type point() :: {_X, _Y}.   % Error: _X: Type variable is unbound.
-spec origin() -> point().   % Error: origin/0 references type with invalid definition: point/0.
    origin() -> {0, 0}.
```

Fixing the definition of `point/0` fixes both errors:

```Erlang
-type point(T) :: {X :: T, Y :: T}.
-spec origin() -> point(integer()).
    origin() -> {0, 0}.
```


### redundant_fixme

This error indicates that fixme was used above a line that does not have a type error.

```Erlang
-spec redundant() -> ok.
    % eqwalizer:fixme  -- this fixme is redundant and eqWAlizer will complain
    redundant() -> ok.
```

In such cases, you can delete the fixme.

Sometimes this error can happen when a fixme is misplaced: make sure the fixme
is on the line directly above the line with the type error:

```Erlang
-spec redundant() -> term().
redundant() ->
    % eqwalizer:fixme  -- this fixme is on the wrong line: move it one line down
    % another line
    2 + 'a' % type error here.
```


### redundant_nowarn_function

This error indicates that the `nowarn_function` pragma was used to removing
warnings about a function that does not have a type error.

```Erlang
-eqwalizer({nowarn_function, redundant/0}).  % This instruction is redundant

-spec redundant() -> ok.
    redundant() -> ok.
```

In such a case, the instruction nowarn_function can be deleted.


### ambiguous_union

This error indicates that an expression is used in a context involving generics,
but there are multiple possibilities to solve these generics, due to a union.

```Erlang
-spec generic_function(Config, {Config, R} | {ok, R}) -> {Config, R}.
% impl

-spec apply_generic({ok, atom()}) -> {ok, atom()}.
apply_generic(V) -> generic_function(ok, V).
```

When resolving the type of `generic_function/1` applied to `V`, the type
of `V`, `{ok, atom()}`, can match both `{Config, R}` and `{ok, R}`.
While is this case both seem equivalent, it is difficult in the general
case to know in advance whether an option is better than the other.
Hence, to keep signal clear and consistent, eqWAlizer chooses not to
attempt to solve generics in such a case.

A possible fix here is to use tagged unions to explicitly tell eqWAlizer
which branch to use, e.g.:

```Erlang
-spec generic_function(Config, {left, {Config, R}} | {right, {ok, R}}) -> {Config, R}.
% impl

-spec apply_generic({ok, atom()}) -> {ok, atom()}.
apply_generic(V) -> generic_function(ok, {right, V}).
```


### clause_not_covered

This error indicates that a clause is not properly covered by a spec, and will
thus be only partially checked by eqWAlizer.

```Erlang
-spec do_something({ok, atom()}) -> atom().
do_something({ok, A}) -> A;
do_something(err) -> err;
do_something(_) -> exit(badarg).
```

In the above example, the second clause matches when the argument is `err`,
even though the spec does not allow such an argument. Hence, the clause is
not covered.

To fix it, add the `err` case to the spec as follows:
```Erlang
-spec do_something({ok, atom()} | err) -> atom().
```
