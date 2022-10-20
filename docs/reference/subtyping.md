# Subtyping in eqWAlizer

This page summarizes the main aspects of subtyping in eqWAlizer. In some contexts,
eqWAlizer may attempt to perform smarter subtyping than what is underlined here,
but this is only done on a "best effort" basis and should not be relied upon.


### Records and tuples

In Erlang, records are internally represented as tuples whose first field is the
record's name, and both representations are interchangeable. As such, subtyping
in eqWAlizer attempts to reflect this as much as possible.
In particular, given a record type `-record(foo, {a :: integer(), b :: binary()})`,
both of the following functions are accepted by eqWAlizer:
```erlang
-spec rec_to_tuple(#foo{}) -> {foo, integer(), binary()}.
rec_to_tuple(R) -> R.

-spec tuple_to_rec({foo, integer(), binary()}) -> #foo{}.
tuple_to_rec(R) -> R.
```
However, this feature should be avoided if possible, as it may lead to confusing
signal. It is also very dependent on the order in which record fields are defined,
which means that the above two function would not type-check if the type was instead
rewritten as `-record(foo, {b :: binary(), a :: integer()})`.

Subtyping in eqWAlizer compares the name of record types: two record types with the
same fields but different names will not be subtype of one another. However, eqWAlizer
supports *refined record fields*, as long as they are properly indicated, using
the special type `eqwalizer:refinable/1`.

Declaring for example the record type `-record(bar, {a :: eqwalizer:refinable(term())})`
defines a record type `bar` with a refinable field `a`, whose initial type is `term()`.
It is then possible to write the following specced function, refining the type `#bar{}`:
```erlang
-spec refine_bar_number(#bar{}) -> err | #bar{a :: number()}.
refine_bar_number(R = #bar{a = N}) when is_number(N) -> R;
refine_bar_number(_) -> err.
```
In this case, the refined record type `#bar{a :: number()}` is a subtype of `#bar{}`.


### Dynamic

In gradual mode, the dynamic type `eqwalizer:dynamic()` is, in essence, both a subtype
and a supertype of every other type. That is, a value of type `dynamic()` can be used
in any context, and a function whose return type is `dynamic()` can return anything.
This differs from the interpretation of `term()`, which is a supertype but not a
subtype of every other type. That is, given a function whose spec is
`-spec foo() -> term()`, the expression `atom_to_binary(foo())` would be ill-typed
since `foo()` has type `term()` which is not a subtype of `atom()`. Whereas if the
spec was `-spec foo() -> dynamic()`, this expression would be accepted.

However, in some instances, subtyping in eqWAlizer may handle `dynamic()` differently,
to give more precise signal and detect some possible errors. Consider for example the
following function, whose return type is `dynamic()`:
```erlang
-spec get_a(#{dynamic() => dynamic()}) -> dynamic().
get_a(#{a := V}) -> V;
get_a(_) -> err.
```
Since the return type is `dynamic()`, the result of this function can be used in any
context. In particular, given a map `M`, it would be possible to write
`binary_to_atom(get_a(M))` and eqWAlizer would accept this code.

However, if the above spec were refined as `-spec get_a(#{dynamic() => dynamic()}) -> dynamic() | err.`,
now it would not be possible to write `binary_to_atom(get_a(M))`, since it
specifies that `get_a(M)` *can* return `err`, which is not of type `binary()` and
cannot be passed to `binary_to_atom`. In this case, it is necessary to perform
some kind of check (e.g., pattern-matching) to handle the `err` case separately.


### Opaque types

Opaque types are converted into normal, non-opaque aliases in the module they are
defined in, and they behave as such for subtyping.

When used in another module, opaque types are simply compared by name. That is,
two opaque types `foo` and `bar` defined in other modules can never be subtype
of one another, even if they have the same definition.

When considering parameterized opaque types, eqWAlizer first compare the names, then
the parameters. For example, `foo(undefined)` is a subtype of `foo(atom())`
because both reference the same opaque type `foo`, and `undefined` is a subtype
of `atom()`.

This means that parameterized opaque types (and, more generally, type aliases)
**must** be *covariant* in eqWAlizer, that is, a type parameter cannot appear
on the left hand side of a function type in a type alias. For example, the type
`-type non_covariant(X) :: fun(X) -> integer().` will be rejected by eqWAlizer,
since it is not covariant in `X`.


### Shapes and dictionaries

In eqWAlizer, the fields of a shape must match the fields specified in its type, with some
exceptions regarding optional fields. For example, the following function
does not type-check, since the shape `M` has two fields `a` and `b`, and we expect
the function to return a shape with only a single field `a`:
```erlang
-spec forget_key(#{a := term(), b := number()}) -> #{a := term()}.
forget_key(M) -> M.
```
Note that the function still does not type check if `b` is made optional:
`#{a := term(), b => number()}`. The set of keys given in the return type must
*at least* contain the keys that may be present in `M`.

It is, however, possible to introduce new optional keys by subtyping, or for
mandatory associations to become optional ones. For example, the following
function type-checks:
```erlang
-spec maybe_keys(#{a := term(), b := number()}) -> #{a := term(), b => number(), c => number()}.
maybe_keys(M) -> M.
```

Shapes are automatically upcast to dictionaries when needed, or when eqWAlizer
is unable to deduce precise enough information. For example, given a shape `M`
`#S{a := number()}`, if one writes `maps:put(A, 42, M)` where `A` is of type
`atom()` but the precise atom cannot be known, the result will be a dictionary
of type `#D{atom() => number()}`.

This also means that shapes can be used when dictionaries are expected. For
example, a shape of type `#S{a := integer(), b := float()}` can be used as
a dictionary of type `#D{atom() => number()}`.
