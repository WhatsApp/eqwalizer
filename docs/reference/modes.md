# Gradual and strict typing modes

EqWAlizer features two different modes: gradual mode, and strict mode. They differ in how
unannotated functions are handled, and how the builtin constant `eqwalizer:dynamic()` behaves.
Strict mode offer stronger type-checking guarantees, while gradual mode favours ease-of-use
and better signal-to-noise.
Strict mode is kept sound, however, current development of eqWAlizer focuses on optimising for
gradual mode. The current distribution of eqWAlizer uses gradual mode by default.


### Type `eqwalizer:dynamic()`

`eqwalizer:dynamic()` (later abbreviated to `dynamic()`) is a special type which,
in gradual mode, "slips through the fingers" of the type-checker. It is similar to
[any in TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#any),
[dynamic in Hack](https://docs.hhvm.com/hack/built-in-types/dynamic),
[untyped in Sorbet](https://sorbet.org/docs/untyped).
Formally, in gradual mode, `dynamic()` is compatible with every type (and, conversely,
every type is compatible with `dynamic()`). This
means that a function that expects an argument of type `dynamic()` can be used with any
value, and a result of type `dynamic()` can be used anywhere.

While this may seem too lenient to be useful, the power of type `dynamic()` becomes clear when
it is used alongside other types or type constructors. For example, a value of type `{dynamic(), dynamic()}` is
not any value: it is necessarily a tuple containing two elements, although both can be of any type.
This value can in particular be passed to a function expecting, say, an argument of type
`{integer(), boolean()}`, but not to a function expecting an argument of type
`{integer(), boolean(), atom()}`.

In strict mode, eqWAlizer considers `dynamic()` to be equivalent to `term()`. This is a
completely different, stricter, behaviour. In strict mode, a function that accepts arguments
of type `dynamic()` is considered by eqWAlizer as accepting arguments of type `term()`. Hence,
this function can be applied to values of any type, which is basically the same behaviour as in
gradual mode. However, in strict mode, a value of type `dynamic()` **cannot** be used in every
context. It can only be used in contexts expecting a value of type `term()`, since
the only supertype of `dynamic()` in strict mode is `term()`. As an example, consider the
following function:
```Erlang
-spec dyn_to_int(eqwalizer:dynamic()) -> integer().
dyn_to_int(X) -> X.
```
This function is accepted by eqWAlizer in gradual mode: `X` is of type `eqwalizer:dynamic()`
which, in gradual mode, is compatible with every type, in particular `integer()`. However, in
strict mode, `X` is considered to be of type `term()` which is not compatible with `integer()`.

The type `eqwalizer:dynamic()` is provided in the module `eqwalizer.erl`, which should be
made available during analysis. This module is present in the open-source library
[eqwalizer_support](https://github.com/WhatsApp/eqwalizer/tree/main/eqwalizer_support).


### Using `eqwalizer:dynamic()` in gradual mode

The purpose of `dynamic()` is twofold:

- provide signal for unspecced functions;
- handle mixtures of specced and unspecced code gracefully without much noise.

While it is possible to manually introduce the constant `eqwalizer:dynamic()`, the dynamic
type is introduced automatically by eqWAlizer in gradual mode in certain cases:

- as parameter types and result types of unspecced functions, both when type-checking their
definitions and their invocations;
- as parameter types and result types of dynamic invocations of the form `M:F(Arg1, Arg2)`;
- when dealing with structures of unspecified type, and in certain expressions, detailed below.

In gradual mode, eqWAlizer introduces the dynamic type when using the following structures:

- `list()` is supposed to be of type `[dynamic()]`;
- `map()` is supposed to be of type `#{dynamic() => dynamic()}`;
- untyped record fields are given type `dynamic()`.

Similarly, eqWAlizer assumes type `dynamic()` for the variable `Dyn` in the following expressions,
to reduce noise.

In receive expressions:
```Erlang
receive
    Dyn -> ...
end
```

In catch expressions:
```Erlang
Dyn = catch foo(),
```

In the catch clause of try-catch expressions:
```Erlang
try foo()
catch
  exit:Dyn -> ...
end
```

#### Example 1: signal for unspecced functions

`dynamic()` is one of the simplest ways to provide signal for unspecced functions.
As explained above, in gradual mode, arguments are assumed to be of type `dynamic()`.
But type inference still applies, and it allows some actual type-errors to be reported:
```Erlang
foo(Arg) ->
    Arg + atom_to_binary(Arg).
          ^^^^^^^^^^^^^^^^^^^
Expected: number()
Got: binary()
```
Here, since `Arg` is supposed to be of type `dynamic()`, the call to `atom_to_binary/1`
type-checks as a value of type `dynamic()` can be passed to any function. However, the
result type of `atom_to_binary(Arg)` is still `binary()` (this is independent of the
type of its argument). Since the operator `+` expects two values of type `number()`,
eqWAlizer raises a type error.

This allows for **some** type errors to be detected. However, the absence of type-errors
inside an unspecced function does not mean that this function is well-typed:
```Erlang
-spec some_function(integer(), atom()) -> atom().

unspecced(Arg) -> some_function(Arg, Arg).
```
Since `Arg` is supposed to be of type `dynamic()`, it is accepted as both a first and a
second argument to `some_function`, and the function `unspecced` is accepted. However,
since there is no value that is both an integer and an atom, no call to `unspecced` will
truly be well-typed.

#### Example 2: less noise for invocations of unspecced functions

Consider an unspecced function `unspecced/1`. When invoking it and running eqWAlizer in
strict mode, it would simply report that it does not know the spec of `unspecced/1` and
give up. In gradual mode, eqWAlizer instead supposes that `unspecced/1` actually has the
spec `dynamic() -> dynamic()` and uses this to report some actual
type errors:
```Erlang
-spec call_unspecced() -> binary().
call_unspecced() ->
    [unspecced(1)].
    ^^^^^^^^^^^^^^
Expected: binary()
Got: [dynamic()]
```
In this example, since `unspecced/1` is supposed to be of type `dynamic() -> dynamic()`,
the call `unspecced(1)` is accepted and is inferred to return a value of type `dynamic()`.
Therefore, `[unspecced(1)]` is of type `[dynamic()]`. This is not compatible with the
spec of `call_unspecced/0`, which expects a result of type `binary()`, and eqWAlizer reports
this error.
