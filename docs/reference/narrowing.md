# Narrowing and occurrence typing

Narrowing and occurrence typing are two features of eqWAlizer whose goal is to
refine the type of expressions according to patterns, guards, and dynamic checks.
The two are very related, but each comes with its own set of limitations.


## Type narrowing

Type narrowing allows eqWAlizer to refine the type of an expression according
to a pattern, a guard, or a dynamic check. For example:
```Erlang
-spec to_atom(atom() | binary()) -> atom().
to_atom(X) when is_atom(X) -> X;
to_atom(X) when is_binary(X) -> binary_to_atom(X).
```
this function type-checks thanks to type narrowing, which allows eqWAlizer to
deduce from the guards that `X` has type `atom()` in the first clause, and
type `binary()` in the second.

EqWAlizer also performs narrowing using the structure of pattern matchings:
```Erlang
-spec get_result({ok, result()} | {err, term()} | undefined) -> result() | error.
get_result({ok, R}) -> R;
get_result({err, _}) -> error;
get_result(undefined) -> error.
```
Here, in the first clause, eqWAlizer is able to deduce from the pattern and the
spec that the only possible type for `R` is `result()`, since the argument must
be a tuple whose first component is `ok`.

#### Supported constructs

EqWAlizer is able to deduce type information from dynamic type checks (`is_atom/1`, `is_integer/1`,
`is_record/2`, ...) and equality checks (`==`, `=:=`, `=/=`) present in guards, case
statements, and if statements. Checks can also be joined with logical connectives such
as `andalso`, `orelse`, and `not`, and eqWAlizer will deduce information from these.

The following examples are all equivalent way to use narrowing to type the same
code in eqWAlizer.
- Overloaded function with guards:
    ```Erlang
    -spec to_atom(atom() | binary()) -> atom().
    to_atom(X) when is_atom(X) -> X;
    to_atom(X) when is_binary(X) -> binary_to_atom(X).
    ```
- Case statement:
    ```Erlang
    -spec to_atom(atom() | binary()) -> atom().
    to_atom(X) ->
        case X of
            A when is_atom(A) -> A;
            B when is_binary(B) -> binary_to_atom(B)
        end.
    ```
- If statement:
    ```Erlang
    -spec to_atom(atom() | binary()) -> atom().
    to_atom(X) ->
        if
            is_atom(X) -> X;
            is_binary(X) -> binary_to_atom(X)
        end.
    ```

#### Opaque types

EqWAlizer allows partial introspection of opaque types in patterns, however the type
of internal values will not be narrowed and will always be `term()`. For example,
if a module `m1` contains the following declaration:

```Erlang
-module(m1).
-export_type([res/0]).
-opaque res() :: {ok, atom()} | undefined.
```

then, in another module `m2`, the following code will type-check in eqWAlizer:

```Erlang
-module(m2).

-spec get_res(res()) -> atom().
get_res({ok, A}) when is_atom(A) -> A;
get_res(_) -> undefined.
```

However, the following will not, since `A` will be given type `term()` in the first
clause, which is not compatible with the expected return type `atom()`:

```Erlang
-module(m2).

-spec get_res(res()) -> atom().
get_res({ok, A}) -> A;
get_res(_) -> undefined.
```

The technical reason for this is that a value of type `res()` is included (by subtyping)
into type `term()`, and can therefore be pattern-matched in any way, without violating
opacity (if upcast to `term()`). Values having an opaque type therefore obey the same rules as values of
type `term()` for pattern-matching: eqWAlizer will perform very limited narrowing,
keeping the internals as `term()`.


## Occurrence typing

Type narrowing is sufficient to type most programs. However, there are some cases
where one might want eqWAlizer to deduce more information, which is where occurrence
typing comes into play.

In essence, occurrence typing boils down to type narrowing but with additional
negative information. For example:
```Erlang
-spec to_atom(atom() | binary()) -> atom().
to_atom(X) when is_atom(X) -> X;
to_atom(X) -> binary_to_atom(X).
```
This example would be rejected using only narrowing, since in the second clause,
the type of `X` is not refined and is supposed to be `atom() | binary()`.
However, occurrence typing allows eqWAlizer to deduce that, after the first
clause, `X` *cannot have* type `atom()`, and is therefore necessarily of
type `binary()`.

Occurrence typing supports roughly the same constructs as type narrowing, with
some caveats, detailed below.

#### Repetition of the same variable in a pattern

For technical reasons and to keep a consistent signal, occurrence typing is
automatically disabled for a clause whenever the same variable occurs twice in the same
pattern. Consider for example the following function which sets an entry in
a map but is optimised to not perform any change if it is passed a value
that is equal to the default value:
```Erlang
-spec set_value(#{value := binary()} | undefined, binary(), binary()) -> #{value := binary()}.
set_value(undefined, Value, _) -> #{value => Value};
set_value(Map, Default, Default) -> Map;
set_value(Map, Value, _) -> Map#{value := Value}.
```
As-is, this function requires occurrence typing to type-check, since otherwise
eqWAlizer is unable to deduce that `Map = undefined` has been covered by
the first clause and therefore that `Map` is indeed a map in the second and
third clauses.

Unfortunately, since the same variable `Default` appears twice in the second clause,
occurrence typing is disabled for this clause and this function cannot be type-checked. There
are two solutions to overcome this particular problem.

First, as is often the case with occurrence typing, one can add guards so that
this only requires narrowing instead of occurrence typing:
```Erlang
-spec set_value(#{value := binary()} | undefined, binary(), binary()) -> #{value := binary()}.
set_value(undefined, Value, _) -> #{value => Value};
set_value(Map, Default, Default) when is_map(Map) -> Map;
set_value(Map, Value, _) -> Map#{value := Value}.
```

If adding a guard is not possible for some reason, it is also possible to
rewrite the second clause to separate the variables and introduce a guard
instead:
```Erlang
-spec set_value(#{value := binary()} | undefined, binary(), binary()) -> #{value := binary()}.
set_value(undefined, Value, _) -> #{value => Value};
set_value(Map, Value, Default) when Value =:= Default -> Map;
set_value(Map, Value, _) -> Map#{value := Value}.
```

#### Number of clauses, unions, and guards

The second main limitation of occurrence typing comes from the complexity of
the code to be type-checked. There are several instances where types will not
be refined, for performance reasons.

1. In function clauses, if expressions, and case expressions, occurrence
typing will be disabled in eqWAlizer if the number of clauses exceeds seven.
2. Similarly, if the number of connectives in a guard (`;`, `,`, `andalso`, `orelse`, ...)
exceeds 32, occurrence typing will be disabled.
3. Type aliases used as enumerations (unions) of more than 16 cases will not
be refined by occurrence typing, as they are often not meant to be in practice.

Hence, when working with very large union types, or functions that have
many cases, one should try to rely as much as possible on narrowing by
adding more guards instead of relying on occurrence typing. Note that
narrowing supports arbitrary numbers of guards.

However, in most cases, occurrence typing is still fairly optimized and
can easily go over these limits. If one still wants to rely on
occurrence typing to type-check a function (say `my_function/2`) with many
clauses, large guards, or large union aliases, it is possible to add the pragma
`-eqwalizer({unlimited_refinement, my_function/2})`
at the top of the module defining this function to force occurrence typing
and refinements.
