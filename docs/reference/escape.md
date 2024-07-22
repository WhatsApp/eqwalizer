# Escape hatches

To make eqWAlizer adoption practical, it features several mechanisms to make
the type checker less strict, or to bypass it entirely.


### Type `dynamic()`

The built-in type `dynamic()` as well as `eqwalizer:dynamic/1` (provided in app `eqwalizer_support`
for the open source release) are a good way to make the type-checker more lenient
while still providing some signal. See [gradual typing](./gradual.md).


### Ignoring errors for a function completely

Adding the directive `-eqwalizer({nowarn_function, foo/1})` removes all warnings
and errors issued by eqWAlizer when type-checking `foo/1`. Note that eqWAlizer
will still type-check the function, but will not report errors. This allows
eqWAlizer to emit a warning when this directive is redundant, that is, when
`foo/1` is well-typed.


### Comments with `eqwalizer:fixme` and `eqwalizer:ignore`

Adding a comment of the form `% eqwalizer:fixme <reason>` or
`% eqwalizer:ignore <reason>` above a line containing an error suppresses this
error. Idiomatically, `eqwalizer:fixme` should be used when a piece of code is
temporarily ill-typed and should be fixed later, or if the error is due to an
obvious gap in eqWAlizer. On the opposite, `eqwalizer:ignore` should be used
when a piece of code deliberately and knowingly goes against types and specs.
For example:
```Erlang
-spec foo() -> ok.
foo() ->
    % eqwalizer:ignore I know what I'm doing
    error.
```
As for the `nowarn_function` directive, eqWAlizer will emit a warning when
such a comment is redundant and the line below is well-typed.
