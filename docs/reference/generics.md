# Generics and type inference

EqWAlizer features generics, whose implementation mostly follows Pierce and Turner's
[Local Type Inference](https://www.cis.upenn.edu/~bcpierce/papers/lti-toplas.pdf),
but with support for more data types and better error messages.


### Generics in eqWAlizer

Generics are, in essence, functions whose specs feature type variables. A very
simple example is a function that takes any value and wraps it into a result:
```Erlang
-spec wrap(T) -> {ok, T}.
wrap(V) -> {ok, V}.
```
Here, the type variable `T` allows the return type of `wrap/1` to depend on the
type of its argument. For instance, applying `wrap/1` to a number returns
a value of type `{ok, number()}`. Without generics, one would have to write
one function per data type to be wrapped:
```Erlang
-spec wrap_number(number()) -> {ok, number()}.
-spec wrap_binary(binary()) -> {ok, binary()}.
...
```


### Type variables and `when` constraints

In type specs, it is possible to specify a constraint on a type variable using
a `when` expression, such as for example:
```Erlang
-spec foo(X) -> ok when X :: atom().
```
When dealing with such specs, eqWAlizer simply unfolds all `when` constraints,
replacing the corresponding type variables with the given type. This is, in
essence, equivalent to defining a type alias in a spec. The above spec is
therefore **strictly** equivalent to the following:
```Erlang
-spec foo(atom()) -> ok.
```
This may be useful to make specs clearer and easier to understand, by giving
explicit names to types without having to write proper type aliases. There are,
however, a few limitations to take into account.

#### Constraints do not allow for bounded polymorphism

Since eqWAlizer simply unfolds all constraints, this means in particular that
`when` constraints break genericity. Consider for example the following spec:
```Erlang
-spec bar(X) -> X when X :: integer() | boolean().
```
This spec is understood by eqWAlizer as the following:
```Erlang
-spec bar(integer() | boolean()) -> (integer() | boolean()).
```
Notice how we lose the correspondence between the input type and the output type.
This means that, for eqWAlizer, the type of e.g. `bar(3)` will be
`integer() | boolean()`, even though `3` is of type `integer()`. However, the
former spec may be understood as saying that `bar(3)` should have type `integer()`,
since the type of the result should be the same as the type of the argument
(as would be the case without the `when` constraint).

The workaround to this issue is simple: one should use an overloaded spec instead
of a type variable in this case, which also has the advantage of making the spec
easier to read. For example:
```Erlang
-spec bar
    (integer()) -> integer();
    (boolean()) -> boolean().
```
This spec makes it clear that `bar` applied to an integer returns an integer, and
`bar` applied to a boolean returns a boolean.

This design choice is motivated by experience, which shows that in a great majority
of cases, `when` constraints are simply used as type aliases. For the remaining few
use cases that may require bounded polymorphism, rewriting specs as mentioned above
proved to be a good solution.

#### Constraints can't be recursive

EqWAlizer does not allow a variable to appear recursively under a `when` constraint.
That is, a constraint such as the following is rejected by eqWAlizer:
```Erlang
-spec flatten(DeepList) -> List when
      DeepList :: [term() | DeepList],
      List :: [term()].
```
In this snippet taken from the OTP module `lists.erl`, the variable `DeepList` appears
recursively in its constraint. Such recursive constraints are very rare in practice,
hence we decided not to support them in eqWAlizer for the sake of simplicity.

In cases where such a constraint is truly needed, it is possible to introduce a
recursive type alias manually and obtain the same behaviour:
```Erlang
-type DeepList() :: [term() | DeepList()].

-spec flatten(DeepList()) -> List when List :: [term()].
```


### Generics, lambdas, and type inference

EqWAlizer also supports type inference for generics applied to lambdas.
The canonical example is the function `lists:foldl`, whose spec is
`-spec foldl(fun((T, Acc) -> Acc), Acc, [T]) -> Acc`. This is a particularly
tricky case since the type variable `Acc` appears in several positions.
When a lambda is passed as first argument to `foldl`, eqWAlizer performs type
inference to deduce a solution for the type variables `T` and `Acc`. However,
the result of this inference greatly depends on how the lambda is introduced.

#### Passing a lambda directly

The first and most common way of passing a lambda to `foldl` is to simply
write it explicitly as first argument, for example:
```Erlang
Reversed = lists:foldl(fun (X, Acc) -> [X | Acc] end, [], [1, 2, 3])
```
Here, eqWAlizer performs full type inference to deduce the type of the
lambda `fun (X, Acc) -> [X | Acc] end`. Given the type of the third
argument to `foldl` (which is `[number()]`), eqWAlizer is able to deduce
that the lambda has type `(number(), [number()]) -> [number()]`, and
that the result of `foldl` is `[number()]`.

Introducing an anonymous function directly as argument to a function in
this way allows eqWAlizer to use contextual information (here, the spec of
`foldl`) to deduce much more precise types for lambdas.

#### Passing a lambda via a variable

The second way is to first store the lambda in a variable, and pass this
variable to the function:
```Erlang
Lambda = fun (X, Acc) -> [X | Acc] end,
Reversed = lists:foldl(Lambda, [], [1, 2, 3])
```
In such a setting, eqWAlizer will not be able to use information such as the
spec of `foldl` to infer the type of `Lambda`, and will perform much more
optimistic type-checking. It will simply assume for `Lambda`
the type `(dynamic(), dynamic()) -> dynamic()`. Therefore, the type-checking
of `foldl` will be very optimistic, and eqWAlizer will deduce for `Reversed`
the type `dynamic()`, which is much less precise than `[number()]`.
