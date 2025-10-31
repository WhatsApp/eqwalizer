# Shortcomings and limitations of eqWAlizer

### Summary

- [Custom validation/predicates](#custom-validationpredicates)
- [Filtering](#filtering)
- [Disjunction of many cases/clauses](#disjunction-of-many-casesclauses)
- [Distributivity of unions](#distributivity-of-unions)
- [Complex functions (lists, maps)](#complex-functions-lists-maps)

### Custom validation/predicates

Erlang supports a very specific list of predicates as guards, such as `is_atom/1`, `is_record/2`, etc. For these, eqWAlizer is capable of powerful reasoning about types.
However, custom validation functions, or even custom wrapper functions around these predicates are not currently supported.

For example, the following will not typecheck:
```erlang
-spec is_foo(term()) -> boolean().
is_foo(#foo{}) -> true;
is_foo(_) -> false.

-spec filter_foo([term()]) -> [#foo{}].
filter_foo(L) -> [X || X <- L, is_foo(X)].
```
If possible, use a predicate directly:
```erlang
-spec filter_foo([term()]) -> [#foo{}].
filter_foo(L) -> [X || X <- L, is_record(X, foo)].
```
Or wrap it in a macro for better understanding:
```erlang
-spec filter_foo([term()]) -> [#foo{}].
filter_foo(L) -> [X || X <- L, ?IS_FOO(X)].
```
In case the predicate is too complex and cannot be simply expressed in terms of basic Erlang predicates, then one has to resort to escape mechanisms (unsafe type ascriptions, dynamic types, or `eqwalizer:ignore`s). For example:
```erlang
-include_lib("eqwalizer/include/eqwalizer.hrl").

-spec filter_complex([term()]) -> [complex_type()].
filter_complex(L) ->
    [?UNCHECKED_CAST(X, complex_type()) || X <- L, is_complex_type(X)].
```

### Filtering

In the same vein as custom validation and custom predicates, filtering operations are only supported on an ad-hoc basis in eqWAlizer.

For example, the following will not be accepted:
```erlang
-spec filter_undefined([foo() | undefined]) -> [foo()].
filter_undefined(L) ->
    lists:filter(fun(V) -> V =/= undefined end, L).
```
In this case, support for lists comprehensions is slightly more powerful than `lists:filter/2` in eqWAlizer, so this function can be rewritten into the following accepted one:
```erlang
-spec filter_undefined([foo() | undefined]) -> [foo()].
filter_undefined(L) ->
    [V || V <- L, V =/= undefined].
```
However, for more general cases and until gaps are patched, one has to resort to error suppression mechanisms.

### Disjunction of many cases/clauses

When in the presence of multiple clauses, eqWAlizer is capable of powerful reasoning to detect which cases have been handled by the previous clauses, and which cases remain. This is called [occurrence typing](../reference/narrowing.md#occurrence-typing). For example, the following is accepted because eqWAlizer is capable to deduce that `undefined` has been properly filtered in all cases:
```erlang
-spec occ(
    integer() | undefined,
    integer() | undefined
) -> {integer(), integer()}.
occ(A1, A2) ->
  case {A1, A2} of
    {undefined, undefined} ->
      {0, 0};
    {undefined, _} ->
      {0, A2};
    {_, undefined} ->
      {A1, 0};
    _ ->
      {A1, A2}
  end.
```

However, since this problem is exponential in the general case, occurrence typing is disabled in each of the following situations:
1. If there are more than 5 clauses;
2. If there are too many intricate guards, leading to a blowup of the number of cases;
3. If an already-bound variable is used in a pattern.

For the first two cases, the solution is to allow eqWAlizer to perform arbitrarily expensive computations, by adding the following:
```erlang
-eqwalizer({unlimited_refinement, parent_function/5}).
```

For the last case, the best solution is to not use a bound variable, and instead add an equality check. For example:
```erlang
f(X, Y) ->
    case X of
        {a, Y} -> ok;
        _ -> error
    end.
```
should rather be written as:
```erlang
f(X, Y) ->
    case X of
        {a, Y2} when Y2 =:= Y -> ok;
        _ -> error
    end.
```

### Distributivity of unions

When one or several members of a tuple are unions, it is, in theory, possible to distribute them. For example, `{a | b, foo()}` can be distributed to the equivalent type `{a, foo()} | {b, foo()}`.

However, in the general case, this distribution is exponential and breaks eqWAlizer's performance. Hence, it is done on a best-effort basis and only in specific cases. This can lead to confusing signal, for example:
```erlang
-spec test_impl([foo()], [boolean()]) -> {[foo()], [foo()]}.
test_impl(Foos, Bars) ->
    wa_lists:partitionmap(fun({Foo, Bar}) -> {Bar =:= true, Foo} end, lists:zip(Foos, Bars)).

Expression has type:   fun(({foo(), bar()}) -> {boolean(), foo()})
Context expected type: fun(({foo(), bar()}) -> {'true', L} | {'false', R})
```

Because of generic types `L` and `R`, eqWAlizer cannot do the distribution properly. There are several ways to overcome this. One is to use a type ascription to force eqWAlizer to distribute the union:
```erlang
-include_lib("eqwalizer/include/eqwalizer.hrl").

-spec test_impl([foo()], [bar()]) -> {[foo()], [foo()]}.
test_impl(Foos, Bars) ->
    wa_lists:partitionmap(fun({Foo, Bar}) ->
        ?CHECKED_CAST({Bar =:= true, Foo}, {true, foo()} | {false, foo()})
    end, lists:zip(Foos, Bars)).
```

A second solution is to make the union explicit using multiple clauses:
```erlang
-spec test_impl([foo()], [bar()]) -> {[foo()], [foo()]}.
test_impl(Foos, Bars) ->
    wa_lists:partitionmap(fun
        ({Foo, true}) -> {true, Foo};
        ({Foo, _}) -> {false, Foo}
    end, lists:zip(Foos, Bars)).
```

### Complex functions (lists, maps)

OTP functions with complex logic are supported on a case-by-case and best-effort basis in eqWAlizer. This includes most of the functions of the `lists` and `maps` modules, such as `lists:filtermap` and `maps:fold`, for example.

While we continuously improve support for these functions, there are always some gaps. Moreover, very large types (such as large map types) can be prohibitively expensive to support properly, hence we have to resort to some approximations. In many cases, the type of a map will be "flattened", e.g., `#{a => foo(), b => bar()}` will be approximated to `#{a | b => foo() | bar()}`, possibly leading to false positives.
