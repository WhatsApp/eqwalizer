Our implementation of Generics: All differences from Pierce and Turner "Local Type Inference"

We implemented Local Type Inference for generics, for the reasons described in
[eqWAlizer's Approach to Type Inference](https://fb.workplace.com/groups/typederlang/permalink/319738999492935/),
starting with [Pierce and Turner 2000's "Local Type Inference" algorithm](https://www.cis.upenn.edu/~bcpierce/papers/lti-toplas.pdf) (abbreviated "P&T" below).

## First Iteration: Direct Translation of P&T Local Type Inference

Generics in eqWAlizer started as an direct implementation of
[P&T](https://www.cis.upenn.edu/~bcpierce/papers/lti-toplas.pdf). This was a conscious choice that
enabled us to start from a solid, maintainable foundation, to reason about what the P&T approach
does and does not do, and to make data-driven decisions about what the largest gaps were in terms of
expressivity.

The only differences at this stage were:
- Supporting more data types: P&T had only tuples and lambdas, but Erlang has more features. These changes
  were straightforward, as the logic has a lot in common with the logic of subtyping.
- Logic for better error messages.

After Phase 1, we extended P&T in several ways, described in the rest of this post

> Note: eqWAlizer also contains some innovations w.r.t. recursive type aliases with parameters, not covered by P&T.
We will cover that topic in a separate post.

## Extending the Approach: Expressive Lambdas

Our analysis indicated that type-checking idiomatic WA Erlang requires
advanced handling of lambdas (funs).

While we drew inspiration from the literature, novel solutions were required to support idiomatic WA Erlang.
The following Workplace post describes why we needed these novel solutions and what we did:
[eqWAlizer’s approach to generics applied to lambdas](https://fb.workplace.com/groups/typederlang/permalink/324789545654547/).


## Generics with parameters with Unions of Type Variables

These cases occurred in WASERVER, and supporting them enables good code to type check without requiring any changes to
how WA devs write Erlang.

This diff gives examples and explains our algorithm:

https://www.internalfb.com/diff/D29426958

We tweaked the algorithm slightly in:

https://www.internalfb.com/diff/D32880201

## Other changes: Maintainability and Expressivity

The work on expressive lambdas meant that we did not need P&T's C-Abs-Inf rule.
We dropped this rule in https://www.internalfb.com/diff/D27799001, which made subsequent features easier to implement
and test.

An invariant we introduced for inference for lambdas makes it easier for us to ensure changes to our
generics-handling are sound. Conceptually, we have two separate phases, where P&T have just one:
- Find a substitution of types for type parameters
- Use this substitution to type-check using the same logic used elsewhere in the checker for function
calls.

The separation of inference and checking, which emerged naturally from our work on lambdas, gives us more assurance
that our changes are safe and enables a simple mental model for what the type checker is doing.

Finally, in https://www.internalfb.com/diff/D31923815
we allow code to type-check even in obscure cases where we can't infer a "best" type. For example, this really-contrived
code would be rejected by P&T but is accepted by eqWAlizer:

```erl
-spec invar(T, T) -> fun((T) -> T).
invar(_T, U) ->
    fun(_X) -> U end.

-spec test_invar1() -> fun((a) -> a).
test_invar1() ->
    invar(a, a).
```
