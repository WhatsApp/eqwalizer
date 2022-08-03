# Frequently Asked Questions

## What's the difference between eqWAlizer and Dialyzer?

TLDR: eqWAlizer is a type checker and Dialyzer is a static analyis tool.
You don't have to choose between the two tools.

### Approach to analysis

**Dialyzer reports only cases where it can prove** your code (or specs)
are incorrect. As such, it can miss issues, and gives you no guarantees.
However, when it reports an issue something is surely wrong.

**eqWAlizer reports cases where it cannot prove** your code is correct.
As such, it can report issues in code that would run
completely fine (due to dynamism it cannot understand). However,
it can give you guarantees about removing certain class of errors from your programs.

### Scope of analysis

Dialyzer performs global analysis across the whole codebase.
This is how it can uncover complex bugs that involve the interaction of
many unspecced functions across many modules. This capability is valuable,
but the cost is performance and that error messages
can be hard to understand or not easily actionable.

eqWAlizer performs local analysis for each function separately.
This means eqWAlizer is fast and reports issues close to where they are created.
However, it cannot understand more complex structures involving many functions.
To deliver the most value, it also requires every function to have a spec.
This also allows eqWAlizer to be adopted gradually,
module-by-module, or even function-by-function.

## Why not extend Dialyzer, rather than creating a new tool?

Because the differences mentioned in the previous question are so fundamental
to how the tool operates, we didn't see a practical way of combining the two
styles of analysis in the same tool.

## What about Elixir?

As it is today, eqWAlizer works only with Erlang code. However, eqWAlizer operates
on the Erlang AST, which the Elixir compiler is capable of producing. It should
be entirely possible to run eqWAlizer analysis on the Elixir-produced BEAM files.
We'd be happy to accept PRs in this area!
