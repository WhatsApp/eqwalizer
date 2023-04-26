# Syntax of types and specs in eqWAlizer

Like Dialyzer, eqWAlizer consumes specs and type aliases written using the
[Erlang syntax of types and specs](https://www.erlang.org/doc/reference_manual/typespec.html).

Types written according to this spec are fully accepted by eqWAlizer.
However, since this documentation does not fully describe the semantics of
types and specs, some design choices had to be made regarding the
interpretation of types during the development of eqWAlizer.

These choices, summarized in this page, are mostly guided by practicality,
both from a usability and programming perspective. In short, eqWAlizer
tends to focus on clear, non-ambiguous, and intuitive signal. Rarely
used, ambiguous, or difficult to implement features are often not supported.


### Numeric types

EqWAlizer does not distinguish between the different numeric types.
All numeric types (`integer()`, `float()`, integer ranges, `byte()`, etc)
are all converted into type `number()` before type-checking.


### Maps: shapes and dictionaries

To make eqWAlizer more expressive, Erlang maps are separated into two
categories: shapes and dictionaries.

#### Shapes

Shapes are maps whose keys are all explicitly defined atoms. For example,
the type `-type my_map :: #{a => term(), b := number()}` is interpreted by eqWAlizer
as the type of shapes with an optional association `a => term()` and a
mandatory association `b := number()`. To emphasize the fact that it is
understood as a shape, eqWAlizer will report this type as
`#S{a => term(), b := number()}`.

EqWAlizer attempts to track the type of shapes as precisely as possible on a
best-effort basis, with several OTP functions having a custom implementation
in eqWAlizer to produce more accurate types. For example, given a shape `M` of
type `my_map` above, `maps:put(c, 42, M)` will return a shape of type
`#S{a => term(), b := number(), c := number()}`.


#### Dictionaries

A map whose keys are not all explicitly defined atoms is a dictionary.
If eqWAlizer is not able to precisely deduce the atoms associated to the
keys of a map, then it attempts to understand it as a dictionary.
A dictionary in eqWAlizer is a map containing exactly one optional association,
from a type to another type, such as `#D{atom() => number()}` for the type
of dictionaries mapping atoms to numbers. EqWAlizer differentiates dictionaries
from shapes by printing them using `#D` instead of `#S`.

Note that the Erlang syntax allows for some ambiguous map types to be defined.
For example, it is possible to define maps with mandatory associations such as
`#{atom() := number()}`, in which case it is not clear whether such a map
must contain a mapping *for every* atomic key, or *at least one* mapping with
an atomic key. Similarly, one can define heterogeneous maps such as
`#{a => binary(), atom() => atom()}`, in which case it is not clear what can
be stated about a map such as `#{a => ok}`. To avoid such questions, eqWAlizer
interprets all such maps as the dictionary `#D{term() => term()}` in strict mode,
or `#D{dynamic() => dynamic()}` in gradual mode.


### List types

Much like numeric types, the various kind of list types are only supported in a
limited form by eqWAlizer. Non-empty lists are subsumed to standard lists in
eqWAlizer. That is, `nonempty_list(integer())` is equivalent to `list(integer())`.

Similarly, improper lists are not supported and are simply considered as proper
lists: `maybe_improper_list(T1, T2)` is the same as `list(T1)` for eqWAlizer.
Moreover, trying to construct an improper list will, in most contexts, produce
a type error.

However, eqWAlizer still distinguishes the empty list from other lists. The type
`[]` will be understood by eqWAlizer as being the type of empty lists.
It is, in particular, a subtype of any list type `list(T)`.
