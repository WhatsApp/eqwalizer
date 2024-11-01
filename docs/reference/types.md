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


### Maps

Map types in eqWAlizer contain two parts: a set of associations whose keys are
statically-defined singleton types, and a default association. Both are optional:
a map type can contain one or the other, both, or none (in which case it will just be `#{}`).

The first part is composed of keys that are statically-defined atoms or nested tuples
of such atoms. For example, the atom `a`, and the tuple `{a, {b, c}}` are such keys.
Corresponding associations can be marked as optional `=>` or mandatory `:=`, for example,
`#{a => atom(), {a, {b, c}} := binary()}`.

The second part is the so-called "default association", which is necessarily marked as
optional, and can contain any key/value combination, e.g. `#{atom() => binary()}`.
For complexity reasons and to preserve good signal, only one such association is allowed
per map type. The default association will have the least precedence when understanding
a map type. For example, the type `#{a => integer(), atom() => binary()}` stands for
maps that optionally associate the atom `a` to an integer, and any other atom to a
binary.

Note that the Erlang syntax allows for some ambiguous map types to be defined.
For example, it is possible to define maps with mandatory associations such as
`#{atom() := number()}`, in which case it is not clear whether such a map
must contain a mapping *for every* atomic key, or *at least one* mapping with
an atomic key. Similarly, one can define maps with multiple overlapping default
associations `#{term() => binary(), atom() => atom()}`, in which case the precedence
of associations is unclear. To avoid such questions, eqWAlizer
interprets all such maps as the map type `#{dynamic() => dynamic()}`, issuing
an error in the process.


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
