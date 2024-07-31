# Advanced features

This page describes some of the more advanced features of eqWAlizer.


## Overloaded specs

EqWAlizer supports overloaded function specs, as defined in the
[Erlang Reference Manual](https://www.erlang.org/doc/reference_manual/typespec.html#specifications-for-functions).
However, eqWAlizer was designed with a very precise use of overloaded
specs in mind, and some good practices must be followed to avoid the
pitfalls that come with eqWAlizer's handling of overloaded specs.

### When and when not to use overloaded specs

Historically, the introduction of overloaded specs in eqWAlizer came from the
implementation of behavior modules from OTP (e.g., `gen_server`), and support
for overloaded specs is very tailored to this particular use case.
While eqWAlizer supports other uses of overloaded specs, this is mostly on
a "best-effort" basis, and may lead to noise or confusing signal.

Proper implementation of callbacks from behavior modules often follows strict
guidelines. For example, the implementation of `handle_call/3` needed by
`gen_server` for a process that tracks some jobs in progress may look like:
```erlang
-spec handle_call
    ({register_job, job_id(), job_data()}, term(), #state{}) -> {reply, ok | too_many_jobs, #state{}};
    ({cancel_job, job_id()}, term(), #state{}) -> {reply, ok, #state{}};
    (get_job_count, term(), #state{}) -> {reply, non_neg_integer(), #state{}}.
handle_call({register_job, JobId, JobData}, {Pid, Tag}, #state{jobs = Jobs} = State) ->
    MaxJobs = get_max_job_count(),
    case maps:size(Jobs) of
        JobsSize when JobsSize >= MaxJobs -> {reply, too_many_jobs, State};
        _ -> {reply, ok, State#state{jobs = maps:put(JobId, JobData, Jobs)}}
    end;
handle_call({cancel_job, JobId}, {Pid, Tag}, #state{jobs = Jobs} = State) ->
    {reply, ok, State#state{jobs = maps:remove(JobId, Jobs)}};
handle_call(get_job_count, {Pid, Tag}, #state{jobs = Jobs} = State) ->
    {reply, maps:size(Jobs), State}.
```
There are three important things to note here, that constitute the good practices
for overloaded specs in eqWAlizer:

- sub-specs are needed here because there is a clear relation between the callback
argument and its result;
- all sub-specs are easily differentiated by an atom (`register_job`, `cancel_job`, `get_job_count`);
- each clause matches exactly one sub-spec in a non-ambiguous way.

**Support for uses of overloaded specs that do not follow these guidelines is limited
and may lead to confusing signal.**

### Checking of overloaded specs

In a nutshell, when type-checking overloaded specs, eqWAlizer checks that every
clause of the function satisfies every compatible clause of the spec (a design choice
guided by the previously explained use of overloaded specs). Here,
"compatible" means that the types given to the parameters in the spec are
compatible with the parameters given in the clause.

In the above `handle_call/3` example, eqWAlizer detects that in the first clause,
the first argument must be a tuple whose first component is `register_job`. Hence,
only the first sub-spec matches, and eqWAlizer will attempt to verify that this
clause satisfies the first sub-spec, that is, that it returns some value of type
`{reply, ok | too_many_jobs, #state{}}`. Similarly, the second sub-spec will be
matched against the second clause, and the third sub-spec against the third clause.

Similarly, when type-checking an overloaded function application, eqWAlizer
uses the type information available to deduce which sub-spec to use to
deduce the type of the application. For example, if calling `handle_call/3`
above with first argument `get_job_count`, eqWAlizer will deduce that the
result has type `{reply, non_neg_integer(), #state{}}`.

### Caveat #1: branching information

Since eqWAlizer uses the available type information to deduce which sub-spec
to consider when type-checking an overloaded function application, one must
make sure that there is sufficient information to avoid any ambiguity.

Consider for example the following overloaded function, which is simply the
logical negation on booleans but with a very precise overloaded spec:
```erlang
-spec negb
    (true) -> false;
    (false) -> true.
negb(true) -> false;
negb(false) -> true.
```
While this spec is extremely precise, it cannot be truly exploited by
eqWAlizer in most settings. For example, consider the following code:
```erlang
-spec apply_neg(boolean()) -> boolean().
apply_neg(B) -> negb(B).
```
Since `B` has type `boolean()`, eqWAlizer cannot decide which sub-spec of
`negb` to use to decide the type of `negb(B)`. eqWAlizer will simply assume
type `eqwalizer:dynamic()` for the result of `negb(B)`, disregarding the spec.

Hence, to get better signal in this case, `negb` should simply be
specced as `(boolean()) -> boolean()`.

### Caveat #2: overlapping sub-specs

The second caveat consists in having multiple sub-specs that are compatible
with one clause of the function. Such a pattern leads to sometimes
counter-intuitive behaviour and must be avoided.

**Keep in mind that type-checking of overloaded specs does not depend on the
order of sub-specs and clauses.**

For example:
```erlang
-spec convert
  (atom()) -> binary();
  (binary()) -> atom().
convert(A) when is_atom(A) ->
  atom_to_binary(A);
convert(B) ->
  binary_to_atom(B).
```
Here, only the first sub-spec matches the first clause of the function,
thanks to the guard `is_atom(A)`. However, the second clause is unrestricted,
and is thus compatible with both sub-specs. This means that eqWAlizer will
attempt to check both sub-specs against the second clause.

In this particular instance, the second branch does not satisfy the sub-spec
`(atom()) -> binary()` since the call to `binary_to_atom(B)` is ill-typed if
`B` is an atom, and eqWAlizer will report a type error.

To avoid this confusing signal, one should add a guard to the second clause
to make sure that the order of clauses and specs is irrelevant:
```erlang
-spec convert
  (atom()) -> binary();
  (binary()) -> atom().
convert(A) when is_atom(A) ->
  atom_to_binary(A);
convert(B) when is_binary(B) ->
  binary_to_atom(B).
```


## Experimental and expert features

Some features of eqWAlizer are disabled by default as they are experimental
or not meant for casual use. They can be enabled by setting the corresponding
environment variable when calling `elp eqwalize`, e.g.,
`EQWALIZER_CLAUSE_COVERAGE=true elp eqwalize-all`.

### Occurrence typing

Disable occurrence typing with `EQWALIZER_EQWATER=false`. By default, eqWAlizer
performs occurrence typing except in some instances (for example if a function
contains too many clauses). See [occurrence typing](./narrowing.md#occurrence-typing).

### Error tolerance

Disable error tolerance with `EQWALIZER_TOLERATE_ERRORS=false`. By default, eqWAlizer
attempts to recover from type errors, to provide as much signal as possible for
a given function or module.

### Spec coverage of function clauses

Disable checks of proper coverage of function clauses by specs using `EQWALIZER_CLAUSE_COVERAGE=false`.
By default, eqWAlizer will check coverage of function clauses by the corresponding spec,
ensuring all clauses are properly checked.
