# Overriding specs and types from third-party libraries

When using third-party libraries or OTP modules, their type specs may be
imprecise, overly broad, or incompatible with eqWAlizer. Rather than using
escape hatches throughout your codebase, eqWAlizer supports centralized
override modules that replace specs and type aliases at type-checking time.

Two dedicated modules can be used for this purpose:

- **`eqwalizer_specs`**: overrides function specs from external modules.
- **`eqwalizer_types`**: overrides type aliases from external modules.

These modules act as a single source of truth for spec/type adjustments and
affect the entire project, so changes should be made with caution.


## Overriding specs with `eqwalizer_specs`

The `eqwalizer_specs` module lets you provide replacement specs for functions
from OTP or third-party libraries. eqWAlizer will use the overridden spec
instead of the original one when type-checking call sites.

### Syntax

Function names are written as quoted atoms of the form `'module:function'`.
Each overridden function must have:

1. A `-spec` attribute with the replacement spec.
2. A function clause whose body is `error(eqwalizer_specs)` (a dummy body that
   is never executed).

The function clause must have the same arity as the original, with throwaway
arguments (`_`).

```erlang
-module(eqwalizer_specs).
-compile([export_all, nowarn_export_all]).

-spec 'lists:keyfind'(Key, N, TupleList) -> Tuple | false when
    Key :: dynamic(),
    N :: pos_integer(),
    TupleList :: [Tuple],
    Tuple :: tuple().
'lists:keyfind'(_, _, _) -> error(eqwalizer_specs).
```

### Defining helper types

You can define helper types directly in the `eqwalizer_specs` module and
reference them in your overridden specs:

```erlang
-type crypto_cipher_aead() ::
    aes_128_gcm
    | aes_256_gcm
    | chacha20_poly1305.

-spec 'crypto:crypto_one_time_aead'(
    Cipher :: crypto_cipher_aead(),
    Key :: iodata(),
    IV :: iodata(),
    InText :: iodata(),
    AAD :: iodata(),
    EncFlg :: boolean()
) -> binary().
'crypto:crypto_one_time_aead'(_, _, _, _, _, _) -> error(eqwalizer_specs).
```


## Overriding type aliases with `eqwalizer_types`

The `eqwalizer_types` module lets you provide replacement definitions for
type aliases exported by OTP or third-party libraries. eqWAlizer will use
the overridden definition wherever the original type is referenced, with some caveats.

### Syntax

Type names are written as quoted atoms of the form `'module:type'`. Each
overridden type must be:

1. Declared with a `-type` attribute.
2. Listed in an `-export_type` attribute.

```erlang
-module(eqwalizer_types).

-export_type([
    'digraph:vertex'/0,
    'io:format'/0,
    'ssl:sslsocket'/0
]).

-type 'digraph:vertex'() :: dynamic().
-type 'io:format'() :: string() | binary().
-type 'ssl:sslsocket'() :: dynamic().
```


## Known limitations

### Cannot override unexported types

If the original type in the third-party module is not exported, adding an
override in `eqwalizer_types` and exporting it there will not help — eqWAlizer
will still report a "type exists but is not exported" error referring to the
original module. The override mechanism can only replace types that are already
exported by their defining module.

### Mutually recursive type overrides do not work

If two types in the same module reference each other:

```erlang
%% In foo.erl
-type a() :: {ok, b()}.
-type b() :: {error, a()}.
```

You cannot override both in `eqwalizer_types`, because the overrides would need
to cross-reference the originals, creating a cycle that eqWAlizer cannot
resolve:

```erlang
%% In eqwalizer_types.erl — this does NOT work
-type 'foo:a'() :: {ok, foo:b()}.
-type 'foo:b'() :: {error, foo:a()}.
```


## When to use overrides

- **Overly broad return types**: an OTP function returns `term()` but actually
  returns a well-known shape that your code depends on.
- **Missing or incorrect specs**: a library function has no spec, or has a spec
  that is incorrect for your usage.
- **Replacing `term()` with `dynamic()`**: to get partial
  type-checking signal instead of silently accepting everything.
