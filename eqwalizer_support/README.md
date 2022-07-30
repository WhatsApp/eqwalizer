# eqwalizer_support library

This library provides API and integrations points for eqWAlizer:
- support for using eqWAlizer in gradual mode (`eqwalizer:dynamic()` type)
- alternative (more type-checking-friendly) specs for some essential functions from OTP libraries

Minimal rebar3 config:

```
{deps, [
    {eqwalizer_support, {git_subdir, "https://github.com/whatsapp/eqwalizer.git", {branch, "main"}, "eqwalizer_support"}}
]}.
```
