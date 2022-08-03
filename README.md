# eqWAlizer

A type-checker for Erlang.

## Using it with rebar3 projects

1. Use OTP 25
2. Download the `elp` binary for your system from https://github.com/WhatsApp/eqwalizer/releases
3. Add `eqwalizer_support` dependency and `eqwalizer_rebar3` plugin
   to your rebar3 project definition (see below)
4. From the project directory run:
  - `elp eqwalize <module>` to type-check a single module
  - `elp eqwalize-all` to type-check all `src` modules in the project


Adding `eqwalizer_support` and `eqwalizer_rebar3`:

```
{deps, [
  {eqwalizer_support,
    {git_subdir,
        "git@github.com:whatsapp/eqwalizer.git",
        {branch, "main"},
        "eqwalizer_support"}}
]}.

{project_plugins, [
  {eqwalizer_rebar3,
    {git_subdir,
        "git@github.com:whatsapp/eqwalizer.git",
        {branch, "main"},
        "eqwalizer_rebar3"}}
]}.
```

## FAQ

Please refer to [the FAQ document][./FAQ.md] for answers to some common questions,
including:

- What's the difference between eqWAlizer and Dialyzer?
- Why not extend Dialyzer, rather than creating a new tool?
- What about Elixir?

## License

eqWAlizer is [Apache licensed](./LICENSE).
