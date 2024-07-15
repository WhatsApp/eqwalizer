# eqWAlizer

A type-checker for Erlang.

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="./logo/eqWAlizer_final_Full_Logo_White_Text.png">
  <img alt="eqWAlizer logo" src="./logo/eqWAlizer_final_Full__Logo_Black_Text.png" width="100%">
</picture>

## Using it with rebar3 projects

1. Use OTP 26
2. Download the `elp` binary for your system from https://github.com/WhatsApp/erlang-language-platform/releases

    > On Mac you will probably get the following message when trying to run the executable the first time: "elp cannot be opened because the developer cannot be verified.".
    To solve this, go to Preferences > Security and Privacy and add an exception. Alternatively, you can build elp from source.

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
        "https://github.com/whatsapp/eqwalizer.git",
        {branch, "main"},
        "eqwalizer_support"}}
]}.

{project_plugins, [
  {eqwalizer_rebar3,
    {git_subdir,
        "https://github.com/whatsapp/eqwalizer.git",
        {branch, "main"},
        "eqwalizer_rebar3"}}
]}.
```


## Using it with non-rebar projects

1. Use OTP 26 (it will be detected automatically by eqWAlizer)
2. Download the `elp` binary for your system from https://github.com/WhatsApp/erlang-language-platform/releases
3. Write a `project.json` file describing your project, see below for the file structure.
Ensure `eqwalizer_support` is added as a dependency, and that its `ebin` folder is reachable and populated
with `.beam` files.
4. From the project directory, assuming your `.json` file is called `project.json` run:
  - `elp eqwalize <module> --project project.json` to type-check a single module
  - `elp eqwalize-all --project project.json` to type-check all `src` modules in the project

The `.json` file should be structured in this way:
```
{
  "apps": [app list],
  "deps": [app list],      // 3rd party dependencies (not type-checked), defaults to []
}
```
where an `app` is a map structured as such:
```
{
  "name": "app_name",
  "dir": "path/to/app",                         // Relative to project root
  "src_dirs": ["path/to/src", ...],             // Relative to app dir, defaults to ["src"]
  "extra_src_dirs": ["path/to/extra_src", ...], // Relative to app dir, defaults to []
  "ebin": "path/to/ebin",                       // Relative to app dir, defaults to "ebin"
  "include_dirs": ["include", ...],             // Relative to app dir, defaults to []
  "macros": ["MACRO", ...],                     // Defaults to []
}
```


## FAQ

Please refer to [the FAQ document](./FAQ.md) for answers to some common questions,
including:

- What's the difference between eqWAlizer and Dialyzer?
- Why not extend Dialyzer, rather than creating a new tool?
- What about Elixir?

## Where can I learn more about eqWAlizer, its technical principles, its relation to dialyzer and elixir support feasibility?

[here](https://www.beamrad.io/35).

## License

eqWAlizer is [Apache licensed](./LICENSE).
