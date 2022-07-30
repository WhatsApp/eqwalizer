# eqwalizer_rebar3 plugin

This simple plugin provides an integration point for ELP and eqWAlizer
to extract the project model needed for type-checking.

Using it in `rebar.config`:

```
{project_plugins, [
    {eqwalizer_rebar3, {git_subdir, "https://github.com/whatsapp/eqwalizer.git", {branch, "main"}, "eqwalizer_rebar3"}}
]}.
```
