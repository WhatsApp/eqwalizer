# Eqwater occurrence typing

Next-generation (smart) occurrence typing for eqWAlizer.

It's based on eqWAter-4 [prototype](https://www.internalfb.com/code/eqwalizer/src/fbsource/fbcode/whatsapp/experimental/eqwater/).

Structure of examples:

- `eqwater.erl` - the basic examples (also used for keep code coverage) with "raw types" (without type aliases).
- `eqwater_aliases.erl` - smoke tests that type aliases are supported as well.
- `eqwater_unknown.erl` - smoke tests demonstrating the occurrence typing correctly handles
  patterns, type predicates, etc. which it doesn't understand yet + tests demonstrating the current limitations
  of eqwater occurrence typing (for sake of simplicity or performance).
