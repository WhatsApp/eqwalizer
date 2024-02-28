# eqWAlizer stats

This page references stats that eqWAlizer can report using command `elp eqwalize_stats`.

### eqwalizer_fixme

This indicates the suppression of an error message using a `%eqwalizer:fixme` escape hatch.
This is discouraged and should be avoided at all costs.

### eqwalizer_ignore

This indicates the suppression of an error message using a `%eqwalizer:ignore` escape hatch.
This is discouraged and should be avoided at all costs.

### eqwalizer_nowarn

This indicates the suppression of all eqWAlizer errors associated to a function,
using a `-eqwalizer({nowarn_function, foo/n})` pragma.
eqWAlizer will not run on such a function, thus the use of this pragma is highly
discouraged.
