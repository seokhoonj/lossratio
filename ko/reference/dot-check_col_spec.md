# Check a data frame against a named list of expected column classes

Compares the actual class of each column in `df` against the expected
class defined in `col_spec`, prints a colored console summary grouped by
status (`match`, `mismatch`, `missing`, `extra`), and returns a tidy
data.frame invisibly.

Integer/numeric mismatches are flagged as `compatible` in the note
column.

## Usage

``` r
.check_col_spec(df, col_spec)
```

## Arguments

- df:

  A data.frame.

- col_spec:

  A named list where names are column names and values are expected
  class strings (length-1 character).

## Value

Invisibly returns a data.frame with columns `column`, `actual`,
`expected`, `status`, `note`, `sample`.
