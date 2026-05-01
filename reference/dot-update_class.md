# Update class attribute: remove then prepend

Convenience wrapper that first removes classes listed in `remove`, then
prepends classes listed in `prepend`.

## Usage

``` r
.update_class(x, remove = NULL, prepend = NULL)
```

## Arguments

- x:

  An R object.

- remove:

  Character vector of class names to remove. `NULL` to skip.

- prepend:

  Character vector of class names to prepend. `NULL` to skip.

## Value

`x` with class attribute updated.
