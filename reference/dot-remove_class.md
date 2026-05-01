# Remove class(es) from an object

Remove one or more class names from an object's class vector. For
`data.table` input, uses
[`data.table::setattr()`](https://rdrr.io/pkg/data.table/man/setattr.html)
to avoid copy.

## Usage

``` r
.remove_class(x, classes)
```

## Arguments

- x:

  An R object.

- classes:

  Character vector of class names to remove.

## Value

`x` with class attribute updated.
