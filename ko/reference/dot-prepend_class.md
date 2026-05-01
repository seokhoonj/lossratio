# Prepend class(es) without duplication

Ensure one or more class names appear at the front of an object's class
vector, without duplicating existing classes. For `data.table` input,
uses
[`data.table::setattr()`](https://rdrr.io/pkg/data.table/man/setattr.html)
to avoid copy and preserve selfref.

## Usage

``` r
.prepend_class(x, classes)
```

## Arguments

- x:

  An R object.

- classes:

  Character vector of class names to prepend.

## Value

`x` with class attribute updated.
