# Create a base lazy query

`lazy_base_query()` is a constructor for base lazy query objects. A base
lazy query represents the root of a lazy query tree, i.e. a database
table or query.

## Usage

``` r
lazy_base_query(x, vars, class = character(), ...)
```

## Arguments

- x:

  A data source, typically a table identifier created by
  [`in_schema()`](https://dbplyr.tidyverse.org/dev/reference/in_schema.md)
  or
  [`in_catalog()`](https://dbplyr.tidyverse.org/dev/reference/in_schema.md),
  or a literal
  [`sql()`](https://dbplyr.tidyverse.org/dev/reference/sql.md) string.

- vars:

  A character vector of column names.

- class:

  A character vector of additional subclasses to add. The resulting
  object will have class
  `c("lazy_base_{class}_query", "lazy_base_query", "lazy_query")`.

- ...:

  Additional arguments passed to
  [`lazy_query()`](https://dbplyr.tidyverse.org/dev/reference/sql_build.md).

## Value

A lazy query object.
