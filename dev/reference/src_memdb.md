# Deprecated

**\[deprecated\]**

- `src_memdb()` is deprecated; use
  [`memdb()`](https://dbplyr.tidyverse.org/dev/reference/memdb.md)
  instead.

- `tbl_memdb(df)` is deprecated; use `copy_to(memdb(), df)` instead.

## Usage

``` r
src_memdb()

tbl_memdb(df, name = deparse(substitute(df)))
```

## Arguments

- df:

  Data frame to copy.

- name:

  Name of table in database.
