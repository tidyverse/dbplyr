# Table paths

dbplyr standardises all the ways of referring to a table (i.e. a single
string, a string wrapped in [`I()`](https://rdrr.io/r/base/AsIs.html), a
[`DBI::Id()`](https://dbi.r-dbi.org/reference/Id.html) and the results
of
[`in_schema()`](https://dbplyr.tidyverse.org/dev/reference/in_schema.md)
and
[`in_catalog()`](https://dbplyr.tidyverse.org/dev/reference/in_schema.md))
into a table "path" of the form `table`, `schema.table`, or
`catalog.schema.path`. A table path is always suitable for inlining into
a query, so user input is quoted unless it is wrapped in
[`I()`](https://rdrr.io/r/base/AsIs.html).

This is primarily for internal usage, but you may need to work with it
if you're implementing a backend, and you need to compute with the table
path, not just pass it on unchanged to some other dbplyr function.

- `is_table_path()` returns `TRUE` if the object is a `table_path`.

- `as_table_path()` coerces known table identifiers to a `table_path`.

- `check_table_path()` throws an error if the object is not a
  `table_path`.

- `table_path_name()` returns the last component of the table path (i.e.
  the name of the table).

- `table_path_components()` returns a list containing the components of
  each table path.

A `table_path` object can technically be a vector of table paths, but
you will never see this in table paths constructed from user inputs.

## Usage

``` r
is_table_path(x)

table_path_name(x, con)

table_path_components(x, con)

check_table_path(x, error_arg = caller_arg(x), error_call = caller_env())

as_table_path(x, con, error_arg = caller_arg(x), error_call = caller_env())
```
