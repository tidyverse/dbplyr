# Generate SQL for Insert, Update, Upsert, and Delete

These functions generate the SQL used in `rows_*(in_place = TRUE)`.

## Usage

``` r
sql_query_insert(
  con,
  table,
  from,
  insert_cols,
  by,
  ...,
  conflict = c("error", "ignore"),
  returning_cols = NULL,
  method = NULL
)

sql_query_append(con, table, from, insert_cols, ..., returning_cols = NULL)

sql_query_update_from(
  con,
  table,
  from,
  by,
  update_values,
  ...,
  returning_cols = NULL
)

sql_query_upsert(
  con,
  table,
  from,
  by,
  update_cols,
  ...,
  returning_cols = NULL,
  method = NULL
)

sql_query_delete(con, table, from, by, ..., returning_cols = NULL)
```

## Arguments

- con:

  A
  [sql_dialect](https://dbplyr.tidyverse.org/dev/reference/sql_dialect.md)
  object or database connection. Connections are supported for backward
  compatibility.

- table:

  Table to update. Must be a table identifier. Use a string to refer to
  tables in the current schema/catalog or
  [`I()`](https://rdrr.io/r/base/AsIs.html) to refer to tables in other
  schemas/catalogs.

- from:

  Table or query that contains the new data. Either a table identifier
  or SQL.

- insert_cols:

  Names of columns to insert.

- by:

  An unnamed character vector giving the key columns. The key columns
  must exist in both `x` and `y`. Keys typically uniquely identify each
  row, but this is only enforced for the key values of `y` when
  [`rows_update()`](https://dplyr.tidyverse.org/reference/rows.html),
  [`rows_patch()`](https://dplyr.tidyverse.org/reference/rows.html), or
  [`rows_upsert()`](https://dplyr.tidyverse.org/reference/rows.html) are
  used.

  By default, we use the first column in `y`, since the first column is
  a reasonable place to put an identifier variable.

- ...:

  Other parameters passed onto methods.

- conflict:

  For
  [`rows_insert()`](https://dplyr.tidyverse.org/reference/rows.html),
  how should keys in `y` that conflict with keys in `x` be handled? A
  conflict arises if there is a key in `y` that already exists in `x`.

  One of:

  - `"error"`, the default, will error if there are any keys in `y` that
    conflict with keys in `x`.

  - `"ignore"` will ignore rows in `y` with keys that conflict with keys
    in `x`.

- returning_cols:

  Optional. Names of columns to return.

- method:

  Optional. The method to use.

- update_values:

  A named SQL vector that specify how to update the columns.

- update_cols:

  Names of columns to update.

## Value

A SQL query.

## Details

Insert Methods

### `"where_not_exists"`

The default for most databases.

    INSERT INTO x_name
    SELECT *
    FROM y
    WHERE NOT EXISTS <match on by columns>

### `"on_conflict"`

Supported by:

- Postgres

- SQLite

This method uses the `ON CONFLICT` clause and therefore requires a
unique index on the columns specified in `by`.

Upsert Methods

### `"merge"`

The upsert method according to the SQL standard. It uses the `MERGE`
statement

    MERGE INTO x_name
    USING y
      ON <match on by columns>
    WHEN MATCHED THEN
      UPDATE SET ...
    WHEN NOT MATCHED THEN
      INSERT ...

### `"on_conflict"`

Supported by:

- Postgres

- SQLite

This method uses the `ON CONFLICT` clause and therefore requires a
unique index on the columns specified in `by`.

### `"cte_update"`

Supported by:

- Postgres

- SQLite

- Oracle

The classical way to upsert in Postgres and SQLite before support for
`ON CONFLICT` was added. The update is done in a CTE clause and the
unmatched values are then inserted outside of the CTE.

## Examples

``` r
sql_query_upsert(
  con = simulate_postgres(),
  table = "airlines",
  from = "df",
  by = "carrier",
  update_cols = "name"
)
#> <SQL> INSERT INTO "airlines" ("carrier", "name")
#> SELECT "carrier", "name"
#> FROM "df" AS "...y"
#> WHERE true
#> ON CONFLICT ("carrier")
#> DO UPDATE
#> SET "name" = "excluded"."name"
```
