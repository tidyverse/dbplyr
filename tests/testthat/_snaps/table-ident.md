# is properly vectorised

    Code
      new_table_ident(table = c("A", "B", "c"), schema = c("schema1", "schema2"))
    Condition
      Error:
      ! Can't recycle `table` (size 3) to match `schema` (size 2).

# can't supply table and sql

    Code
      new_table_ident(schema = "my schema", table = "my table", quoted = TRUE)
    Condition
      Error in `purrr::pmap()`:
      i In index: 1.
      Caused by error:
      ! Can't supply a schema when `table` is quoted.

# must supply table and schema when catalog is used

    Code
      new_table_ident(table = "my table", catalog = "cat")
    Condition
      Error in `purrr::pmap()`:
      i In index: 1.
      Caused by error:
      ! Must supply `schema` when `catalog` is supplied.
    Code
      new_table_ident(schema = "schema", catalog = "cat")
    Condition
      Error in `purrr::pmap()`:
      i In index: 1.
      Caused by error:
      ! Must supply `table` when `schema` is supplied.

---

    Code
      new_table_ident(table = "my table", schema = c("my schema", NA), catalog = "cat")
    Condition
      Error in `purrr::pmap()`:
      i In index: 2.
      Caused by error:
      ! Must supply `schema` when `catalog` is supplied.

# can't coerce or cast to character

    Code
      c(table, "character")
    Condition
      Error in `vec_c()`:
      ! Can't combine `..1` <dbplyr_table_ident> and `..2` <character>.
    Code
      as.character(table)
    Condition
      Error in `as.character()`:
      ! Can't convert `x` <dbplyr_table_ident> to <character>.

# can print

    Code
      new_table_ident(table = "table")
    Output
      <dbplyr_table_ident[1]>
      [1] table
    Code
      new_table_ident(schema = "schema", table = "table")
    Output
      <dbplyr_table_ident[1]>
      [1] schema.table
    Code
      new_table_ident(catalog = "catalog", schema = "schema", table = "table")
    Output
      <dbplyr_table_ident[1]>
      [1] catalog.schema.table
    Code
      new_table_ident(table = "`my schema`.`my table`", quoted = TRUE)
    Output
      <dbplyr_table_ident[1]>
      [1] `my schema`.`my table`

---

    Code
      new_table_ident(table = c("`my schema`.`my table`", "table1", "table2",
        "table3"), schema = c(NA, NA, "schema2", "schema3"), catalog = c(NA, NA, NA,
        "catalog3"), quoted = c(TRUE, FALSE, FALSE, FALSE))
    Output
      <dbplyr_table_ident[4]>
      [1] `my schema`.`my table`  table1                  schema2.table2         
      [4] catalog3.schema3.table3

