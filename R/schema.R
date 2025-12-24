#' Refer to a table in another schema/catalog
#'
#' @description
#' `in_schema()` and `in_catalog()` can be used to refer to tables outside of
#' the current catalog/schema. However, we now recommend using `I()` as it's
#' typically less typing.
#'
#' @param catalog,schema,table Names of catalog, schema, and table.
#'   These will be automatically quoted; use [sql()] to pass a raw name
#'   that won't get quoted.
#' @export
#' @keywords internal
#' @examples
#' # Previously:
#' in_schema("my_schema", "my_table")
#' in_catalog("my_catalog", "my_schema", "my_table")
#' in_schema(sql("my_schema"), sql("my_table"))
#'
#' # Now
#' I("my_schema.my_table")
#' I("my_catalog.my_schema.my_table")
#' I("my_schema.my_table")
#'
#' # Example using schemas with SQLite
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'
#' # Add auxiliary schema
#' tmp <- tempfile()
#' DBI::dbExecute(con, paste0("ATTACH '", tmp, "' AS aux"))
#'
#' library(dplyr, warn.conflicts = FALSE)
#' copy_to(con, iris, "df", temporary = FALSE)
#' copy_to(con, mtcars, I("aux.df"), temporary = FALSE)
#'
#' con |> tbl("df")
#' con |> tbl(I("aux.df"))
in_schema <- function(schema, table) {
  structure(
    list(
      schema = as_ident_or_sql(schema),
      table = as_ident_or_sql(table)
    ),
    class = "dbplyr_schema"
  )
}

#' @rdname in_schema
#' @export
in_catalog <- function(catalog, schema, table) {
  structure(
    list(
      schema = as_ident_or_sql(schema),
      table = as_ident_or_sql(table),
      catalog = as_ident_or_sql(catalog)
    ),
    class = "dbplyr_catalog"
  )
}

#' @export
format.dbplyr_schema <- function(x, ...) {
  con <- simulate_dbi()
  paste0(escape(x$schema, con = con), ".", escape(x$table, con = con))
}
#' @export
print.dbplyr_schema <- function(x, ...) {
  cat_line("<SCHEMA> ", format(x))
}

#' @export
format.dbplyr_catalog <- function(x, ...) {
  con <- simulate_dbi()
  paste0(
    escape(x$catalog, con = con),
    ".",
    escape(x$schema, con = con),
    ".",
    escape(x$table, con = con)
  )
}
#' @export
print.dbplyr_catalog <- function(x, ...) {
  cat_line("<CATALOG> ", format(x))
}

is_schema <- function(x) inherits(x, "dbplyr_schema")

is_catalog <- function(x) inherits(x, "dbplyr_catalog")

# Old dbplyr approach -----------------------------------------------------

#' Declare a identifier as being pre-quoted.
#'
#' No longer needed; please use [sql()] instead.
#'
#' @keywords internal
#' @export
ident_q <- function(...) {
  x <- c_character(...)
  structure(x, class = c("ident_q", "ident", "character"))
}

#' @export
escape.ident_q <- function(x, parens = FALSE, collapse = ", ", con = NULL) {
  sql_vector(names_to_as(con, x, names2(x)), parens, collapse, con = con)
}
