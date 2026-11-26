#' SAP HANA backend
#'
#' @description
#' This backend supports SAP HANA databases, typically accessed via
#' `HDBConnection` created by [DBI::dbConnect()]. Use `dialect_hana()` with
#' `lazy_frame()` to see simulated SQL without connecting to a live database.
#'
#' Key differences for this backend are:
#'
#' * Temporary tables get `#` prefix and use `LOCAL TEMPORARY COLUMN`.
#' * No table analysis performed in [copy_to()].
#' * `paste()` uses `||`
#' * Note that you can't create new boolean columns from logical expressions;
#'   you need to wrap with explicit `ifelse`: `ifelse(x > y, TRUE, FALSE)`.
#'
#' See `vignette("translation-function")` and `vignette("translation-verb")` for
#' details of overall translation technology.
#'
#' @name backend-hana
#' @aliases NULL
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = dialect_hana())
#' lf |> transmute(x = paste0(d, " times"))
NULL

#' @export
#' @rdname backend-hana
dialect_hana <- function() {
  new_sql_dialect(
    "hana",
    quote_identifier = function(x) sql_quote(x, '"')
  )
}

#' @export
#' @rdname backend-hana
simulate_hana <- function() simulate_dbi("HDB")

#' @export
sql_dialect.HDB <- function(con) {
  dialect_hana()
}

#' @export
dbplyr_edition.HDB <- function(con) {
  2L
}

#' @export
sql_translation.sql_dialect_hana <- function(con) {
  sql_variant(
    sql_translator(
      .parent = base_scalar,
      as.character = sql_cast("VARCHAR"),
      as.numeric = sql_cast("DOUBLE"),
      as.double = sql_cast("DOUBLE"),

      # string functions ------------------------------------------------
      paste = sql_paste_infix(" ", "||"),
      paste0 = sql_paste_infix("", "||"),
      str_c = sql_paste_infix("", "||"),

      # https://help.sap.com/viewer/7c78579ce9b14a669c1f3295b0d8ca16/Cloud/en-US/20e8341275191014a4cfdcd3c830fc98.html
      substr = sql_substr("SUBSTRING"),
      substring = sql_substr("SUBSTRING"),
      str_sub = sql_str_sub("SUBSTRING"),
    ),
    base_agg,
    base_win
  )
}

#' @export
sql_table_temporary.sql_dialect_hana <- function(con, table, temporary, ...) {
  # HANA temporary tables must start with `#` and be created with
  # `CREATE LOCAL TEMPORARY TABLE`
  # https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-sql-reference-guide/create-table-statement-data-definition
  list(
    table = add_temporary_prefix(con, table, temporary = temporary),
    temporary = temporary
  )
}

# nocov start
#' @export
dbplyr_write_table.HDB <- function(
  con,
  table,
  types,
  values,
  temporary = TRUE,
  ...,
  overwrite = FALSE
) {
  if (!temporary) {
    dbplyr_write_table.DBIConnection(
      con,
      table,
      types,
      values,
      temporary = FALSE,
      ...,
      overwrite = overwrite
    )
  } else {
    dbplyr_write_table_ddl(
      con,
      table,
      types,
      values,
      temporary = TRUE,
      overwrite = overwrite
    )
  }
}
# nocov end

#' @export
sql_table_create.sql_dialect_hana <- function(
  con,
  table,
  fields = NULL,
  as = NULL,
  temporary = TRUE,
  ...
) {
  if (!temporary) {
    return(sql_table_create.DBIConnection(
      con,
      table,
      fields = fields,
      as = as,
      temporary = FALSE
    ))
  }

  if (is.null(as)) {
    fields_sql <- sql_field_types(con, fields)
    sql_glue2(
      con,
      "CREATE LOCAL TEMPORARY COLUMN TABLE {.tbl table}
      ({fields_sql})"
    )
  } else {
    sql_glue2(
      con,
      "CREATE LOCAL TEMPORARY COLUMN TABLE {.tbl table} AS
      {as_table_source(as)}"
    )
  }
}

#' @export
sql_table_analyze.sql_dialect_hana <- function(con, table, ...) {
  # CREATE STATISTICS doesn't work for temporary tables, so
  # don't do anything at all
}

#' @export
sql_values_subquery.sql_dialect_hana <- function(con, df, types, lvl = 0, ...) {
  sql_values_subquery_union(con, df, types = types, lvl = lvl, from = "DUMMY")
}
