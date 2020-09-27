#' Backend: SAP HANA
#'
#' @description
#' See `vignette("translate-function")` and `vignette("translate-verb")` for
#' details of overall translation technology. Key differences for this backend
#' are:
#'
#' * Temporary tables get `#` prefix and use `LOCAL TEMPORARY COLUMN`.
#' * No table analysis performed in [copy_to()].
#' * `paste()` uses `||`
#' * Note that you can't create new boolean columns from logical expressions;
#'   you need to wrap with explicit `ifelse`: `ifelse(x > y, TRUE, FALSE)`.
#'
#' Use `simulate_hana()` with `lazy_frame()` to see simulated SQL without
#' converting to live access database.
#'
#' @name backend-hana
#' @aliases NULL
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = simulate_hana())
#' lf %>% transmute(x = paste0(z, " times"))
NULL

#' @export
#' @rdname backend-hana
simulate_hana <- function() simulate_dbi("HDB")

# setClass("HDB", contains = "OdbcConnection")

#' @export
sql_translate_env.HDB <- function(con) {
  sql_variant(
    sql_translator(.parent = base_scalar,
      as.character = sql_cast("SHORTTEXT"),
      as.numeric = sql_cast("DOUBLE"),
      as.double = sql_cast("DOUBLE"),

      # string functions ------------------------------------------------
      paste = sql_paste_infix(" ", "||", function(x) sql_expr(cast(!!x %as% text))),
      paste0 = sql_paste_infix("", "||", function(x) sql_expr(cast(!!x %as% text))),
      str_c = sql_paste_infix("", "||", function(x) sql_expr(cast(!!x %as% text))),

      substr = sql_substr("SUBSTRING"),
      substring = sql_substr("SUBSTRING"),
      str_sub = sql_str_sub("SUBSTRING"),
    ),
    base_agg,
    base_win
  )
}

#' @export
setMethod(sqlCreateTable, "HDB",
  function(con, table, fields, row.names = NA, temporary = FALSE, ...) {
    table <- dbQuoteIdentifier(con, table)

    if (is.data.frame(fields)) {
      fields <- sqlRownamesToColumn(fields, row.names)
      fields <- vapply(fields, function(x) dbDataType(con, x), character(1))
    }

    field_names <- dbQuoteIdentifier(con, names(fields))
    field_types <- unname(fields)
    fields <- paste0(field_names, " ", field_types)

    SQL(paste0(
      "CREATE ", if (temporary) "LOCAL TEMPORARY COLUMN ",
      "TABLE ", table, " (\n",
      "  ", paste(fields, collapse = ",\n  "),
      "\n)\n"
    ))
  }
)

#' @export
db_copy_to.HDB <- function(con, table, values,
                           overwrite = FALSE, types = NULL, temporary = TRUE,
                           unique_indexes = NULL, indexes = NULL,
                           analyze = TRUE, ..., in_transaction = TRUE) {
  NextMethod(
    table = mssql_table_rename(table, temporary),
    types = types,
    values = values
  )
}

#' @export
db_write_table.HDB <- function(con, table, types, values, temporary = TRUE, ...) {
  NextMethod(
    table = mssql_table_rename(table, temporary),
    types = types,
    values = values
  )
}

#' @export
db_save_query.HDB <- function(con, sql, name, temporary = TRUE, ...) {
  NextMethod(
    sql = sql,
    name = mssql_table_rename(name, temporary)
  )
}

#' @export
`sql_table_analyze.HDB` <- function(con, table, ...) {
  # CREATE STATISTICS doesn't work for temporary tables, so
  # don't do anything at all
}
