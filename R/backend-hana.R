#' Backend: SAP HANA
#'
#' @description
#' See `vignette("translation-function")` and `vignette("translation-verb")` for
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
#' lf |> transmute(x = paste0(d, " times"))
NULL

#' @export
#' @rdname backend-hana
simulate_hana <- function() simulate_dbi("HDB")

dialect_hana <- function() {
  new_sql_dialect(
    "hana",
    quote_identifier = function(x) sql_quote(x, '"')
  )
}

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

# nocov start
#' @export
sql_table_temporary.sql_dialect_hana <- function(con, table, temporary, ...) {
  list(
    table = add_temporary_prefix(con, table, temporary = temporary),
    temporary = FALSE
  )
}
# nocov end

#' @export
sql_table_analyze.sql_dialect_hana <- function(con, table, ...) {
  # CREATE STATISTICS doesn't work for temporary tables, so
  # don't do anything at all
}

#' @export
sql_values_subquery.sql_dialect_hana <- function(con, df, types, lvl = 0, ...) {
  sql_values_subquery_union(con, df, types = types, lvl = lvl, from = "DUMMY")
}
