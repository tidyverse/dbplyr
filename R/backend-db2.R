#' DB2 backend
#'
#' @description
#' This backend supports IBM DB2 databases, typically accessed via ODBC.
#' Use `dialect_db2()` with `lazy_frame()` to see simulated SQL without
#' connecting to a live database.
#'
#' Key differences for this backend are:
#'
#' * Uses `FETCH FIRST n ROWS ONLY` instead of `LIMIT n`
#' * Uses double quotes for identifier quoting
#'
#' See `vignette("translation-function")` and `vignette("translation-verb")` for
#' details of overall translation technology.
#'
#' @name backend-db2
#' @aliases NULL
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = dialect_db2())
#' lf |> head()
NULL

#' @export
#' @rdname backend-db2
dialect_db2 <- function() {
  new_sql_dialect(
    "db2",
    quote_identifier = function(x) sql_quote(x, '"')
  )
}

#' @export
#' @rdname backend-db2
simulate_db2 <- function() simulate_dbi("DB2Connection")

#' @export
sql_dialect.DB2Connection <- function(con) {
  dialect_db2()
}

#' @export
dbplyr_edition.DB2Connection <- function(con) {
  2L
}

#' @export
sql_query_select.sql_dialect_db2 <- function(
  con,
  select,
  from,
  where = NULL,
  group_by = NULL,
  having = NULL,
  window = NULL,
  order_by = NULL,
  limit = NULL,
  distinct = FALSE,
  ...,
  subquery = FALSE,
  lvl = 0
) {
  sql_select_clauses(
    select = sql_clause_select(select, distinct),
    from = sql_clause_from(from),
    where = sql_clause_where(where),
    group_by = sql_clause_group_by(group_by),
    having = sql_clause_having(having),
    window = sql_clause_window(window),
    order_by = sql_clause_order_by(order_by, subquery, limit),
    # DB2 uses FETCH FIRST instead of LIMIT
    limit = if (!is.null(limit)) {
      limit <- as.integer(limit)
      sql_glue2(con, "FETCH FIRST {limit} ROWS ONLY")
    },
    lvl = lvl
  )
}

#' @export
sql_translation.sql_dialect_db2 <- function(con) {
  sql_variant(
    base_scalar,
    base_agg,
    base_win
  )
}
