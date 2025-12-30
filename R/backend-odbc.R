#' Backend: ODBC
#'
#' @description
#' See `vignette("translation-function")` and `vignette("translation-verb")` for
#' details of overall translation technology. Key differences for this backend
#' are minor translations for common data types.
#'
#' Use `simulate_odbc()` with `lazy_frame()` to see simulated SQL without
#' converting to live access database.
#'
#' @name backend-odbc
#' @aliases NULL
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' lf <- lazy_frame(a = TRUE, b = 1, d = 2, c = "z", con = simulate_odbc())
#' lf |> transmute(x = as.numeric(b))
#' lf |> transmute(x = as.integer(b))
#' lf |> transmute(x = as.character(b))
NULL

#' @export
#' @rdname backend-odbc
simulate_odbc <- function() simulate_dbi("OdbcConnection")

#' @export
#' @rdname backend-odbc
dialect_odbc <- function() {
  new_sql_dialect(
    "odbc",
    quote_identifier = function(x) sql_quote(x, '"')
  )
}

#' @export
sql_dialect.OdbcConnection <- function(con) {
  dialect_odbc()
}

#' @export
dbplyr_edition.OdbcConnection <- function(con) {
  2L
}

#' @export
sql_translation.sql_dialect_odbc <- function(con) {
  sql_variant(
    base_odbc_scalar,
    base_odbc_agg,
    base_odbc_win
  )
}

#' @export
#' @rdname sql_variant
#' @format NULL
base_odbc_scalar <- sql_translator(
  .parent = base_scalar,
  as.numeric = sql_cast("DOUBLE"),
  as.double = sql_cast("DOUBLE"),
  as.integer = sql_cast("INT"),
  as.character = sql_cast("STRING")
)

#' @export
#' @rdname sql_variant
#' @format NULL
base_odbc_agg <- sql_translator(
  .parent = base_agg,
  sd = sql_aggregate("STDDEV_SAMP", "sd")
)

#' @export
#' @rdname sql_variant
#' @format NULL
base_odbc_win <- sql_translator(
  .parent = base_win,
  sd = win_aggregate("STDDEV_SAMP"),
)

# nocov start
#' @export
db_connection_describe.OdbcConnection <- function(con, ...) {
  info <- DBI::dbGetInfo(con)

  host <- if (info$servername == "") "localhost" else info$servername
  port <- if (info$port == "") "" else paste0(":", info$port)

  paste0(
    info$dbms.name,
    " ",
    info$db.version,
    "[",
    info$username,
    "@",
    host,
    port,
    "/",
    info$dbname,
    "]"
  )
}
# nocov end
