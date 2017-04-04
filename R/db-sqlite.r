#' @export
db_desc.SQLiteConnection <- function(x) {
  paste0("sqlite ", sqlite_version(), " [", x@dbname, "]")
}

sqlite_version <- function() {
  if (utils::packageVersion("RSQLite") > 1) {
    RSQLite::rsqliteVersion()[[2]]
  } else {
    DBI::dbGetInfo(RSQLite::SQLite())$clientVersion
  }
}

# SQL methods -------------------------------------------------------------

#' @export
sql_translate_env.SQLiteConnection <- function(con) {
  sql_variant(
    sql_translator(.parent = base_scalar,
      log     = function(x, base = exp(1)) {
        if (base != exp(1)) {
          build_sql("log(", x, ") / log(", base, ")")
        } else {
          build_sql("log(", x, ")")
        }
      }
    ),
    sql_translator(.parent = base_agg,
      sd = sql_prefix("stdev")
    ),
    base_no_win
  )
}

#' @export
sql_escape_ident.SQLiteConnection <- function(con, x) {
  sql_quote(x, "`")
}

#' @export
sql_escape_logical.SQLiteConnection <- function(con, x){
  y <- as.character(as.integer(x))
  y[is.na(x)] <- "NULL"
  y
}

#' @export
sql_subquery.SQLiteConnection <- function(con, from, name = unique_name(), ...) {
  if (is.ident(from)) {
    setNames(from, name)
  } else {
    if (is.null(name)) {
      build_sql("(", from, ")", con = con)
    } else {
      build_sql("(", from, ") AS ", ident(name), con = con)
    }
  }
}
