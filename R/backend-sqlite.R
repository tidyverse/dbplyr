#' @export
db_desc.SQLiteConnection <- function(x) {
  paste0("sqlite ", sqlite_version(), " [", x@dbname, "]")
}

#' @export
db_explain.SQLiteConnection <- function(con, sql, ...) {
  exsql <- build_sql("EXPLAIN QUERY PLAN ", sql, con = con)
  expl <- dbGetQuery(con, exsql)
  out <- utils::capture.output(print(expl))

  paste(out, collapse = "\n")
}

sqlite_version <- function() {
  numeric_version(RSQLite::rsqliteVersion()[[2]])
}

# SQL methods -------------------------------------------------------------

#' @export
sql_translate_env.SQLiteConnection <- function(con) {
  sql_variant(
    sql_translator(.parent = base_scalar,
      as.numeric = sql_cast("REAL"),
      as.double = sql_cast("REAL"),
      log = function(x, base = exp(1)) {
        if (base != exp(1)) {
          sql_expr(log(!!x) / log(!!base))
        } else {
          sql_expr(log(!!x))
        }
      },
      paste = sql_paste_infix(" ", "||", function(x) sql_expr(cast(!!x %as% text))),
      paste0 = sql_paste_infix("", "||", function(x) sql_expr(cast(!!x %as% text))),
      # https://www.sqlite.org/lang_corefunc.html#maxoreunc
      pmin = sql_prefix("MIN"),
      pmax = sql_prefix("MAX"),

    ),
    sql_translator(.parent = base_agg,
      sd = sql_aggregate("STDEV", "sd")
    ),
    if (sqlite_version() >= "3.25") {
      sql_translator(.parent = base_win,
        sd = win_aggregate("STDEV")
      )
    } else {
      base_no_win
    }
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
