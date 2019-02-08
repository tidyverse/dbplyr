#' @export
sql_select.SQLiteConnection<- function(con, select, from, where = NULL,
                                       group_by = NULL, having = NULL,
                                       order_by = NULL,
                                       limit = NULL,
                                       distinct = FALSE,
                                       sample = NULL,
                                       ...) {
  out <- vector("list", 8)
  names(out) <- c("select", "from", "where", "group_by", "having", "order_by",
                  "limit", "sample")

  out$select    <- sql_clause_select(select, con, distinct)
  out$from      <- sql_clause_from(from, con)
  out$where     <- sql_clause_where(where, con)
  out$group_by  <- sql_clause_group_by(group_by, con)
  out$having    <- sql_clause_having(having, con)
  out$order_by  <- sql_clause_order_by(order_by, con)
  out$limit     <- sql_clause_limit(limit, con)

  if (length(sample)) {
    if(sample$type == "n"){
      out$where <- build_sql(
        "WHERE _rowid_ IN (SELECT _rowid_ FROM (",
        escape(from, collapse = ", ", con = con) ,
        ") ORDER BY RANDOM() LIMIT ",
        sample$size,
        ")"
      )
    } else {
      stop("Only number of rows is supported. Try using sample_n() instead")
    }
  }
  escape(unname(compact(out)), collapse = "\n", parens = FALSE, con = con)
}

#' @export
sql_clause_sample.SQLiteConnection <- function(sample, con) {

}

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
