#' More SQL generics
#'
#' These are new, so not included in dplyr for backward compatibility
#' purposes.
#'
#' @keywords internal
#' @export
sql_escape_logical <- function(con, x) {
  UseMethod("sql_escape_logical")
}


# DBIConnection methods -----------------------------------------------------------------

#' @export
sql_select.DBIConnection <- function(con, select, from, where = NULL,
                               group_by = NULL, having = NULL,
                               order_by = NULL,
                               limit = NULL,
                               distinct = FALSE,
                               ...) {
  out <- vector("list", 7)
  names(out) <- c("select", "from", "where", "group_by", "having", "order_by",
    "limit")

  out$select    <- sql_clause_select(select, con, distinct)
  out$from      <- sql_clause_from(from, con)
  out$where     <- sql_clause_where(where, con)
  out$group_by  <- sql_clause_group_by(group_by, con)
  out$having    <- sql_clause_having(having, con)
  out$order_by  <- sql_clause_order_by(order_by, con)
  out$limit     <- sql_clause_limit(limit, con)

  escape(unname(compact(out)), collapse = "\n", parens = FALSE, con = con)
}

#' @export
sql_subquery.DBIConnection <- function(con, from, name = unique_name(), ...) {
  if (is.ident(from)) {
    setNames(from, name)
  } else {
    build_sql("(", from, ") ", ident(name %||% random_table_name()), con = con)
  }
}

#' @export
sql_join.DBIConnection <- function(con, x, y, vars, type = "inner", by = NULL, ...) {
  JOIN <- switch(
    type,
    left = sql("LEFT JOIN"),
    inner = sql("INNER JOIN"),
    right = sql("RIGHT JOIN"),
    full = sql("FULL JOIN"),
    cross = sql("CROSS JOIN"),
    stop("Unknown join type:", type, call. = FALSE)
  )

  select <- sql_join_vars(con, vars)
  on <- sql_join_tbls(con, by)

  # Wrap with SELECT since callers assume a valid query is returned
  build_sql(
    "SELECT ", select, "\n",
    "  FROM ", x, "\n",
    "  ", JOIN, " ", y, "\n",
    if (!is.null(on)) build_sql("  ON ", on, "\n") else NULL,
    con = con
  )
}

sql_join_vars <- function(con, vars) {
  sql_vector(
    mapply(
      FUN = sql_join_var,
      alias = vars$alias,
      x = vars$x,
      y = vars$y,
      MoreArgs = list(con = con),
      SIMPLIFY = FALSE,
      USE.NAMES = TRUE
    ),
    parens = FALSE,
    collapse = ", ",
    con = con
  )
}

sql_join_var <- function(con, alias, x, y) {
  if (!is.na(x) & !is.na(y)) {
    sql_coalesce(
      sql_table_prefix(con, x, table = "TBL_LEFT"),
      sql_table_prefix(con, y, table = "TBL_RIGHT")
    )
  } else if (!is.na(x)) {
    sql_table_prefix(con, x, table = "TBL_LEFT")
  } else if (!is.na(y)) {
    sql_table_prefix(con, y, table = "TBL_RIGHT")
  } else {
    stop("No source for join column ", alias, call. = FALSE)
  }
}

sql_join_tbls <- function(con, by) {
  on <- NULL
  if (length(by$x) + length(by$y) > 0) {
    on <- sql_vector(
      paste0(
        sql_table_prefix(con, by$x, "TBL_LEFT"),
        " = ",
        sql_table_prefix(con, by$y, "TBL_RIGHT")
      ),
      collapse = " AND ",
      parens = TRUE
    )
  }

  on
}

sql_coalesce <- function(...) {
  vars <- sql_vector(list(...), parens = FALSE, collapse = ", ")
  build_sql("coalesce(", vars, ")")
}

sql_as <- function(con, var, alias = names(var), table = NULL) {
  if (length(var) == 0)
    return(ident())

  var <- sql_table_prefix(con, var, table = table)
  alias <- sql_escape_ident(con, alias)

  sql(paste0(var, " AS ", alias))
}

sql_table_prefix <- function(con, var, table = NULL) {
  var <- sql_escape_ident(con, var)

  if (!is.null(table)) {
    table <- sql_escape_ident(con, table)
    sql(paste0(table, ".", var))
  } else {
    var
  }

}

#' @export
sql_semi_join.DBIConnection <- function(con, x, y, anti = FALSE, by = NULL, ...) {
  # X and Y are subqueries named TBL_LEFT and TBL_RIGHT
  left <- escape(ident("TBL_LEFT"), con = con)
  right <- escape(ident("TBL_RIGHT"), con = con)
  on <- sql_vector(
    paste0(
      left,  ".", sql_escape_ident(con, by$x), " = ",
      right, ".", sql_escape_ident(con, by$y)
    ),
    collapse = " AND ",
    parens = TRUE,
    con = con
  )

  build_sql(
    "SELECT * FROM ", x, "\n\n",
    "WHERE ", if (anti) sql("NOT "), "EXISTS (\n",
    "  SELECT 1 FROM ", y, "\n",
    "  WHERE ", on, "\n",
    ")",
    con = con
  )
}

#' @export
sql_set_op.default <- function(con, x, y, method) {
  build_sql(
    "(", x, ")",
    "\n", sql(method), "\n",
    "(", y, ")"
  )
}

#' @export
sql_set_op.SQLiteConnection <- function(con, x, y, method) {
  # SQLite does not allow parentheses
  build_sql(
    x,
    "\n", sql(method), "\n",
    y
  )
}

#' @export
sql_escape_string.DBIConnection <- function(con, x) {
  dbQuoteString(con, x)
}

#' @export
sql_escape_string.NULL <- function(con, x) {
  sql_quote(x, "'")
}

#' @export
sql_escape_ident.DBIConnection <- function(con, x) {
  dbQuoteIdentifier(con, x)
}

#' @export
sql_escape_ident.NULL <- function(con, x) {
  sql_quote(x, '"')
}

#' @export
sql_escape_logical.DBIConnection <- function(con, x) {
  y <- as.character(x)
  y[is.na(x)] <- "NULL"

  y
}

#' @export
sql_escape_logical.NULL <- sql_escape_logical.DBIConnection


#' @export
sql_translate_env.NULL <- function(con) {
  sql_variant(
    base_scalar,
    base_agg,
    base_win
  )
}

#' @export
sql_translate_env.DBIConnection <- function(con) {
  sql_variant(
    base_scalar,
    base_agg,
    base_win
  )
}
