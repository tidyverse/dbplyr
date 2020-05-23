#' @export
#' @rdname sql_build
join_query <- function(x, y, vars, type = "inner", by = NULL, suffix = c(".x", ".y")) {
  structure(
    list(
      x = x,
      y = y,
      vars = vars,
      type = type,
      by = by
    ),
    class = c("join_query", "query")
  )
}

#' @export
print.join_query <- function(x, ...) {
  cat_line("<SQL JOIN (", toupper(x$type), ")>")

  cat_line("By:")
  cat_line(indent(paste0(x$by$x, "-", x$by$y)))

  cat_line("X:")
  cat_line(indent_print(sql_build(x$x)))

  cat_line("Y:")
  cat_line(indent_print(sql_build(x$y)))
}

#' @export
sql_render.join_query <- function(query, con = NULL, ..., bare_identifier_ok = FALSE) {
  from_x <- sql_subquery(
    con,
    sql_render(query$x, con, ..., bare_identifier_ok = TRUE),
    name = "LHS"
  )
  from_y <- sql_subquery(
    con,
    sql_render(query$y, con, ..., bare_identifier_ok = TRUE),
    name = "RHS"
  )

  sql_join(con, from_x, from_y, vars = query$vars, type = query$type, by = query$by)
}

# SQL generation ----------------------------------------------------------

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
    "FROM ", x, "\n",
    JOIN, " ", y, "\n",
    if (!is.null(on)) build_sql("ON ", on, "\n", con = con) else NULL,
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
  if (!is.na(x) && !is.na(y)) {
    sql_expr(
      COALESCE(
        !!sql_table_prefix(con, x, table = "LHS"),
        !!sql_table_prefix(con, y, table = "RHS")
      ),
      con = con
    )
  } else if (!is.na(x)) {
    sql_table_prefix(con, x, table = "LHS")
  } else if (!is.na(y)) {
    sql_table_prefix(con, y, table = "RHS")
  } else {
    stop("No source for join column ", alias, call. = FALSE)
  }
}

sql_join_tbls <- function(con, by) {
  on <- NULL
  if (length(by$x) + length(by$y) > 0) {
    on <- sql_vector(
      paste0(
        sql_table_prefix(con, by$x, "LHS"),
        " = ",
        sql_table_prefix(con, by$y, "RHS")
      ),
      collapse = " AND ",
      parens = TRUE,
      con = con
    )
  } else if (length(by$on) > 0) {
    on <- build_sql("(", by$on, ")", con = con)
  }

  on
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

utils::globalVariables("COALESCE")
