sql_select_clauses <- function(con,
                               select,
                               from,
                               where,
                               group_by,
                               having,
                               order_by,
                               limit = NULL) {
  out <- list(
    select = select,
    from = from,
    where = where,
    group_by = group_by,
    having = having,
    order_by = order_by,
    limit = limit
  )
  out <- purrr::compact(out)
  escape(unname(out), collapse = "\n", parens = FALSE, con = con)
}

sql_clause_select <- function(con, select, distinct = FALSE, top = NULL, lvl = 0) {
  assert_that(is.character(select))
  if (is_empty(select)) {
    abort("Query contains no columns")
  }

  clause <- build_sql(
    "SELECT",
    if (distinct) sql(" DISTINCT"),
    if (!is.null(top)) build_sql(" TOP ", as.integer(top), con = con),
    con = con
  )

  sql_clause_generic(con, clause, select, lvl = lvl)
}

sql_clause_from  <- function(con, from, lvl = 0) {
  sql_clause_generic(con, "FROM", from, lvl = lvl)
}

sql_clause_where <- function(con, where, lvl = 0) {
  if (length(where) == 0L) {
    return()
  }

  assert_that(is.character(where))
  where_paren <- escape(where, parens = TRUE, con = con)
  sql_clause_generic(con, "WHERE", where_paren, lvl = lvl, sep = " AND")
}

sql_clause_group_by <- function(con, group_by, lvl = 0) {
  sql_clause_generic(con, "GROUP BY", group_by, lvl = lvl)
}

sql_clause_having <- function(con, having, lvl = 0) {
  sql_clause_generic(con, "HAVING", having, lvl = lvl)
}

sql_clause_order_by <- function(con, order_by, subquery = FALSE, limit = NULL, lvl = 0) {
  if (subquery && length(order_by) > 0 && is.null(limit)) {
    warn_drop_order_by()
    NULL
  } else {
    sql_clause_generic(con, "ORDER BY", order_by, lvl = lvl)
  }
}

sql_clause_limit <- function(con, limit, lvl = 0){
  if (!is.null(limit) && !identical(limit, Inf)) {
    build_sql_line(
      "LIMIT ", sql(format(limit, scientific = FALSE)),
      con = con,
      lvl = lvl
    )
  }
}

# helpers -----------------------------------------------------------------

sql_clause_generic <- function(con, clause, fields, lvl = 0, sep = ",") {
  if (length(fields) == 0L) {
    return()
  }

  assert_that(is.character(fields))
  build_sql_line(
    sql(clause), !!get_clause_separator(fields, lvl),
    escape(fields, collapse = get_field_separator(fields, sep = sep, lvl), con = con),
    con = con,
    lvl = lvl
  )
}

sql_clause_kw <- function(..., lvl) {
  sql(paste0(get_clause_indent(lvl), ...))
}

build_sql_wrap <- function(..., .env = parent.frame(), con = sql_current_con(), lvl = 0) {
  if (getOption("dbplyr_break_subquery", FALSE)) {
    build_sql("(\n", ..., "\n", !!get_clause_indent(lvl), ")", con = con, .env = .env)
  } else {
    build_sql("(", ..., ")", con = con, .env = .env)
  }
}

lvl_indent <- function(times, char = "  ") {
  paste(rep.int(char, times), collapse = "")
}

build_sql_line <- function(..., .env = parent.frame(), con = sql_current_con(), lvl = 0) {
  build_sql(
    !!get_clause_indent(lvl), ...,
    .env = .env,
    con = con
  )
}

get_clause_indent <- function(lvl) {
  if (getOption("dbplyr_break_subquery", FALSE)) {
    lvl_indent(lvl)
  } else {
    ""
  }
}

get_clause_separator <- function(fields, lvl) {
  if (break_after_clause(fields)) {
    field_collapse <- paste0("\n", lvl_indent(lvl + 1))
  } else {
    field_collapse <- " "
  }
}

get_field_separator <- function(fields, sep, lvl) {
  if (break_between_fields(fields)) {
    paste0(sep, "\n", lvl_indent(lvl + 1))
  } else {
    paste0(sep, " ")
  }
}

break_after_clause <- function(fields) {
  getOption("dbplyr_indent_fields", FALSE) && length(fields) > 1
}

break_between_fields <- function(fields) {
  getOption("dbplyr_indent_fields", FALSE)
}
