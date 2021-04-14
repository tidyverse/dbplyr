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

sql_clause_select <- function(con, select, distinct = FALSE, top = NULL, level = 0) {
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

  sql_clause_generic(con, clause, select, level = level)
}

sql_clause_from  <- function(con, from, level = 0) {
  sql_clause_generic(con, "FROM", from, level = level)
}

sql_clause_where <- function(con, where, level = 0) {
  if (length(where) == 0L) {
    return()
  }

  assert_that(is.character(where))
  where_paren <- escape(where, parens = TRUE, con = con)
  sql_clause_generic(con, "WHERE", where_paren, level = level, sep = " AND")
}

sql_clause_group_by <- function(con, group_by, level = 0) {
  sql_clause_generic(con, "GROUP BY", group_by, level = level)
}

sql_clause_having <- function(con, having, level = 0) {
  sql_clause_generic(con, "HAVING", having, level = level)
}

sql_clause_order_by <- function(con, order_by, subquery = FALSE, limit = NULL, level = 0) {
  if (subquery && length(order_by) > 0 && is.null(limit)) {
    warn_drop_order_by()
    NULL
  } else {
    sql_clause_generic(con, "ORDER BY", order_by, level = level)
  }
}

sql_clause_limit <- function(con, limit, level = 0){
  if (!is.null(limit) && !identical(limit, Inf)) {
    build_sql(
      "LIMIT ", sql(format(limit, scientific = FALSE)),
      con = con
    )
  }
}

# helpers -----------------------------------------------------------------

sql_clause_generic <- function(con, clause, fields, level = 0, sep = ",") {
  if (length(fields) == 0L) {
    return()
  }

  assert_that(is.character(fields))
  build_sql(
    !!get_clause_indent(level), sql(clause), !!get_clause_separator(fields, level),
    escape(fields, collapse = get_field_separator(fields, sep = sep, level), con = con),
    con = con
  )
}

lvl_indent <- function(times, char = "  ") {
  paste(rep.int(char, times), collapse = "")
}

get_clause_indent <- function(level) {
  if (getOption("dbplyr_break_subquery", FALSE)) {
    lvl_indent(level)
  } else {
    ""
  }
}

get_clause_separator <- function(fields, level) {
  if (break_after_clause(fields)) {
    field_collapse <- paste0("\n", lvl_indent(level + 1))
  } else {
    field_collapse <- " "
  }
}

get_field_separator <- function(fields, sep, level) {
  if (break_between_fields(fields)) {
    paste0(sep, "\n", lvl_indent(level + 1))
  } else {
    paste0(sep, " ")
  }
}

break_after_clause <- function(fields) {
  # TODO add option to always break or not break at all
  getOption("dbplyr_indent_fields", FALSE) && length(fields) > 1
}

break_between_fields <- function(fields) {
  # TODO improve option to control
  getOption("dbplyr_indent_fields", FALSE)
}
