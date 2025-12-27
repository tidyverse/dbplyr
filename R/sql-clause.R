sql_select_clauses <- function(
  select,
  from,
  where,
  group_by,
  having,
  window,
  order_by,
  limit = NULL,
  lvl = 0
) {
  out <- list(select, from, where, group_by, having, window, order_by, limit)
  sql_format_clauses(out, lvl)
}

sql_clause <- function(kw, parts, sep = ",", parens = FALSE, lvl = 0) {
  if (length(parts) == 0) {
    return()
  }

  check_sql(parts, allow_names = FALSE)

  clause <- list(
    kw = kw,
    parts = parts,
    sep = sep,
    parens = parens,
    lvl = lvl
  )

  class(clause) <- "sql_clause"
  clause
}

sql_clause_select <- function(select, distinct = FALSE, top = NULL, lvl = 0) {
  check_sql(select)
  check_bool(distinct)
  check_number_whole(top, min = 0, allow_null = TRUE)

  if (is_empty(select)) {
    cli_abort("Query contains no columns")
  }

  clause <- paste0(
    "SELECT",
    if (distinct) " DISTINCT",
    if (!is.null(top)) paste0(" TOP ", as.integer(top))
  )
  sql_clause(clause, select)
}

sql_clause_from <- function(from, lvl = 0) {
  check_sql(from)
  sql_clause("FROM", from, lvl = lvl)
}

sql_clause_where <- function(where, lvl = 0) {
  check_sql(where, allow_null = TRUE)
  where <- sql(paste0("(", where, ")", recycle0 = TRUE))
  sql_clause("WHERE", where, sep = " AND", lvl = lvl)
}

sql_clause_group_by <- function(group_by, lvl = 0) {
  check_sql(group_by, allow_null = TRUE)
  sql_clause("GROUP BY", group_by)
}

sql_clause_having <- function(having, lvl = 0, call = caller_env()) {
  check_sql(having, allow_null = TRUE)
  having <- sql(paste0("(", having, ")", recycle0 = TRUE))
  sql_clause("HAVING", having, sep = " AND")
}

sql_clause_window <- function(window) {
  check_sql(window, allow_null = TRUE)
  sql_clause("WINDOW", window)
}

sql_clause_order_by <- function(
  order_by,
  subquery = FALSE,
  limit = NULL,
  lvl = 0
) {
  if (subquery && length(order_by) > 0 && is.null(limit)) {
    warn_drop_order_by()
    NULL
  } else {
    sql_clause("ORDER BY", order_by)
  }
}

sql_clause_limit <- function(con, limit, lvl = 0) {
  if (!is.null(limit) && !identical(limit, Inf)) {
    sql_clause("LIMIT", sql(format(limit, scientific = FALSE)))
  }
}

sql_clause_update <- function(table) {
  check_sql(table)
  sql_clause("UPDATE", table)
}

sql_clause_set <- function(lhs, rhs) {
  update_clauses <- sql(paste0(lhs, " = ", rhs))

  sql_clause("SET", update_clauses)
}

sql_clause_insert <- function(cols, into = NULL, lvl = 0) {
  check_sql(cols)
  check_sql(into, allow_null = TRUE)

  if (is.null(into)) {
    sql_clause("INSERT", cols, parens = TRUE, lvl = lvl)
  } else {
    kw <- sql(paste0(style_kw("INSERT INTO "), into))
    sql_clause(kw, cols, parens = TRUE, lvl = lvl)
  }
}

sql_clause_on <- function(on, lvl = 0, parens = FALSE) {
  sql_clause("ON", on, sep = " AND", parens = parens, lvl = lvl)
}

sql_clause_where_exists <- function(table, where, not) {
  check_sql(table)
  list(
    sql(paste0("WHERE ", if (not) "NOT ", "EXISTS (")),
    # lvl = 1 because they are basically in a subquery
    sql_clause("SELECT 1 FROM", table, lvl = 1),
    sql_clause_where(where, lvl = 1),
    sql(")")
  )
}

#' @export
print.sql_clause <- function(x, ...) {
  out <- sql_format_clause(x, lvl = 0)
  cat("<sql clause>", out)
}

# helpers -----------------------------------------------------------------

sql_format_clauses <- function(clauses, lvl) {
  clauses <- purrr::compact(clauses)

  formatted_clauses <- purrr::map(clauses, sql_format_clause, lvl = lvl)
  clause_level <- purrr::map_dbl(clauses, "lvl", .default = 0)
  out <- indent_lvl(formatted_clauses, lvl + clause_level)

  sql(paste0(out, collapse = "\n"))
}

sql_format_clause <- function(x, lvl, nchar_max = 80) {
  if (is.sql(x)) {
    return(x)
  }

  lvl <- lvl + x$lvl

  # check length without starting a new line
  if (x$sep == " AND") {
    x$sep <- style_kw(x$sep)
  }

  fields_same_line <- sql_collapse(
    x$parts,
    collapse = paste0(x$sep, " "),
    parens = x$parens
  )

  x$kw <- style_kw(x$kw)
  same_line_clause <- paste0(x$kw, " ", fields_same_line)
  nchar_same_line <- cli::ansi_nchar(lvl_indent(lvl)) +
    cli::ansi_nchar(same_line_clause)

  if (length(x$parts) == 1 || nchar_same_line <= nchar_max) {
    return(sql(same_line_clause))
  }

  indent <- lvl_indent(lvl + 1)
  collapse <- paste0(x$sep, "\n", indent)

  field_string <- paste0(
    x$kw,
    if (x$parens) " (",
    "\n",
    indent,
    sql_collapse(x$parts, collapse = collapse),
    if (x$parens) paste0("\n", indent_lvl(")", lvl))
  )

  sql(field_string)
}

lvl_indent <- function(times, char = "  ") {
  strrep(char, times)
}

indent_lvl <- function(x, lvl) {
  sql(paste0(lvl_indent(lvl), x))
}
