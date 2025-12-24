sql_select_clauses <- function(
  con,
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
  sql_format_clauses(out, lvl, con)
}

sql_clause <- function(kw, parts, sep = ",", parens = FALSE, lvl = 0) {
  if (length(parts) == 0) {
    return()
  }

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

sql_clause_select <- function(
  select,
  distinct = FALSE,
  top = NULL,
  lvl = 0
) {
  check_sql(select)
  check_bool(distinct)
  check_number_whole(top, allow_null = TRUE, allow_infinite = TRUE, min = 0)

  if (is_empty(select)) {
    cli_abort("Query contains no columns")
  }

  clause <- paste0(
    "SELECT",
    if (distinct) " DISTINCT",
    if (!is.null(top)) paste0(" TOP ", top)
  )
  sql_clause(clause, select)
}

sql_clause_from <- function(from, lvl = 0) {
  check_sql(from)
  sql_clause("FROM", from, lvl = lvl)
}

sql_clause_where <- function(where, lvl = 0) {
  check_sql(where, allow_null = TRUE)
  check_character(where, allow_null = TRUE)

  # wrap each clause in parens
  where <- sql(paste0("(", where, ")", recycle0 = TRUE))
  sql_clause("WHERE", where, sep = " AND", lvl = lvl)
}

sql_clause_group_by <- function(group_by, lvl = 0) {
  check_sql(group_by, allow_null = TRUE)
  sql_clause("GROUP BY", group_by)
}

sql_clause_having <- function(having, lvl = 0) {
  check_sql(having, allow_null = TRUE)

  # wrap each clause in parens
  having <- sql(paste0("(", having, ")", recycle0 = TRUE))
  sql_clause("HAVING", having, sep = " AND")
}

sql_clause_window <- function(window) {
  sql_clause("WINDOW", window)
}

sql_clause_order_by <- function(
  order_by,
  subquery = FALSE,
  limit = NULL,
  lvl = 0
) {
  check_sql(order_by, allow_null = TRUE)

  if (subquery && length(order_by) > 0 && is.null(limit)) {
    warn_drop_order_by()
    NULL
  } else {
    sql_clause("ORDER BY", order_by)
  }
}

sql_clause_limit <- function(limit, lvl = 0) {
  check_number_whole(limit, allow_null = TRUE, allow_infinite = TRUE, min = 0)

  if (!is.null(limit) && !identical(limit, Inf)) {
    sql_clause("LIMIT", sql(format(limit, scientific = FALSE)))
  }
}

sql_clause_update <- function(table) {
  sql_clause("UPDATE", table)
}

sql_clause_set <- function(lhs, rhs) {
  update_clauses <- sql(paste0(lhs, " = ", rhs))

  sql_clause("SET", update_clauses)
}

sql_clause_insert <- function(con, cols, into = NULL, lvl = 0) {
  cols <- sql(sql_escape_ident(con, cols))

  if (is.null(into)) {
    sql_clause("INSERT", cols, parens = TRUE, lvl = lvl)
  } else {
    kw <- sql_glue2(con, "INSERT INTO {.tbl into}")
    sql_clause(kw, cols, parens = TRUE, lvl = lvl)
  }
}

sql_clause_on <- function(on, lvl = 0, parens = FALSE) {
  sql_clause("ON", on, sep = " AND", parens = parens, lvl = lvl)
}

sql_clause_where_exists <- function(table, where, not) {
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
  cat("<sql clause>", format(x))
  invisible()
}

#' @export
format.sql_clause <- function(x, ...) {
  unclass(sql_format_clause(x, lvl = 0, con = simulate_dbi()))
}


# helpers -----------------------------------------------------------------

sql_format_clauses <- function(clauses, lvl, con) {
  clauses <- purrr::compact(unname(clauses))

  formatted_clauses <- purrr::map_chr(
    clauses,
    sql_format_clause,
    lvl = lvl,
    con = con
  )
  clause_level <- purrr::map_dbl(clauses, "lvl", .default = 0)
  out <- indent_lvl(formatted_clauses, lvl + clause_level)

  sql(paste0(out, collapse = "\n"))
}

sql_format_clause <- function(x, lvl, con, nchar_max = 80) {
  if (is.sql(x)) {
    return(x)
  }

  lvl <- lvl + x$lvl

  # check length without starting a new line
  if (x$sep == " AND") {
    x$sep <- style_kw(x$sep)
  }

  fields_same_line <- escape(x$parts, collapse = paste0(x$sep, " "), con = con)
  if (x$parens) {
    fields_same_line <- paste0("(", fields_same_line, ")")
  }

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
    escape(x$parts, collapse = collapse, con = con),
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
