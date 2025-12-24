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
  check_sql(parts)

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
  sql_clause(clause, select, lvl = lvl)
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
  sql_clause("GROUP BY", group_by, lvl = lvl)
}

sql_clause_having <- function(having, lvl = 0) {
  check_sql(having, allow_null = TRUE)

  # wrap each clause in parens
  having <- sql(paste0("(", having, ")", recycle0 = TRUE))
  sql_clause("HAVING", having, sep = " AND", lvl = lvl)
}

sql_clause_window <- function(window, lvl = 0) {
  sql_clause("WINDOW", window, lvl = lvl)
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
    sql_clause("ORDER BY", order_by, lvl = lvl)
  }
}

sql_clause_limit <- function(limit, lvl = 0) {
  check_number_whole(limit, allow_null = TRUE, allow_infinite = TRUE, min = 0)

  if (!is.null(limit) && !identical(limit, Inf)) {
    sql_clause("LIMIT", sql(format(limit, scientific = FALSE)), lvl = lvl)
  }
}

sql_clause_update <- function(table, lvl = 0) {
  sql_clause("UPDATE", table, lvl = lvl)
}

sql_clause_set <- function(lhs, rhs, lvl = 0) {
  update_clauses <- sql(paste0(lhs, " = ", rhs))

  sql_clause("SET", update_clauses, lvl = lvl)
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
  unclass(sql_format_clause(x, lvl = 0))
}


# helpers -----------------------------------------------------------------

sql_format_clauses <- function(clauses, lvl) {
  clauses <- purrr::compact(unname(clauses))

  formatted_clauses <- purrr::map_chr(clauses, sql_format_clause, lvl = lvl)
  clause_level <- purrr::map_dbl(clauses, "lvl", .default = 0)
  out <- paste0(lvl_indent(lvl + clause_level), formatted_clauses)

  sql(paste0(out, collapse = "\n"))
}

sql_format_clause <- function(x, lvl, nchar_max = 80) {
  if (is.sql(x)) {
    return(x)
  }

  lvl <- lvl + x$lvl
  if (x$sep == " AND") {
    x$sep <- style_kw(x$sep)
  }

  # check length without starting a new line

  one_line <- paste(x$parts, collapse = paste0(x$sep, " "))
  if (x$parens) {
    one_line <- paste0("(", one_line, ")")
  }

  x$kw <- style_kw(x$kw)
  one_line <- paste0(x$kw, " ", one_line)
  nchar_same_line <- lvl * 2 + cli::ansi_nchar(one_line)

  if (length(x$parts) == 1 || nchar_same_line <= nchar_max) {
    return(one_line)
  }

  indent <- lvl_indent(lvl + 1)
  collapse <- paste0(x$sep, "\n", indent)

  paste0(
    x$kw,
    if (x$parens) " (",
    "\n",
    paste0(indent, x$parts, collapse = paste0(x$sep, "\n")),
    if (x$parens) paste0("\n", lvl_indent(lvl), ")")
  )
}

lvl_indent <- function(times) {
  strrep("  ", times)
}
