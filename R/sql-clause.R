sql_select_clauses <- function(con,
                               select,
                               from,
                               where,
                               group_by,
                               having,
                               order_by,
                               limit = NULL,
                               lvl) {
  out <- list(
    select = select,
    from = from,
    where = where,
    group_by = group_by,
    having = having,
    order_by = order_by,
    limit = limit
  )
  sql_format_clauses(out, lvl, con)
}

sql_clause <- function(kw, parts, sep = ",", parens = FALSE, lvl = 0) {
  list(
    kw = kw,
    parts = parts,
    sep = sep,
    parens = parens,
    lvl = lvl
  )
}

sql_clause_select <- function(con, select, distinct = FALSE, top = NULL, lvl = 0) {
  assert_that(is.character(select))
  if (is_empty(select)) {
    abort("Query contains no columns")
  }

  clause <- build_sql(
    sql_kw("SELECT"),
    if (distinct) sql_kw(" DISTINCT"),
    if (!is.null(top)) build_sql(sql_kw(" TOP "), as.integer(top), con = con),
    con = con
  )

  sql_clause(clause, select)
}

sql_clause_from  <- function(from, lvl = 0) {
  sql_clause("FROM", from, lvl = lvl)
}

sql_clause_where <- function(where, lvl = 0) {
  if (length(where) == 0L) {
    return()
  }

  assert_that(is.character(where))
  where_paren <- sql(paste0("(", where, ")"))
  sql_clause("WHERE", where_paren, sep = " AND", lvl = lvl)
}

sql_clause_group_by <- function(group_by, lvl = 0) {
  sql_clause("GROUP BY", group_by)
}

sql_clause_having <- function(having, lvl = 0) {
  sql_clause("HAVING", having)
}

sql_clause_order_by <- function(order_by, subquery = FALSE, limit = NULL, lvl = 0) {
  if (subquery && length(order_by) > 0 && is.null(limit)) {
    warn_drop_order_by()
    NULL
  } else {
    sql_clause("ORDER BY", order_by)
  }
}

sql_clause_limit <- function(con, limit, lvl = 0){
  if (!is.null(limit) && !identical(limit, Inf)) {
    sql_clause(sql_kw("LIMIT"), sql(format(limit, scientific = FALSE)))
  }
}

# helpers -----------------------------------------------------------------

sql_kw <- function(kw) {
  sql(kw)
}

sql_format_clauses <- function(clauses, lvl, con) {
  clauses <- purrr::discard(clauses, ~ !is.sql(.x) && is_empty(.x$parts))

  out <- purrr::map(clauses, sql_format_clause, lvl = lvl, con = con)
  out <- purrr::map2(
    out, purrr::map_dbl(clauses, ~ purrr::pluck(.x, "lvl", .default = 0)),
    ~ indent_lvl(.x, lvl = lvl + .y)
  )

  escape(unname(out), collapse = "\n", parens = FALSE, con = con)
}

sql_format_clause <- function(x, lvl, con, nchar_max = 80) {
  if (is.sql(x)) {
    return(x)
  }

  parts_sql <- format_fields(
    x$parts,
    field_sep = x$sep,
    lvl = lvl + x$lvl,
    kw = x$kw,
    con = con,
    parens = x$parens,
    nchar_max = nchar_max
  )

  sql(paste0(x$kw, parts_sql))
}

#' @noRd
#' @examples
#' con <- simulate_dbi()
#' cols <- ident("mpg", "cyl", "gear")
#' conds <- sql("`mpg` > 0", "`cyl` = 6")
#'
#' cat(paste0("SELECT", format_fields(cols, ",", lvl = 0, kw = "SELECT", con = con, parens = FALSE)))
#' cat(paste0("SELECT", format_fields(cols, ",", lvl = 0, kw = "SELECT", con = con, parens = TRUE)))
#' cat(paste0("SELECT", format_fields(conds, " AND", lvl = 0, kw = "SELECT", con = con)))
format_fields <- function(x, field_sep, lvl, kw, con, parens = FALSE, nchar_max = 80) {
  # check length without starting a new line
  fields_same_line <- escape(x, collapse = paste0(field_sep, " "), con = con)
  if (parens) {
    fields_same_line <- paste0(" (", fields_same_line, ")")
  } else {
    fields_same_line <- paste0(" ", fields_same_line)
  }
  nchar_same_line <- nchar(lvl_indent(lvl)) + nchar(kw) + nchar(fields_same_line)

  if (length(x) == 1 || nchar_same_line <= nchar_max) {
    return(sql(fields_same_line))
  }

  indent <- lvl_indent(lvl + 1)
  collapse <- paste0(field_sep, "\n", indent)

  field_string <- paste0(
    if (parens) " (", "\n",
    indent, escape(x, collapse = collapse, con = con),
    if (parens) paste0("\n", indent_lvl(")", lvl))
  )

  sql(field_string)
}

sql_clause_kw <- function(..., lvl) {
  sql_kw(paste0(get_clause_indent(lvl), ...))
}

rep_char <- function(times, char) {
  strrep(char, times)
}

lvl_indent <- function(times, char = "  ") {
  rep_char(times, char)
}

indent_lvl <- function(x, lvl) {
  sql(paste0(lvl_indent(lvl), x))
}

get_clause_indent <- function(lvl) {
  indent_lvl("", lvl)
}
