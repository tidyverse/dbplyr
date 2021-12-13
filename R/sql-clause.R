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
    sql_kw("SELECT"),
    if (distinct) sql_kw(" DISTINCT"),
    if (!is.null(top)) build_sql(sql_kw(" TOP "), as.integer(top), con = con),
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
    sql <- build_sql(
      sql_kw("LIMIT"), " ", sql(format(limit, scientific = FALSE)),
      con = con
    )

    indent_lvl(sql, lvl)
  }
}

# helpers -----------------------------------------------------------------

sql_clause_generic <- function(con, clause, fields, lvl = 0, sep = ",", parens = FALSE) {
  if (length(fields) == 0L) {
    return()
  }

  assert_that(is.character(fields))
  fields_formatted <- format_fields(
    fields, sep,
    lvl = lvl,
    kw = clause,
    con = con,
    parens = parens
  )
  sql <- build_sql(
    sql_kw(clause),
    fields_formatted,
    con = con
  )

  indent_lvl(sql, lvl)
}

sql_kw <- function(kw) {
  sql(kw)
}

#' @noRd
#' @examples
#' con <- simulate_dbi()
#' cols <- ident("mpg", "cyl", "gear")
#' conds <- sql("`mpg` > 0", "`cyl` = 6")
#'
#' cat(paste0("SELECT", field_minimal(cols, ",", con = con)))
#' cat(paste0("SELECT", field_minimal(conds, " AND", con = con)))
#'
#' cat(paste0("SELECT", field_align(cols, ",", lvl = 0, kw = "SELECT", con = con)))
#' cat(paste0("SELECT", field_align(conds, " AND", lvl = 0, kw = "SELECT", con = con)))
#'
#' cat(paste0("SELECT", field_indent(cols, ",", lvl = 0, con = con, parens = FALSE)))
#' cat(paste0("SELECT", field_indent(cols, ",", lvl = 0, con = con, parens = TRUE)))
#' cat(paste0("SELECT", field_indent(conds, " AND", lvl = 0, con = con)))
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
