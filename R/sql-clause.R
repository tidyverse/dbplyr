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

sql_clause_generic <- function(con, clause, fields, lvl = 0, sep = ",") {
  if (length(fields) == 0L) {
    return()
  }

  assert_that(is.character(fields))
  sql <- build_sql(
    sql_kw(clause),
    format_fields(fields, sep, lvl = lvl, kw = clause, con = con, strategy = "indent"),
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
format_fields <- function(x, field_sep, lvl, kw, con, parens = FALSE, strategy = c("minimal", "align", "indent")) {
  strategy <- match.arg(strategy)
  field_string <- switch(strategy,
    minimal = field_minimal(x, field_sep, con = con, parens = parens),
    align = field_align(x, field_sep, kw = kw, lvl = lvl, con = con, parens = parens),
    indent = field_indent(x, field_sep, lvl = lvl, con = con, parens = parens)
  )
  sql(field_string)
}

field_minimal <- function(x, field_sep, con, parens) {
  paste0(" ", if (parens) "(", escape(x, collapse = paste0(" ", field_sep), con = con), if (parens) ")")
}

field_align <- function(x, field_sep, lvl, kw, con, parens) {
  if (length(x) == 1) {
    return(paste0(if (parens) "(" else " ", escape(x, con = con), if (parens) ")"))
  }

  kw_indent <- rep_char(nchar(kw) + (parens == TRUE), " ")
  collapse <- paste0(field_sep, "\n", " ", lvl_indent(lvl), kw_indent)
  # TODO should this really look like this??
  paste0(" ", if (parens) "(", escape(x, collapse = collapse, con = con), if (parens) ")")
}

field_indent <- function(x, field_sep, lvl, con, parens) {
  if (length(x) == 1) {
    return(paste0(if (parens) "(" else " ", escape(x, con = con), if (parens) ")"))
  }

  indent <- lvl_indent(lvl + 1)
  collapse <- paste0(field_sep, "\n", indent)
  paste0(
    if (parens) "(", "\n",
    indent, escape(x, collapse = collapse, con = con),
    if (parens) paste0("\n", indent_lvl(")", lvl))
  )
}

sql_clause_kw <- function(..., lvl) {
  sql_kw(paste0(get_clause_indent(lvl), ...))
}

rep_char <- function(times, char) {
  paste(rep.int(char, times), collapse = "")
}

lvl_indent <- function(times, char = "  ") {
  rep_char(times, char)
}

indent_lvl <- function(x, lvl) {
  if (getOption("dbplyr_break_subquery", FALSE)) {
    sql(paste0(lvl_indent(lvl), x))
  } else {
    x
  }
}

get_clause_indent <- function(lvl) {
  if (getOption("dbplyr_break_subquery", FALSE)) {
    sql(lvl_indent(lvl))
  } else {
    sql("")
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
