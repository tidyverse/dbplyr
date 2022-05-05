#' @export
#' @rdname sql_build
join_query <- function(x, y, vars, type = "inner", by = NULL, suffix = c(".x", ".y"), na_matches = FALSE) {
  structure(
    list(
      x = x,
      y = y,
      vars = vars,
      type = type,
      by = by,
      na_matches = na_matches
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
sql_render.join_query <- function(query, con = NULL, ..., subquery = FALSE, lvl = 0) {
  from_x <- sql_render(query$x, con, ..., subquery = TRUE, lvl = lvl + 1)
  from_y <- sql_render(query$y, con, ..., subquery = TRUE, lvl = lvl + 1)

  dbplyr_query_join(con, from_x, from_y,
    vars = query$vars,
    type = query$type,
    by = query$by,
    na_matches = query$na_matches,
    lvl = lvl
  )
}

# SQL generation ----------------------------------------------------------


sql_join_vars <- function(con, vars, x_as = ident("LHS"), y_as = ident("RHS")) {
  join_vars_list <- mapply(
    FUN = sql_join_var,
    alias = vars$alias,
    x = vars$x,
    y = vars$y,
    MoreArgs = list(con = con, all_x = vars$all_x, all_y = vars$all_y, x_as = x_as, y_as = y_as),
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE
  )

  sql(unlist(join_vars_list))
}

sql_join_var <- function(con, alias, x, y, all_x, all_y, x_as, y_as) {
  if (!is.na(x) && !is.na(y)) {
    sql_expr(
      COALESCE(
        !!sql_table_prefix(con, x, table = x_as),
        !!sql_table_prefix(con, y, table = y_as)
      ),
      con = con
    )
  } else if (!is.na(x)) {
    sql_table_prefix(con, x, table = if (tolower(x) %in% tolower(all_y)) x_as)
  } else if (!is.na(y)) {
    sql_table_prefix(con, y, table = if (tolower(y) %in% tolower(all_x)) y_as)
  } else {
    cli_abort("No source for join column {alias}") # nocov
  }
}

sql_join_tbls <- function(con, by, na_matches = "never") {
  na_matches <- arg_match(na_matches, c("na", "never"))

  if (na_matches == "na" || length(by$x) + length(by$y) > 0) {
    lhs <- sql_table_prefix(con, by$x, by$x_as %||% ident("LHS"))
    rhs <- sql_table_prefix(con, by$y, by$y_as %||% ident("RHS"))

    if (na_matches == "na") {
      compare <- purrr::map_chr(seq_along(lhs), function(i) {
        sql_expr_matches(sql(lhs[[i]]), sql(rhs[[i]]), con = con)
      })
    } else {
      compare <- paste0(lhs, " = ", rhs)
    }

    sql(compare)
  } else if (length(by$on) > 0) {
    by$on
  }
}

sql_table_prefix <- function(con, var, table = NULL) {
  var <- sql_escape_ident(con, var)

  if (!is.null(table)) {
    table <- escape(table, con = con)
    sql(paste0(table, ".", var))
  } else {
    var
  }
}

utils::globalVariables("COALESCE")
