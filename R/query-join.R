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

multi_join_query <- function(x, joins, table_names, vars, all_vars_list) {
  structure(
    list(
      x = x,
      joins = joins,
      table_names = table_names,
      vars = vars,
      all_vars_list = all_vars_list
    ),
    class = c("multi_join_query", "query")
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
print.multi_join_query <- function(x, ...) {
  cat_line("<SQL JOINS>")

  cat_line("X:")
  cat_line(indent_print(sql_build(x$x)))

  for (i in vctrs::vec_seq_along(x$joins)) {
    cat_line("Type: ", paste0(x$joins$type[[i]]))

    cat_line("By:")
    cat_line(indent(paste0(x$joins$by_x[[i]], "-", x$joins$by_y[[i]])))

    cat_line("Y:")
    cat_line(indent_print(sql_build(x$joins$table[[i]])))
  }
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

#' @export
sql_render.multi_join_query <- function(query, con = NULL, ..., subquery = FALSE, lvl = 0) {
  x <- sql_render(query$x, con, ..., subquery = TRUE, lvl = lvl + 1)
  query$joins$table <- purrr::map(
    query$joins$table,
    ~ sql_render(.x, con, ..., subquery = TRUE, lvl = lvl + 1)
  )

  sql_query_multi_join(
    con = con,
    x = x,
    joins = query$joins,
    table_names = query$table_names,
    vars = query$vars,
    all_vars_list = query$all_vars_list,
    lvl = lvl
  )
}

# SQL generation ----------------------------------------------------------

sql_join_vars <- function(con, vars, x_as = "LHS", y_as = "RHS", type) {
  multi_join_vars <- purrr::map2_dfr(
    vars$x, vars$y,
    ~ {
      table <- integer()
      var <- character()
      if (!is.na(.x)) {
        table <- 1L
        var <- .x
      }

      if (!is.na(.y)) {
        table <- c(table, 2L)
        var <- c(var, .y)
      }

      vctrs::new_data_frame(list(table = list(table), var = list(var)), n = 1L)
    }
  )
  multi_join_vars <- vctrs::vec_cbind(name = vars$alias, multi_join_vars)

  sql_multi_join_vars(
    con,
    multi_join_vars,
    table_names = c(unclass(x_as), unclass(y_as)),
    all_vars_list = vars[c("all_x", "all_y")]
  )
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
    table <- escape(table, collapse = NULL, con = con)
    sql(paste0(table, ".", var))
  } else {
    var
  }
}

utils::globalVariables("COALESCE")
