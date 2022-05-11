#' @export
#' @rdname sql_build
lazy_join_query <- function(x,
                            y,
                            vars,
                            type = "inner",
                            by = NULL,
                            suffix = c(".x", ".y"),
                            na_matches = FALSE,
                            group_vars = NULL,
                            order_vars = NULL,
                            frame = NULL) {
  carry_over <- c("group_vars", "order_vars", "frame", "last_op")

  structure(
    list(
      x = x,
      y = y,
      vars = vars,
      type = type,
      by = by,
      suffix = suffix,
      na_matches = na_matches,
      group_vars = group_vars %||% op_grps(x),
      order_vars = order_vars %||% op_sort(x),
      frame = frame %||% op_frame(x),
      last_op = "join"
    ),
    class = c("lazy_join_query", "lazy_query")
  )
}

#' @export
#' @rdname sql_build
lazy_semi_join_query <- function(x,
                                 y,
                                 vars,
                                 anti = FALSE,
                                 by = NULL,
                                 na_matches = FALSE,
                                 group_vars = NULL,
                                 order_vars = NULL,
                                 frame = NULL) {
  structure(
    list(
      x = x,
      y = y,
      vars = vars,
      anti = anti,
      by = by,
      na_matches = na_matches,
      group_vars = group_vars %||% op_grps(x),
      order_vars = order_vars %||% op_sort(x),
      frame = frame %||% op_frame(x),
      last_op = "semi_join"
    ),
    class = c("lazy_semi_join_query", "lazy_query")
  )
}

#' @export
print.lazy_join_query <- function(x, ...) {
  cat_line("<SQL JOIN (", toupper(x$type), ")>")

  cat_line("By:")
  cat_line(indent(paste0(x$by$x, "-", x$by$y)))

  cat_line("X:")
  cat_line(indent_print(sql_build(x$x, simulate_dbi())))

  cat_line("Y:")
  cat_line(indent_print(sql_build(x$y, simulate_dbi())))
}

#' @export
print.lazy_semi_join_query <- function(x, ...) {
  cat_line("<SQL ", if (x$anti) "ANTI" else "SEMI", " JOIN>")

  cat_line("By:")
  cat_line(indent(paste0(x$by$x, "-", x$by$y)))

  cat_line("X:")
  cat_line(indent_print(sql_build(x$x, simulate_dbi())))

  cat_line("Y:")
  cat_line(indent_print(sql_build(x$y, simulate_dbi())))
}

#' @export
op_vars.lazy_join_query <- function(op) {
  op$vars$alias
}
#' @export
op_vars.lazy_semi_join_query <- function(op) {
  names(op$vars)
}

#' @export
sql_build.lazy_join_query <- function(op, con, ...) {
  join_query(
    sql_optimise(sql_build(op$x, con), con),
    sql_optimise(sql_build(op$y, con), con),
    op$vars,
    type = op$type,
    by = op$by,
    suffix = op$suffix,
    na_matches = op$na_matches
  )
}

#' @export
sql_build.lazy_semi_join_query <- function(op, con, ...) {
  vars <- op$vars
  vars_prev <- op_vars(op$x)
  if (identical(unname(vars), names(vars)) &&
      identical(unname(vars), vars_prev)) {
    vars <- sql("*")
  } else {
    vars <- ident(vars)
  }

  semi_join_query(
    sql_optimise(sql_build(op$x, con), con),
    sql_optimise(sql_build(op$y, con), con),
    vars = vars,
    anti = op$anti,
    by = op$by,
    na_matches = op$na_matches
  )
}
