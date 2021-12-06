#' @export
#' @rdname sql_build
lazy_join_query <- function(x, y, vars, type = "inner", by = NULL, suffix = c(".x", ".y"), na_matches = FALSE) {
  structure(
    list(
      x = x,
      y = y,
      vars = vars,
      type = type,
      by = by,
      suffix = suffix,
      na_matches = na_matches,
      last_op = "join"
    ),
    class = c("lazy_join_query", "lazy_query")
  )
}

#' @export
#' @rdname sql_build
lazy_semi_join_query <- function(x, y, anti = FALSE, by = NULL, na_matches = FALSE) {
  structure(
    list(
      x = x,
      y = y,
      anti = anti,
      by = by,
      na_matches = na_matches,
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
  op_vars(op$x)
}

#' @export
op_grps.lazy_join_query <- function(op) op_grps(op$x)
#' @export
op_grps.lazy_semi_join_query <- function(op) op_grps(op$x)

#' @export
sql_build.lazy_join_query <- function(op, con, ...) {
  join_query(
    sql_build(op$x, con), sql_build(op$y, con), op$vars,
    type = op$type,
    by = op$by,
    suffix = op$suffix,
    na_matches = op$na_matches
  )
}

#' @export
sql_build.lazy_semi_join_query <- function(op, con, ...) {
  semi_join_query(
    sql_build(op$x, con), sql_build(op$y, con),
    anti = op$anti,
    by = op$by,
    na_matches = op$na_matches
  )
}
