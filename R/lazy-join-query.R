#' @export
#' @rdname sql_build
lazy_join_query <- function(x,
                            y,
                            vars,
                            type,
                            by,
                            suffix = c(".x", ".y"),
                            na_matches = c("never", "na"),
                            group_vars = NULL,
                            order_vars = NULL,
                            frame = NULL,
                            call = caller_env()) {
  stopifnot(inherits(x, "lazy_query"))
  stopifnot(inherits(y, "lazy_query"))
  join_check_vars(vars, call = call)
  type <- arg_match(type, c("left", "right", "inner", "full", "cross"), error_call = call)
  join_check_by(by, call = call)
  vctrs::vec_assert(suffix, ptype = character(), size = 2L, arg = "suffix", call = call)
  na_matches <- arg_match(na_matches, c("never", "na"), error_call = call)

  lazy_query(
    query_type = "join",
    x = x,
    y = y,
    vars = vars,
    type = type,
    by = by,
    suffix = suffix,
    na_matches = na_matches,
    group_vars = group_vars,
    order_vars = order_vars,
    frame = frame,
    last_op = "join"
  )
}

join_check_vars <- function(vars, call) {
  if (!vctrs::vec_is_list(vars)) {
    # TODO use `cli_abort()` after https://github.com/r-lib/rlang/issues/1386
    # is fixed
    abort("`vars` must be a list", .internal = TRUE)
  }

  if (!identical(names(vars), c("alias", "x", "y", "all_x", "all_y"))) {
    # TODO use `cli_abort()` after https://github.com/r-lib/rlang/issues/1386
    # is fixed
    abort("`vars` must have fields `alias`, `x`, `y`, `all_x`, and `all_y`", .internal = TRUE)
  }

  n <- vctrs::vec_size(vars$alias)
  vctrs::vec_assert(vars$alias, character(), arg = "vars$alias", call = call)
  vctrs::vec_assert(vars$x, character(), size = n, arg = "vars$x", call = call)
  vctrs::vec_assert(vars$y, character(), size = n, arg = "vars$y", call = call)
  vctrs::vec_assert(vars$all_x, character(), arg = "vars$all_x", call = call)
  vctrs::vec_assert(vars$all_y, character(), arg = "vars$all_y", call = call)
}

join_check_by <- function(by, call) {
  if (!vctrs::vec_is_list(by)) {
    # TODO use `cli_abort()` after https://github.com/r-lib/rlang/issues/1386
    # is fixed
    abort("`by` must be a list", .internal = TRUE)
  }
  vctrs::vec_assert(by$x, character(), arg = "by$x", call = call)
  vctrs::vec_assert(by$y, character(), arg = "by$y", call = call)
  if (vctrs::vec_size(by$x) != vctrs::vec_size(by$y)) {
    # TODO use `cli_abort()` after https://github.com/r-lib/rlang/issues/1386
    # is fixed
    abort("`by$x` and `by$y` must have the same size", .internal = TRUE)
  }
  vctrs::vec_assert(by$x_as, ident(), size = 1L, arg = "by$x_as", call = call)
  vctrs::vec_assert(by$y_as, ident(), size = 1L, arg = "by$y_as", call = call)
}

#' @export
#' @rdname sql_build
lazy_semi_join_query <- function(x,
                                 y,
                                 vars,
                                 anti,
                                 by,
                                 na_matches = c("never", "na"),
                                 group_vars = NULL,
                                 order_vars = NULL,
                                 frame = NULL,
                                 call = caller_env()) {
  stopifnot(inherits(x, "lazy_query"))
  stopifnot(inherits(y, "lazy_query"))
  assert_flag(anti, "anti", call = call)
  join_check_by(by, call)
  na_matches <- arg_match(na_matches, c("never", "na"), error_call = call)

  lazy_query(
    query_type = "semi_join",
    x = x,
    y = y,
    anti = anti,
    by = by,
    na_matches = na_matches,
    vars = vars,
    group_vars = group_vars,
    order_vars = order_vars,
    frame = frame,
    last_op = "semi_join"
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
