join_check_by <- function(by, call) {
  if (!vctrs::vec_is_list(by) && !inherits(by, "dplyr_join_by")) {
    cli_abort("{.arg by} must be a list", .internal = TRUE)
  }
  check_character(by$x, call = call)
  check_character(by$y, call = call)
  if (vctrs::vec_size(by$x) != vctrs::vec_size(by$y)) {
    cli_abort(
      "{.arg by$x} and {.arg by$y} must have the same size",
      .internal = TRUE
    )
  }
  check_string(by$x_as, arg = "by$x_as", call = call)
  check_string(by$y_as, arg = "by$y_as", call = call)
}

#' @export
#' @rdname sql_build
lazy_semi_join_query <- function(
  x,
  y,
  vars,
  anti,
  by,
  where,
  group_vars = op_grps(x),
  order_vars = op_sort(x),
  frame = op_frame(x),
  call = caller_env()
) {
  check_lazy_query(x, call = call)
  check_lazy_query(y, call = call)
  check_bool(anti, call = call)
  join_check_by(by, call)

  lazy_query(
    query_type = "semi_join",
    x = x,
    y = y,
    anti = anti,
    by = by,
    vars = vars,
    where = where,
    group_vars = group_vars,
    order_vars = order_vars,
    frame = frame
  )
}

#' @export
print.lazy_semi_join_query <- function(x, ...) {
  cat_line("<SQL ", if (x$anti) "ANTI" else "SEMI", " JOIN>")

  cat_line("By:")
  cat_line(indent(paste0(x$by$x, "-", x$by$y)))

  if (length(x$where)) {
    cat_line("Where:")
    where <- purrr::map_chr(x$where, as_label)
    cat_line(indent(paste0(where)))
  }

  cat_line("X:")
  cat_line(indent_print(sql_build(x$x, simulate_dbi())))

  cat_line("Y:")
  cat_line(indent_print(sql_build(x$y, simulate_dbi())))
}

#' @export
op_vars.lazy_semi_join_query <- function(op) {
  op$vars$name
}

#' @export
sql_build.lazy_semi_join_query <- function(op, con, ..., sql_options = NULL) {
  vars_prev <- op_vars(op$x)
  if (
    sql_options$use_star &&
      identical(op$vars$var, op$vars$name) &&
      identical(op$vars$var, vars_prev)
  ) {
    vars <- sql_star(con, op$by$x_as)
  } else {
    vars <- ident(set_names(op$vars$var, op$vars$name))
  }

  # We've introduced aliases to disambiguate the internal and external tables
  # so need to update the existing WHERE clause
  y_vars <- op_vars(op$y)
  y_as <- op$by$y_as
  replacements <- lapply(y_vars, \(var) sql_glue2(con, "{y_as}.{.id var}"))
  where <- replace_sym(op$where, y_vars, replacements)
  where_sql <- translate_sql_(
    where,
    con = con,
    context = list(clause = "WHERE")
  )

  semi_join_query(
    sql_build(op$x, con, sql_options = sql_options),
    sql_build(op$y, con, sql_options = sql_options),
    vars = vars,
    anti = op$anti,
    by = op$by,
    where = where_sql,
    na_matches = op$by$na_matches
  )
}
