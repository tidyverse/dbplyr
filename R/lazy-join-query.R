#' @export
#' @rdname sql_build
lazy_multi_join_query <- function(x,
                                  joins,
                                  table_names,
                                  vars,
                                  group_vars = op_grps(x),
                                  order_vars = op_sort(x),
                                  frame = op_frame(x),
                                  call = caller_env()) {
  stopifnot(inherits(x, "lazy_query"))

  if (!identical(colnames(joins), c("table", "type", "by_x_table_id", "by"))) {
    cli_abort("`joins` must have fields `table`, `type`, `by_x_table_id`, `by`", .internal = TRUE)
  }
  vctrs::vec_assert(joins$type, character(), arg = "joins$type", call = caller_env())

  if (!identical(colnames(table_names), c("name", "from"))) {
    cli_abort("`table_names` must have fields `name`, `from`", .internal = TRUE)
  }
  vctrs::vec_assert(table_names$name, character(), arg = "table_names$as", call = caller_env())
  vctrs::vec_assert(table_names$from, character(), arg = "table_names$from", call = caller_env())

  if (!identical(colnames(vars), c("name", "table", "var"))) {
    cli_abort("`vars` must have fields `name`, `table`, `var`", .internal = TRUE)
  }
  vctrs::vec_assert(vars$name, character(), arg = "vars$name", call = caller_env())
  vctrs::vec_assert(vars$table, list(), arg = "vars$table", call = caller_env())
  vctrs::vec_assert(vars$var, list(), arg = "vars$var", call = caller_env())

  lazy_query(
    query_type = "multi_join",
    x = x,
    joins = joins,
    table_names = table_names,
    vars = vars,
    group_vars = group_vars,
    order_vars = order_vars,
    frame = frame
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
                                 group_vars = op_grps(x),
                                 order_vars = op_sort(x),
                                 frame = op_frame(x),
                                 call = caller_env()) {
  stopifnot(inherits(x, "lazy_query"))
  stopifnot(inherits(y, "lazy_query"))
  assert_flag(anti, "anti", call = call)
  # join_check_by(by, call)
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
    frame = frame
  )
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
op_vars.lazy_multi_join_query <- function(op) {
  op$vars$name
}
#' @export
op_vars.lazy_semi_join_query <- function(op) {
  op$vars$name
}

#' @export
sql_build.lazy_multi_join_query <- function(op, con, ...) {
  table_names_out <- generate_join_table_names(op$table_names)

  if (length(table_names_out) > 2 || !op$joins$type %in% c("full", "right")) {
    table_vars <- purrr::map(
      set_names(c(list(op$x), op$joins$table), table_names_out),
      op_vars
    )

    op$joins$table <- purrr::map(op$joins$table, ~ sql_optimise(sql_build(.x, con), con))
    op$joins$by <- purrr::map2(
      op$joins$by, seq_along(op$joins$by),
      function(by, i) {
        by$x_as <- ident(table_names_out[op$joins$by_x_table_id[[i]]])
        by$y_as <- ident(table_names_out[i + 1L])
        by
      }
    )

    out <- multi_join_query(
      x = sql_optimise(sql_build(op$x, con), con),
      joins = op$joins,
      table_vars = table_vars,
      vars = op$vars
    )

    return(out)
  }

  # construct a classical `join_query()` so that special handling of
  # full and right join continue to work
  type <- op$joins$type
  x_idx <- purrr::map_lgl(op$vars$table, ~ 1L %in% .x)
  vars_x <- purrr::map2_chr(op$vars$var, x_idx, ~ {if (.y) .x[[1]] else NA_character_})

  y_idx <- purrr::map_lgl(op$vars$table, ~ 2L %in% .x)
  vars_y <- purrr::map2_chr(op$vars$var, y_idx, ~ {if (.y) dplyr::last(.x) %||% .x[[1]] else NA_character_})

  vars_classic <- list(
    alias = op$vars$name,
    x = vars_x,
    y = vars_y,
    all_x = op_vars(op$x),
    all_y = op_vars(op$joins$table[[1]])
  )

  by <- op$joins$by[[1]]
  by$x_as <- ident(table_names_out[[1]])
  by$y_as <- ident(table_names_out[[2]])

  join_query(
    sql_optimise(sql_build(op$x, con), con),
    sql_optimise(sql_build(op$joins$table[[1]], con), con),
    vars = vars_classic,
    type = type,
    by = by,
    suffix = NULL, # it seems like the suffix is not used for rendering
    na_matches = by$na_matches
  )
}

generate_join_table_names <- function(table_names) {
  if (length(table_names$name) != 2) {
    table_names_repaired <- vctrs::vec_as_names(table_names$name, repair = "unique", quiet = TRUE)
    auto_name <- table_names$from != "as"
    table_names$name[auto_name] <- table_names_repaired[auto_name]

    return(table_names$name)
  }

  join_two_table_alias(table_names$name, table_names$from)
}

#' @export
sql_build.lazy_semi_join_query <- function(op, con, ...) {
  vars_prev <- op_vars(op$x)
  if (identical(op$vars$var, op$vars$name) &&
      identical(op$vars$var, vars_prev)) {
    vars <- sql("*")
  } else {
    vars <- ident(set_names(op$vars$var, op$vars$name))
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
