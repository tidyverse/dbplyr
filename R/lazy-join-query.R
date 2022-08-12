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

#' @export
#' @rdname sql_build
lazy_multi_join_query <- function(x,
                                  joins,
                                  table_names,
                                  vars,
                                  group_vars = NULL,
                                  order_vars = NULL,
                                  frame = NULL,
                                  call = caller_env()) {
  stopifnot(inherits(x, "lazy_query"))

  if (!identical(colnames(joins), c("table", "type", "by_x", "by_y", "on", "na_matches"))) {
    # TODO use `cli_abort()` after https://github.com/r-lib/rlang/issues/1386
    # is fixed
    abort("`joins` must have fields `table`, `type`, `by_x`, `by_y`, `on`, `na_matches`", .internal = TRUE)
    vctrs::vec_assert(joins$type, character(), arg = "joins$type", call = caller_env())
    vctrs::vec_assert(joins$on, character(), arg = "joins$on", call = caller_env())
    vctrs::vec_assert(joins$na_matches, character(), arg = "joins$na_matches", call = caller_env())
  }
  if (!identical(colnames(table_names), c("as", "name"))) {
    # TODO use `cli_abort()` after https://github.com/r-lib/rlang/issues/1386
    # is fixed
    abort("`table_names` must have fields `as`, `name`", .internal = TRUE)
    vctrs::vec_assert(table_names$as, character(), arg = "table_names$as", call = caller_env())
    vctrs::vec_assert(table_names$name, character(), arg = "table_names$as", call = caller_env())
  }
  if (!identical(colnames(vars), c("name", "table", "var"))) {
    # TODO use `cli_abort()` after https://github.com/r-lib/rlang/issues/1386
    # is fixed
    abort("`vars` must have fields `name`, `table`, `var`", .internal = TRUE)
    vctrs::vec_assert(vars$name, character(), arg = "vars$name", call = caller_env())
    vctrs::vec_assert(vars$table, integer(), arg = "vars$table", call = caller_env())
    vctrs::vec_assert(vars$var, character(), arg = "vars$var", call = caller_env())
  }

  lazy_query(
    query_type = "multi_join",
    x = x,
    joins = joins,
    table_names = table_names,
    vars = vars,
    last_op = "join",
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
op_vars.lazy_multi_join_query <- function(op) {
  op$vars$name
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
sql_build.lazy_multi_join_query <- function(op, con, ...) {
  auto_name <- is.na(op$table_names$as)
  table_names_out <- dplyr::coalesce(op$table_names$as, op$table_names$name)
  if (length(table_names_out) == 2) {
    table_names_out <- join_simple_table_alias(op$table_names$name, op$table_names$as)

    if (op$joins$type %in% c("full", "right")) {
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

      out <- join_query(
        sql_optimise(sql_build(op$x, con), con),
        sql_optimise(sql_build(op$joins$table[[1]], con), con),
        vars = vars_classic,
        type = type,
        by = list(
          on = op$joins$on,
          x = op$joins$by_x[[1]],
          y = op$joins$by_y[[1]],
          x_as = ident(table_names_out[[1]]),
          y_as = ident(table_names_out[[2]])
        ),
        suffix = NULL, # it seems like the suffix is not used for rendering
        na_matches = op$joins$na_matches
      )
      return(out)
    }
  } else {
    table_names_repaired <- vctrs::vec_as_names(table_names_out, repair = "unique", quiet = TRUE)
    table_names_out[auto_name] <- table_names_repaired[auto_name]
  }

  all_vars_list <- purrr::map(
    c(list(op$x), op$joins$table),
    op_vars
  )

  all_vars <- tolower(unlist(all_vars_list))
  duplicated_vars <- all_vars[vctrs::vec_duplicate_detect(all_vars)]
  duplicated_vars <- unique(duplicated_vars)

  op$joins$table <- purrr::map(op$joins$table, ~ sql_optimise(sql_build(.x, con), con))

  multi_join_query(
    x = sql_optimise(sql_build(op$x, con), con),
    joins = op$joins,
    table_names = table_names_out,
    vars = op$vars,
    all_vars_list = all_vars_list,
    duplicated_vars = duplicated_vars
  )
}

join_vars_classic <- function(x_names, y_names, type, by, suffix = c(".x", ".y")) {
  y_names_org <- y_names

  # Remove join keys from y
  y_names <- setdiff(y_names, by$y)


  # Add suffix where needed
  # suffix <- check_suffix(suffix, call)
  x_new <- add_suffixes(x_names, y_names, suffix$x)
  y_new <- add_suffixes(y_names, x_names, suffix$y)

  # In left and inner joins, return key values only from x
  # In right joins, return key values only from y
  # In full joins, return key values by coalescing values from x and y
  x_x <- x_names
  x_y <- by$y[match(x_names, by$x)]
  x_y[type == "left" | type == "inner"] <- NA
  x_x[type == "right" & !is.na(x_y)] <- NA
  y_x <- rep_len(NA, length(y_names))
  y_y <- y_names

  # Return a list with 3 parallel vectors
  # At each position, values in the 3 vectors represent
  #  alias - name of column in join result
  #  x - name of column from left table or NA if only from right table
  #  y - name of column from right table or NA if only from left table
  list(
    alias = c(x_new, y_new),
    x = c(x_x, y_x),
    y = c(x_y, y_y),
    all_x = x_names,
    all_y = y_names_org
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
