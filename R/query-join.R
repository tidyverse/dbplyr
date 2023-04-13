#' @export
#' @rdname sql_build
join_query <- function(x,
                       y,
                       vars,
                       type = "inner",
                       by = NULL,
                       suffix = c(".x", ".y"),
                       na_matches = FALSE) {
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

multi_join_query <- function(x, joins, table_vars, vars) {
  structure(
    list(
      x = x,
      joins = joins,
      table_vars = table_vars,
      vars = vars
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
    cat_line(indent(paste0(x$joins$by[[i]]$x, "-", x$joins$by[[i]]$y)))

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
sql_render.multi_join_query <- function(query,
                                        con = NULL,
                                        ...,
                                        subquery = FALSE,
                                        lvl = 0) {
  x <- sql_render(query$x, con, ..., subquery = TRUE, lvl = lvl + 1)
  query$joins$table <- purrr::map(
    query$joins$table,
    function(table) sql_render(table, con, ..., subquery = TRUE, lvl = lvl + 1)
  )

  sql_query_multi_join(
    con = con,
    x = x,
    joins = query$joins,
    table_vars = query$table_vars,
    by_list = query$by_list,
    vars = query$vars,
    lvl = lvl
  )
}

# SQL generation ----------------------------------------------------------

#' @param vars tibble with three columns:
#'   * `name` `<character>`: variable name in output.
#'   * `table` `<list_of<integer>>`: tables index in database.
#'   * `var` `<list_of<character>>`: variable names in database.
#'     If more than one, need to coalesce in output.
#' @param table_names `<character>`: table names indexed by `table`.
#' @param all_vars_list `<list_of<character>>` All variables in each table.
#' @noRd
#' @examples
#' # Left join with *
#' vars <- tibble(
#'   name = c("y", "a", "b", "c"),
#'   table = list(1, 1, 1, 2),
#'   var = list("y", "a", "b", "c")
#' )
#' sql_multi_join_vars(
#'    simulate_dbi(),
#'    vars,
#'    c("one", "two"),
#'    list(c("y", "a", "b"), c("y", "c"))
#' )
#'
#' # Left join with duplicated names
#' vars <- tibble(
#'   name = c("y", "a.y", "a.x"),
#'   table = list(1, 2, 1),
#'   var = list("x", "a", "a")
#' )
#' sql_multi_join_vars(
#'    simulate_dbi(),
#'    vars,
#'    c("one", "two"),
#'    list(c("x", "a"), c("x", "a"))
#' )
#'
#' # Full and right join are handled via `sql_rf_join_vars`
sql_multi_join_vars <- function(con, vars, table_vars) {
  all_vars <- tolower(unlist(table_vars))
  duplicated_vars <- all_vars[vctrs::vec_duplicate_detect(all_vars)]
  duplicated_vars <- unique(duplicated_vars)
  table_names <- names(table_vars)

  # FIXME vectorise `sql_table_prefix()` (need to update `ident()` and friends for this...)
  out <- rep_named(vars$name, list())

  for (i in seq_along(table_names)) {
    all_vars_current <- table_vars[[i]]
    vars_idx <- which(vars$table == i)
    used_vars_current <- vars$var[vars_idx]
    out_vars_current <- vars$name[vars_idx]

    if (join_can_use_star(all_vars_current, used_vars_current, out_vars_current, vars_idx)) {
      id <- vars_idx[[1]]
      tbl_alias <- escape(ident(table_names[i]), con = con)
      out[[id]] <- sql(paste0(tbl_alias, ".*"))
      names(out)[id] <- ""
    } else {
      out[vars_idx] <- purrr::map2(
        used_vars_current, i,
        ~ sql_multi_join_var(con, .x, .y, table_names, duplicated_vars)
      )

    }
  }

  sql(unlist(out))
}

join_can_use_star <- function(all_vars, used_vars, out_vars, idx) {
  # using `tbl.*` for a single variable is silly
  if (length(all_vars) <= 1) {
    return(FALSE)
  }

  # all variables need to be used in same order
  if (!identical(used_vars, all_vars)) {
    return(FALSE)
  }

  # they must not be renamed
  if (!identical(used_vars, out_vars)) {
    return(FALSE)
  }

  # the variables must form a sequence
  all(diff(idx) == 1)
}

sql_multi_join_var <- function(con, var, table_id, table_names, duplicated_vars) {
  if (length(table_id) > 1) {
    # TODO use `vec_check_size()` after vctrs 0.6 release
    cli_abort("{.arg table_id} must have size 1", .internal = TRUE)
  }

  if (tolower(var) %in% duplicated_vars) {
    sql_table_prefix(con, var, ident(table_names[[table_id]]))
  } else {
    sql_escape_ident(con, var)
  }
}

sql_rf_join_vars <- function(con, type, vars, x_as = "LHS", y_as = "RHS") {
  type <- arg_match0(type, c("right", "full"))
  table_names <- c(unclass(x_as), unclass(y_as))

  if (type == "full") {
    duplicated_vars <- intersect(vars$all_x, vars$all_y)
    out <- purrr::map2(
      vars$x, vars$y,
      ~ {
        if (!is.na(.x) && !is.na(.y)) {
          out <- sql_expr(
            COALESCE(
              !!sql_table_prefix(con, .x, table = x_as),
              !!sql_table_prefix(con, .y, table = y_as)
            ),
            con = con
          )

          return(out)
        }

        if (!is.na(.x)) {
          sql_multi_join_var(con, .x, 1L, table_names, duplicated_vars)
        } else {
          sql_multi_join_var(con, .y, 2L, table_names, duplicated_vars)
        }
      }
    )

    out <- set_names(out, vars$name)
    return(sql(unlist(out)))
  }

  multi_join_vars <- purrr::map2_dfr(
    vars$x, vars$y,
    ~ {
      if (!is.na(.x)) {
        table <- 1L
        var <- .x
      } else {
        table <- 2L
        var <- .y
      }

      vctrs::new_data_frame(list(table = table, var = var), n = 1L)
    }
  )

  multi_join_vars <- vctrs::vec_cbind(name = vars$name, multi_join_vars)
  table_vars <- set_names(vars[c("all_x", "all_y")], table_names)

  sql_multi_join_vars(
    con = con,
    vars = multi_join_vars,
    table_vars = table_vars
  )
}

sql_join_tbls <- function(con, by, na_matches) {
  na_matches <- arg_match(na_matches, c("na", "never"))

  if (na_matches == "na" || length(by$x) + length(by$y) > 0) {
    lhs <- sql_table_prefix(con, by$x, by$x_as %||% ident("LHS"))
    rhs <- sql_table_prefix(con, by$y, by$y_as %||% ident("RHS"))

    if (na_matches == "na") {
      compare <- purrr::map_chr(seq_along(lhs), function(i) {
        sql_expr_matches(sql(lhs[[i]]), sql(rhs[[i]]), con = con)
      })
    } else {
      by$condition[by$condition == "=="] <- "="
      compare <- paste0(lhs, " ", by$condition, " ", rhs)
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
