#' @export
#' @rdname sql_build
rf_join_query <- function(
  x,
  y,
  select,
  ...,
  type = "inner",
  by = NULL,
  suffix = c(".x", ".y"),
  na_matches = FALSE
) {
  structure(
    list(
      x = x,
      y = y,
      select = select,
      type = type,
      by = by,
      na_matches = na_matches
    ),
    class = c("rf_join_query", "query")
  )
}

#' @export
print.rf_join_query <- function(x, ...) {
  cat_line(sql_render(x, simulate_dbi()))
}

#' @export
sql_render.rf_join_query <- function(
  query,
  con = NULL,
  ...,
  sql_options = NULL,
  subquery = FALSE,
  lvl = 0
) {
  from_x <- sql_render(query$x, con, ..., subquery = TRUE, lvl = lvl + 1)
  from_y <- sql_render(query$y, con, ..., subquery = TRUE, lvl = lvl + 1)

  dbplyr_query_join(
    con,
    from_x,
    from_y,
    vars = query$vars,
    type = query$type,
    by = query$by,
    na_matches = query$na_matches,
    select = query$select,
    lvl = lvl
  )
}

#' @export
flatten_query.rf_join_query <- flatten_query_2_tables

sql_rf_join_vars <- function(
  con,
  type,
  vars,
  x_as = "LHS",
  y_as = "RHS",
  use_star,
  qualify_all_columns
) {
  type <- arg_match0(type, c("right", "full"))

  check_table_path(x_as)
  check_table_path(y_as)
  table_names <- c(x_as, y_as)

  if (type == "full") {
    duplicated_vars <- intersect(tolower(vars$all_x), tolower(vars$all_y))
    out <- purrr::map2(
      vars$x,
      vars$y,
      ~ {
        if (!is.na(.x) && !is.na(.y)) {
          x_prefix <- sql_table_prefix(con, x_as, .x)
          y_prefix <- sql_table_prefix(con, y_as, .y)
          out <- sql_glue2(con, "COALESCE({x_prefix}, {y_prefix})")

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
    vars$x,
    vars$y,
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
    table_vars = table_vars,
    use_star = use_star,
    qualify_all_columns = qualify_all_columns
  )
}
