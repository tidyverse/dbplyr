#' Save results into a new remote table
#'
#' `compute()` executes the query and stores the results in a new remote table.
#' This is useful when you want to cache intermediate results for reuse or to
#' improve performance by avoiding repeated computation of complex queries.
#'
#' @inheritParams collect.tbl_sql
#' @inheritParams copy_to.src_sql
#' @export
#' @importFrom dplyr compute
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' db <- memdb_frame(a = c(3, 4, 1, 2), b = c(5, 1, 2, NA))
#' db |> filter(a <= 2) |> show_query()
#' db |> filter(a <= 2) |> compute() |> show_query()
compute.tbl_sql <- function(
  x,
  name = NULL,
  temporary = TRUE,
  overwrite = FALSE,
  unique_indexes = list(),
  indexes = list(),
  analyze = TRUE,
  ...,
  cte = FALSE
) {
  check_bool(temporary)

  if (is.null(name)) {
    if (!temporary) {
      lifecycle::deprecate_warn(
        "2.3.3",
        what = "compute(name = 'must be provided when `temporary = FALSE`')"
      )
    }
    name <- unique_table_name()
  }

  name <- as_table_path(name, x$con)
  vars <- op_vars(x)

  compute_check_indexes(x, indexes)
  compute_check_indexes(x, unique_indexes)

  x_aliased <- select(x, !!!syms(vars)) # avoids problems with SQLite quoting (#1754)
  sql <- db_sql_render(x$con, x_aliased$lazy_query, cte = cte)

  name <- db_compute(
    x$con,
    name,
    sql,
    temporary = temporary,
    overwrite = overwrite,
    unique_indexes = unique_indexes,
    indexes = indexes,
    analyze = analyze,
    ...
  )

  new_tbl_sql(x$con, name, vars = colnames(x)) |>
    group_by(!!!syms(op_grps(x))) |>
    window_order(!!!op_sort(x))
}

compute_check_indexes <- function(
  x,
  indexes,
  ...,
  arg = caller_arg(indexes),
  error_call = caller_env()
) {
  if (is.null(indexes)) {
    return()
  }

  check_dots_empty()
  force(arg)
  if (!is.list(indexes) && !is.character(indexes)) {
    stop_input_type(
      indexes,
      c("a character vector", "a list of characters"),
      arg = arg,
      call = error_call
    )
  }
  if (is.list(indexes)) {
    indexes <- unique(unlist(indexes))
  }

  x_nms <- colnames(x)

  missing <- setdiff(indexes, x_nms)
  if (!is_empty(missing)) {
    message <- c(
      "All columns specified through {.arg {arg}} must exist in {.arg x}.",
      i = "The following columns are missing from {.arg {arg}}: {.field {missing}}."
    )

    cli_abort(message, call = error_call)
  }
}
