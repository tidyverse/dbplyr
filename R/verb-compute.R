#' Compute results of a query
#'
#' These are methods for the dplyr generics [collapse()], [compute()],
#' and [collect()]. `collapse()` creates a subquery, `compute()` stores
#' the results in a remote table, and `collect()` executes the query and
#' downloads the data into R.
#'
#' @export
#' @param x A lazy data frame backed by a database query.
#' @importFrom dplyr collapse
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' db <- memdb_frame(a = c(3, 4, 1, 2), b = c(5, 1, 2, NA))
#' db %>% filter(a <= 2) %>% collect()
collapse.tbl_sql <- function(x, ...) {
  sql <- db_sql_render(x$src$con, x)

  tbl_src_dbi(x$src, sql, colnames(x)) %>%
    group_by(!!! syms(op_grps(x))) %>%
    arrange.tbl_lazy(!!!op_sort(x))
}

# compute -----------------------------------------------------------------

#' @rdname collapse.tbl_sql
#' @param name Table name in remote database.
#' @param temporary Should the table be temporary (`TRUE`, the default) or
#'   persistent (`FALSE`)?
#' @inheritParams copy_to.src_sql
#' @inheritParams collect.tbl_sql
#' @export
#' @importFrom dplyr compute
compute.tbl_sql <- function(x,
                            name = NULL,
                            temporary = TRUE,
                            overwrite = FALSE,
                            unique_indexes = list(),
                            indexes = list(),
                            analyze = TRUE,
                            ...,
                            cte = FALSE) {
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

  name <- as_table_path(name, x$src$con)
  vars <- op_vars(x)

  compute_check_indexes(x, indexes)
  compute_check_indexes(x, unique_indexes)

  x_aliased <- select(x, !!! syms(vars)) # avoids problems with SQLite quoting (#1754)
  sql <- db_sql_render(x$src$con, x_aliased$lazy_query, cte = cte)

  name <- db_compute(x$src$con, name, sql,
    temporary = temporary,
    overwrite = overwrite,
    unique_indexes = unique_indexes,
    indexes = indexes,
    analyze = analyze,
    ...
  )

  tbl_src_dbi(x$src, name, colnames(x)) %>%
    group_by(!!!syms(op_grps(x))) %>%
    window_order(!!!op_sort(x))
}

compute_check_indexes <- function(x,
                                  indexes,
                                  ...,
                                  arg = caller_arg(indexes),
                                  error_call = caller_env()) {
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

# collect -----------------------------------------------------------------

#' @rdname collapse.tbl_sql
#' @param n Number of rows to fetch. Defaults to `Inf`, meaning all rows.
#' @param warn_incomplete Warn if `n` is less than the number of result rows?
#' @param cte `r lifecycle::badge("experimental")`
#'   Use common table expressions in the generated SQL?
#' @importFrom dplyr collect
#' @export
collect.tbl_sql <- function(x, ..., n = Inf, warn_incomplete = TRUE, cte = FALSE) {
  if (identical(n, Inf)) {
    n <- -1
  } else {
    # Gives the query planner information that it might be able to take
    # advantage of
    x <- head(x, n)
  }

  sql <- db_sql_render(x$src$con, x, cte = cte)
  withCallingHandlers(
    out <- db_collect(x$src$con, sql, n = n, warn_incomplete = warn_incomplete, ...),
    error = function(cnd) {
      cli_abort("Failed to collect lazy table.", parent = cnd)
    }
  )
  dplyr::grouped_df(out, intersect(op_grps(x), names(out)))
}
