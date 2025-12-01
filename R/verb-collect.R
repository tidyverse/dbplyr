#' Collect results into a local data frame
#'
#' `collect()` executes the query and retrieves the results into a local tibble.
#' This brings all the data from the database into R's memory, which is useful
#' once you've done as much as possible in the database, and now need to use
#' R functions.
#'
#' @param x A lazy data frame backed by a database query.
#' @param ... Ignored.
#' @param n Number of rows to fetch. Defaults to `Inf`, meaning all rows.
#' @param warn_incomplete Warn if `n` is less than the number of result rows?
#' @param cte `r lifecycle::badge("experimental")`
#'   Use common table expressions in the generated SQL?
#' @importFrom dplyr collect
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' db <- memdb_frame(a = c(3, 4, 1, 2), b = c(5, 1, 2, NA))
#' db |> filter(a <= 2) |> collect()
collect.tbl_sql <- function(
  x,
  ...,
  n = Inf,
  warn_incomplete = TRUE,
  cte = FALSE
) {
  if (identical(n, Inf)) {
    n <- -1
  } else {
    # Gives the query planner information that it might be able to take
    # advantage of
    x <- head(x, n)
  }

  sql <- db_sql_render(x$src$con, x, cte = cte)
  withCallingHandlers(
    out <- db_collect(
      x$src$con,
      sql,
      n = n,
      warn_incomplete = warn_incomplete,
      ...
    ),
    error = function(cnd) {
      cli_abort("Failed to collect lazy table.", parent = cnd)
    }
  )
  dplyr::grouped_df(out, intersect(op_grps(x), names(out)))
}
