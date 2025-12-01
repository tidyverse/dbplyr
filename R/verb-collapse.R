#' Collapse a query into a subquery
#'
#' `collapse()` forces computation of a lazy query by wrapping it in a subquery.
#' This is not generally needed, but can be useful if you need to work around
#' database/dbplyr limitations.
#'
#' @export
#' @inheritParams collect.tbl_sql
#' @importFrom dplyr collapse
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' db <- memdb_frame(a = c(3, 4, 1, 2), b = c(5, 1, 2, NA))
#' db |> filter(a <= 2) |> show_query()
#' db |> filter(a <= 2) |> collapse() |> show_query()
collapse.tbl_sql <- function(x, ...) {
  sql <- db_sql_render(x$src$con, x)

  tbl_src_dbi(x$src, sql, colnames(x)) |>
    group_by(!!!syms(op_grps(x))) |>
    arrange.tbl_lazy(!!!op_sort(x))
}
