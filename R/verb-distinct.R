#' Subset distinct/unique rows
#'
#' This is a method for the dplyr [distinct()] generic. It adds the
#' `DISTINCT` clause to the SQL query.
#'
#' @inheritParams arrange.tbl_lazy
#' @inheritParams dplyr::distinct
#' @inherit arrange.tbl_lazy return
#' @export
#' @importFrom dplyr distinct
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' db <- memdb_frame(x = c(1, 1, 2, 2), y = c(1, 2, 1, 1))
#' db %>% distinct() %>% show_query()
#' db %>% distinct(x) %>% show_query()
distinct.tbl_lazy <- function(.data, ..., .keep_all = FALSE) {
  if (dots_n(...) > 0) {
    if (.keep_all) {
      stop(
        "Can only find distinct value of specified columns if .keep_all is FALSE",
        call. = FALSE
      )
    }

    .data <- transmute(.data, !!!syms(op_grps(.data)), ...)
  }

  .data$lazy_query <- add_distinct(.data)
  .data
}

add_distinct <- function(.data) {
  lazy_query <- .data$lazy_query

  lazy_select_query(
    from = lazy_query,
    last_op = "distinct",
    select = syms(set_names(op_vars(lazy_query))),
    distinct = TRUE
  )
}
