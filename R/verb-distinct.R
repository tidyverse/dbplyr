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
  grps <- syms(op_grps(.data))
  can_use_distinct <- !.keep_all || (dots_n(...) == 0 && is_empty(grps))
  if (can_use_distinct) {
    if (dots_n(...) > 0) {
      .data <- transmute(.data, !!!grps, ...)
    }

    .data$lazy_query <- add_distinct(.data)
    return(.data)
  }

  .data %>%
    group_by(..., .add = TRUE) %>%
    filter(row_number() == 1L) %>%
    group_by(!!!grps)
}

add_distinct <- function(.data) {
  lazy_query <- .data$lazy_query

  lazy_select_query(
    x = lazy_query,
    last_op = "distinct",
    distinct = TRUE
  )
}
