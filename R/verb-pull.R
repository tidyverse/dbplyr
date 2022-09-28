#' Extract a single column
#'
#' This is a method for the dplyr [pull()] generic. It evaluates the query
#' retrieving just the specified column.
#'
#' @inheritParams arrange.tbl_lazy
#' @inheritParams dplyr::pull
#' @return A vector of data.
#' @importFrom dplyr pull
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' db <- memdb_frame(x = 1:5, y = 5:1)
#' db %>%
#'   mutate(z = x + y * 2) %>%
#'   pull()
pull.tbl_sql <- function(.data, var = -1) {
  vars <- tbl_vars(.data)
  if (length(vars) > 1 || !missing(var)) {
    var <- tidyselect::vars_pull(vars, {{ var }})
    .data <- ungroup(.data)
    .data <- select(.data, !! sym(var))
  }

  .data <- collect(.data)
  .data[[1]]
}

