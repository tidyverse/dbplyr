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
#' db |>
#'   mutate(z = x + y * 2) |>
#'   pull()
pull.tbl_sql <- function(.data, var = -1, name = NULL, ...) {
  vars <- tbl_vars(.data)
  var <- tidyselect::vars_pull(vars, !!enquo(var), error_arg = "var")
  name_quo <- enquo(name)
  if (!quo_is_null(name_quo)) {
    name <- tidyselect::vars_pull(vars, !!name_quo, error_arg = "name")
  }

  .data <- ungroup(.data)
  .data <- select(.data, all_of(c(var, name)))

  .data <- collect(.data)
  out <- .data[[var]]
  if (!is.null(name)) {
    out <- set_names(out, nm = .data[[name]])
  }

  out
}
