#' @importFrom dplyr pull
#' @export
pull.tbl_sql <- function(.data, var = -1) {
  expr <- enquo(var)
  var <- tidyselect::vars_pull(tbl_vars(.data), {{ var }})

  .data <- ungroup(.data)
  .data <- select(.data, !! sym(var))
  .data <- collect(.data)
  .data[[1]]
}

