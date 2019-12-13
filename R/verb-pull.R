#' @importFrom dplyr pull
#' @export
pull.tbl_sql <- function(.data, var = -1) {
  expr <- enquo(var)
  var <- dplyr:::find_var(expr, tbl_vars(.data))

  .data <- ungroup(.data)
  .data <- select(.data, !! sym(var))
  .data <- collect(.data)
  .data[[1]]
}

