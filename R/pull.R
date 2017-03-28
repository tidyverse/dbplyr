#' @export
pull.tbl_sql <- function(.data, var = -1) {
  var <- dplyr:::find_var(var, tbl_vars(.data))

  .data <- select(.data, !! sym(var))
  .data <- collect(.data)
  .data[[1]]
}
