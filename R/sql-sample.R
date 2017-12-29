#' @export
#' @importFrom dplyr sample_n
sample_n.tbl_sql <- function(tbl,
                             size,
                             replace = FALSE,
                             weight = NULL,
                             .env = parent.frame()) {

    add_op_single("sample_n", .data = tbl, args = list(
    size = size,
    replace = replace,
    weight = weight,
    .env = .env
  ))
}

#' @export
#' @importFrom dplyr sample_frac
sample_frac.tbl_sql <- function(tbl,
                                  size = 1,
                                  replace = FALSE,
                                  weight = NULL,
                                  .env = parent.frame()) {

  add_op_single("sample_frac", .data = tbl, args = list(
    size = size,
    replace = replace,
    weight = weight,
    .env = .env
  ))
}
