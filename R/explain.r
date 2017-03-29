#' Show query
#'
#' @param x A [tbl_dbi]
#' @export
#' @examples
#' library(dplyr)
#'
#' lahman_s <- lahman_sqlite()
#' batting <- tbl(lahman_s, "Batting")
#' batting %>% show_query()
show_query <- function(x) {
  con <- con_acquire(x$src)
  on.exit(con_release(x$src, con), add = TRUE)

  message("<SQL>\n", sql_render(x, con = con))

  invisible(x)
}

#' @export
explain.tbl_sql <- function(x, ...) {
  force(x)
  show_query(x)

  con <- con_acquire(x$src)
  on.exit(con_release(x$src, con), add = TRUE)

  message("\n")
  message("<PLAN>\n", db_explain(con, sql_render(x, con = con)))

  invisible(x)
}

