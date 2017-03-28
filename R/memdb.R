#' Create a database table in temporary in-memory database.
#'
#' `memdb_frame()` works like [tibble::tibble()], but instead of creating a new
#' data frame in R, it creates a table in [src_memdb()].
#'
#' @inheritParams tibble::data_frame
#' @param .name Name of table in database: defaults to a random name that's
#'   unlikely to conflict with an existing table.
#' @export
#' @examples
#' library(dplyr)
#' df <- memdb_frame(x = runif(100), y = runif(100))
#' df %>% arrange(x)
#' df %>% arrange(x) %>% show_query()
memdb_frame <- function(..., .name = random_table_name()) {
  x <- copy_to(src_memdb(), data_frame(...), name = .name)
  x
}

#' @rdname memdb_frame
#' @export
src_memdb <- function() {
  cache_computation("src_memdb", src_sqlite(":memory:", TRUE))
}
