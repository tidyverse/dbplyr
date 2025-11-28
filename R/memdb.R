#' Create a database table in temporary in-memory database.
#'
#' `memdb_frame()` works like [tibble::tibble()], but instead of creating a new
#' data frame in R, it creates a table in [src_memdb()].
#'
#' @inheritParams tibble::tibble
#' @param name,.name Name of table in database: defaults to a random name that's
#'   unlikely to conflict with an existing table.
#' @param df Data frame to copy
#' @export
#' @examples
#' library(dplyr)
#' df <- memdb_frame(x = runif(100), y = runif(100))
#' df |> arrange(x)
#' df |> arrange(x) |> show_query()
#'
#' mtcars_db <- tbl_memdb(mtcars)
#' mtcars_db |> group_by(cyl) |> summarise(n = n()) |> show_query()
memdb_frame <- function(..., .name = unique_table_name()) {
  x <- copy_to(src_memdb(), tibble(...), name = .name)
  x
}

#' @rdname memdb_frame
#' @export
tbl_memdb <- function(df, name = deparse(substitute(df))) {
  copy_to(src_memdb(), df, name = name) # nocov
}

#' @rdname memdb_frame
#' @export
src_memdb <- function() {
  cache_computation("src_memdb", {
    src_dbi(DBI::dbConnect(RSQLite::SQLite(), ":memory:", create = TRUE))
  })
}
