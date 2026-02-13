#' A temporary in-memory database
#'
#' @description
#' `memdb()` creates a temporary in-memory database that disappears when the
#' R session ends. It's a convenient way to learn about and experiment with
#' dbplyr without having to connect to a "real" database.
#'
#' `memdb_frame()` works like [tibble::tibble()], but instead of creating a new
#' data frame in R, it creates a table in `memdb()`. `local_memdb_frame()`
#' is like `memdb_frame()` but the table will be automatically deleted when
#' the current scope ends. It's useful for tests. But beware: this function
#' will overwrite an existing table of the same name.
#'
#' @inheritParams tibble::tibble
#' @param .name Name of table in database: defaults to a random name that's
#'   unlikely to conflict with an existing table.
#' @export
#' @examples
#' library(dplyr)
#'
#' # use memdb_frame() to create a new database table
#' df <- memdb_frame(x = runif(100), y = runif(100))
#' df |> arrange(x)
#' df |> arrange(x) |> show_query()
#'
#' # Use memdb() + copy_to() to copy an existing data frame
#' iris_db <- copy_to(memdb(), iris)
#' iris_db
memdb <- function() {
  cache_computation("memdb", DBI::dbConnect(RSQLite::SQLite(), ":memory:"))
}

#' @rdname memdb
#' @export
memdb_frame <- function(.name = unique_table_name(), ...) {
  if (is.data.frame(.name)) {
    lifecycle::deprecate_warn(
      "2.6.0",
      I("memdb_frame(data.frame(...))"),
      details = "Use `copy_to(memdb(), df)` instead."
    )
    df <- .name
    .name <- unique_table_name()
  } else {
    check_string(.name)
    df <- tibble::tibble(...)
  }

  copy_to(memdb(), df, name = .name)
}

#' @rdname memdb
#' @param frame The created table is bound to this execution frame and will
#'   be deleted when it ends. For expert use only.
#' @export
local_memdb_frame <- function(
  .name = unique_table_name(),
  ...,
  frame = caller_env()
) {
  tbl <- copy_to(memdb(), tibble(...), .name, overwrite = TRUE)
  withr::defer(DBI::dbRemoveTable(memdb(), .name), envir = frame)
  tbl
}

#' Deprecated
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' * `src_memdb()` is deprecated; use `memdb()` instead.
#' * `tbl_memdb(df)` is deprecated; use `copy_to(memdb(), df)` instead.
#'
#' @param df Data frame to copy.
#' @param name Name of table in database.
#' @export
#' @keywords internal
src_memdb <- function() {
  lifecycle::deprecate_warn("2.6.0", "src_memdb()", "memdb()")
  src_dbi(memdb())
}

#' @rdname src_memdb
#' @export
tbl_memdb <- function(df, name = deparse(substitute(df))) {
  lifecycle::deprecate_warn(
    "2.6.0",
    "tbl_memdb()",
    details = "Use `copy_to(memdb(), df)` instead"
  )
  copy_to(memdb(), df, name = name)
}
