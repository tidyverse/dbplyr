#' Literal SQL escaping
#'
#' Use `sql()` to declare that a string is literal SQL and should be used
#' as is, without quoting.
#'
#' @param ... Character vectors that will be combined into a single SQL vector.
#' @param x Object to check if it is an sql object.
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' # sql() just adds a class
#' sql("x + 1")
#' is.sql(sql("x + 1"))
#'
#' # You can use it when you need to insert some literal SQL in a query
#' db <- memdb_frame(x = 1:3)
#' db |> mutate(y = sql("CAST(x as VARCHAR)"))
sql <- function(...) {
  x <- c_character(...)
  structure(x, class = c("sql", "character"))
}

#' @export
`[.sql` <- function(x, i) {
  sql(NextMethod())
}
#' @export
`[[.sql` <- function(x, i) {
  sql(NextMethod())
}

#' @rdname sql
#' @export
is.sql <- function(x) inherits(x, "sql")

#' @export
print.sql <- function(x, ...) cat(format(x, ...), sep = "\n")
#' @export
format.sql <- function(x, ...) {
  if (length(x) == 0) {
    paste0("<SQL> [empty]")
  } else {
    paste0("<SQL> ", x, ifelse(names2(x) == "", "", paste0(" AS ", names2(x))))
  }
}
