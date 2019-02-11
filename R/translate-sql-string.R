# R prefers to specify start / stop or start / end
# databases usually specify start / length
# https://www.postgresql.org/docs/current/functions-string.html
#' @export
#' @rdname sql_variant
sql_substr <- function(f = "SUBSTR") {
  function(x, start, end) {
    start <- as.integer(start)
    length <- pmax(as.integer(stop) - start + 1L, 0L)

    sql_call2(f, x, start, length)
  }
}
#' @export
#' @rdname sql_variant
sql_str_sub <- function(f = "SUBSTR") {
  function(string, start = 1L, end = -1L) {
    start <- as.integer(start)
    if (end == -1L) {
      return(sql_call2(f, string, start))
    } else {
      length <- pmax(as.integer(stop) - start + 1L, 0L)
      return(sql_call2(f, string, start, length))
    }
  }
}
