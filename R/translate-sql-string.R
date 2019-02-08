sql_substr <- function(x, start, stop) {
  start <- as.integer(start)
  length <- pmax(as.integer(stop) - start + 1L, 0L)

  sql_expr(SUBSTR(!!x, !!start, !!length))
}
