# R prefers to specify start / stop or start / end
# databases usually specify start / length
# https://www.postgresql.org/docs/current/functions-string.html
sql_substr <- function(x, start, stop) {
  start <- as.integer(start)
  length <- pmax(as.integer(stop) - start + 1L, 0L)

  sql_expr(SUBSTR(!!x, !!start, !!length))
}
