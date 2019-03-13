# R prefers to specify start / stop or start / end
# databases usually specify start / length
# https://www.postgresql.org/docs/current/functions-string.html
#' @export
#' @rdname sql_variant
sql_substr <- function(f = "SUBSTR") {
  function(x, start, stop) {
    start <- as.integer(start)
    length <- pmax(as.integer(stop) - start + 1L, 0L)

    sql_call2(f, x, start, length)
  }
}

# if no parameters are provided, str_sub returns the whole string
# if just start is provided, str_sub returns the rest of the string (after start)
#' @export
#' @rdname sql_variant
sql_str_sub <- function(f = "SUBSTR", full_length = "drop", compute_method = "LENGTH") {
  function(string, start = 1L, end = -1L) {
    start <- as.integer(start)
    if (end == -1L && full_length == "drop") {
      # drop the length parameter for full_length
      sql_call2(f, string, start)
    } else if (end == -1L && full_length == "compute") {
      # compute the full_length using compute_method
      sql_call2(f, string, start, build_sql(!!compute_method, "(", string, ") - ", start, " + 1"))
    } else {
      length <- pmax(as.integer(end) - start + 1L, 0L)
      sql_call2(f, string, start, length)
    }
  }
}

sql_str_trim <- function(string, side = c("both", "left", "right")) {
  side <- match.arg(side)
  switch(side,
    left = sql_expr(LTRIM(!!string)),
    right = sql_expr(RTRIM(!!string)),
    both = sql_expr(LTRIM(RTRIM(!!string))),
  )
}
