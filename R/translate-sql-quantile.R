sql_quantile <- function(f,
                         style = c("infix", "ordered"),
                         window = FALSE) {
  force(f)
  style <- match.arg(style)
  force(window)

  function(x, probs) {
    check_probs(probs)

    sql <- switch(style,
      infix = sql_call2(f, x, probs),
      ordered = build_sql(
        sql_call2(f, probs), " WITHIN GROUP (ORDER BY ", x, ")"
      )
    )

    if (window) {
      sql <- win_over(sql,
        partition = win_current_group(),
        frame = win_current_frame()
      )
    }
    sql
  }
}

sql_median <- function(f,
                       style = c("infix", "ordered"),
                       window = FALSE) {

  warned <- FALSE
  quantile <- sql_quantile(f, style = style, window = window)
  function(x, na.rm = FALSE) {
    warned <<- check_na_rm("median", na.rm, warned)
    quantile(x, 0.5)
  }
}

check_probs <- function(probs) {
  if (!is.numeric(probs)) {
    stop("`probs` must be numeric", call. = FALSE)
  }

  if (length(probs) > 1) {
    stop("SQL translation only supports single value for `probs`.", call. = FALSE)
  }
}
