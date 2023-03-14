sql_nth <- function(x,
                    n,
                    order_by = NULL,
                    error_call = caller_env()) {
  check_number_whole_inf(n, call = error_call)

  frame <- win_current_frame()
  if (n == 1) {
    sql_expr <- sql_expr(FIRST_VALUE(!!x))
  } else if (is.infinite(n) && n > 0) {
    sql_expr <- sql_expr(LAST_VALUE(!!x))
    frame <- frame %||% c(-Inf, Inf)
  } else {
    sql_expr <- sql_expr(NTH_VALUE(!!x, !!as.integer(n)))
  }

  win_over(
    sql_expr,
    win_current_group(),
    order_by %||% win_current_order(),
    frame
  )
}

sql_quantile <- function(f,
                         style = c("infix", "ordered"),
                         window = FALSE) {
  force(f)
  style <- match.arg(style)
  force(window)

  function(x, probs, na.rm = FALSE) {
    check_probs(probs)
    check_na_rm(na.rm)

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
  quantile <- sql_quantile(f, style = style, window = window)
  function(x, na.rm = FALSE) {
    quantile(x, 0.5, na.rm = na.rm)
  }
}

check_probs <- function(probs, call = caller_env()) {
  # TODO min, max? Inf? NA?
  check_number_decimal(probs, call = call)

  if (length(probs) > 1) {
    cli_abort("SQL translation only supports single value for {.arg probs}.")
  }
}
