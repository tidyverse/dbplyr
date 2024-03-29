#' Generate SQL expression for window functions
#'
#' `win_over()` makes it easy to generate the window function specification.
#' `win_absent()`, `win_rank()`, `win_aggregate()`, and `win_cumulative()`
#' provide helpers for constructing common types of window functions.
#' `win_current_group()` and `win_current_order()` allow you to access
#' the grouping and order context set up by [group_by()] and [arrange()].
#'
#' @param expr The window expression
#' @param partition Variables to partition over
#' @param order Variables to order by
#' @param frame A numeric vector of length two defining the frame.
#' @param f The name of an sql function as a string
#' @param empty_order A logical value indicating whether to order by NULL if `order` is not specified
#' @export
#' @keywords internal
#' @examples
#' con <- simulate_dbi()
#'
#' win_over(sql("avg(x)"), con = con)
#' win_over(sql("avg(x)"), "y", con = con)
#' win_over(sql("avg(x)"), order = "y", con = con)
#' win_over(sql("avg(x)"), order = c("x", "y"), con = con)
#' win_over(sql("avg(x)"), frame = c(-Inf, 0), order = "y", con = con)
win_over <- function(expr,
                     partition = NULL,
                     order = NULL,
                     frame = NULL,
                     con = sql_current_con()) {
  if (length(partition) > 0) {
    partition <- as.sql(partition, con = con)
    partition <- glue_sql2(con, "PARTITION BY {.val partition*}")
  }

  if (length(order) > 0) {
    order <- as.sql(order, con = con)
    order <- glue_sql2(con, "ORDER BY {.val order*}")
  }
  if (length(frame) > 0) {
    if (length(order) == 0) {
      cli::cli_warn(c(
        "Windowed expression `{expr}` does not have explicit order.",
        i = "Please use {.fun arrange} or {.fun window_order} to make deterministic."
      ))
    }

    if (is.numeric(frame)) frame <- rows(frame[1], frame[2])
    frame <- glue_sql2(con, "ROWS {frame}")
  }

  over <- sql_vector(
    purrr::compact(list(partition, order, frame)),
    parens = TRUE,
    con = con
  )

  if (sql_context$register_windows) {
    win_register(over)
  } else {
    over <- win_get(over, con)
  }
  glue_sql2(con, "{.val expr} OVER {.val over}")
}

win_register_activate <- function() {
  sql_context$register_windows <- TRUE
}

win_register_deactivate <- function() {
  sql_context$register_windows <- FALSE
}

win_register <- function(over) {
  sql_context$windows <- append(sql_context$windows, over)
}

win_register_names <- function() {
  windows <- sql_context$windows %||% character()

  window_count <- vctrs::vec_count(windows, sort = "location")
  window_count <- vctrs::vec_slice(window_count, window_count$count > 1)
  if (nrow(window_count) > 0) {
    window_count$name <- ident(paste0("win", seq_along(window_count$key)))
  } else {
    window_count$name <- ident()
  }
  window_count$key <- window_count$key

  sql_context$window_names <- window_count
  window_count
}

win_get <- function(over, con) {
  windows <- sql_context$window_names

  if (vctrs::vec_in(over, windows$key)) {
    id <- vctrs::vec_match(over, windows$key)
    ident(windows$name[[id]])
  } else {
    over
  }
}

win_reset <- function() {
  sql_context$window_names <- NULL
  sql_context$windows <- list()
}

rows <- function(from = -Inf, to = 0) {
  if (from >= to) cli_abort("{.arg from} ({from}) must be less than {.arg to} ({to})")

  dir <- function(x) if (x < 0) "PRECEDING" else "FOLLOWING"
  val <- function(x) if (is.finite(x)) as.integer(abs(x)) else "UNBOUNDED"
  bound <- function(x) {
    if (x == 0) return("CURRENT ROW")
    paste(val(x), dir(x))
  }

  if (to == 0) {
    sql(bound(from))
  } else {
    glue_sql2(sql_current_con(), "BETWEEN {bound(from)} AND {bound(to)}")
  }
}

#' @rdname win_over
#' @export
win_rank <- function(f, empty_order = FALSE) {
  force(f)
  check_bool(empty_order)

  function(order = NULL) {
    group <- win_current_group()
    order <- unwrap_order_expr({{ order }}, f = f)
    con <- sql_current_con()

    if (!is_null(order)) {
      order_over <- translate_sql_(order, con = con)

      order_symbols <- purrr::map_if(order, ~ quo_is_call(.x, "desc", n = 1L), ~ call_args(.x)[[1L]])

      is_na_exprs <- purrr::map(order_symbols, ~ expr(is.na(!!.x)))
      any_na_expr <- purrr::reduce(is_na_exprs, ~ call2("|", .x, .y))

      cond <- translate_sql((case_when(!!any_na_expr ~ 1L, TRUE ~ 0L)), con = con)
      group <- sql(group, cond)

      not_is_na_exprs <- purrr::map(order_symbols, ~ expr(!is.na(!!.x)))
      no_na_expr <- purrr::reduce(not_is_na_exprs, ~ call2("&", .x, .y))
    } else {
      order_over <- win_current_order()
      if (empty_order & is_empty(order_over)) {
        # For certain backends (e.g., Snowflake), need a subquery that returns
        # a constant to work if no ordering is specified
        # https://stackoverflow.com/questions/44105691/row-number-without-order-by
        order_over <- sql("(SELECT NULL)")
      }
    }

    rank_sql <- win_over(
      sql(glue("{f}()")),
      partition = group,
      order = order_over,
      frame = win_current_frame()
    )

    if (is_null(order)) {
      rank_sql
    } else {
      translate_sql(case_when(!!no_na_expr ~ !!rank_sql), con = con)
    }
  }
}

#' @rdname win_over
#' @export
win_aggregate <- function(f) {
  force(f)
  function(x, na.rm = FALSE) {
    check_na_rm(na.rm)
    frame <- win_current_frame()

    win_over(
      glue_sql2(sql_current_con(), "{f}({.val x})"),
      partition = win_current_group(),
      order = if (!is.null(frame)) win_current_order(),
      frame = frame
    )
  }
}

#' @rdname win_over
#' @export
win_aggregate_2 <- function(f) {

  function(x, y) {
    frame <- win_current_frame()

    win_over(
      glue_sql2(sql_current_con(), "{f}({.val x}, {.val y})"),
      partition = win_current_group(),
      order = if (!is.null(frame)) win_current_order(),
      frame = frame
    )
  }
}

#' @rdname win_over
#' @usage NULL
#' @export
win_recycled <- win_aggregate


#' @rdname win_over
#' @export
win_cumulative <- function(f) {
  force(f)
  function(x, order = NULL) {
    win_over(
      glue_sql2(sql_current_con(), "{f}({.val x})"),
      partition = win_current_group(),
      order = order %||% win_current_order(),
      frame = c(-Inf, 0)
    )
  }
}

sql_nth <- function(x,
                    n,
                    order_by = NULL,
                    na_rm = FALSE,
                    ignore_nulls = c("inside", "outside", "bool"),
                    error_call = caller_env()) {
  check_bool(na_rm, call = error_call)
  ignore_nulls <- arg_match(ignore_nulls, error_call = error_call)
  con <- sql_current_con()

  frame <- win_current_frame()
  args <- translate_sql(!!x, con = con)
  if (n == 1) {
    sql_f <- "FIRST_VALUE"
  } else if (is.infinite(n) && n > 0) {
    sql_f <- "LAST_VALUE"
    frame <- frame %||% c(-Inf, Inf)
  } else {
    sql_f <- "NTH_VALUE"
    if (is.numeric(n)) {
      n <- as.integer(n)
    }
    args <- glue_sql2(con, "{args}, {.val n}")
  }

  if (na_rm) {
    if (ignore_nulls == "inside") {
      sql_expr <- "{sql_f}({args} IGNORE NULLS)"
    } else if (ignore_nulls == "outside") {
      sql_expr <- "{sql_f}({args}) IGNORE NULLS"
    } else {
      sql_expr <- "{sql_f}({args}, TRUE)"
    }
  } else {
    sql_expr <- "{sql_f}({args})"
  }

  win_over(
    glue_sql2(con, sql_expr),
    win_current_group(),
    order_by %||% win_current_order(),
    frame
  )
}

#' @rdname win_over
#' @export
win_absent <- function(f) {
  force(f)

  function(...) {
    cli_abort("Window function {.fun {f}} is not supported by this database.")
  }
}


# API to set default partitioning etc -------------------------------------

# Use a global variable to communicate state of partitioning between
# tbl and sql translator. This isn't the most amazing design, but it keeps
# things loosely coupled and is easy to understand.
sql_context <- new.env(parent = emptyenv())
sql_context$group_by <- NULL
sql_context$order_by <- NULL
sql_context$con <- NULL
# Used to carry additional information needed for special cases
sql_context$context <- list()

sql_context$register_windows <- FALSE
sql_context$windows <- NULL
sql_context$window_names <- NULL


set_current_con <- function(con) {
  old <- sql_context$con
  sql_context$con <- con
  invisible(old)
}

local_con <- function(con, env = parent.frame()) {
  old <- set_current_con(con)
  withr::defer(set_current_con(old), envir = env)
  invisible()
}

set_win_current_group <- function(vars) {
  check_character(vars, allow_null = TRUE)

  old <- sql_context$group_by
  sql_context$group_by <- vars
  invisible(old)
}

set_win_current_order <- function(vars) {
  check_character(vars, allow_null = TRUE)

  old <- sql_context$order_by
  sql_context$order_by <- vars
  invisible(old)
}

set_win_current_frame <- function(frame) {
  check_frame_range(frame)

  old <- sql_context$frame
  sql_context$frame <- frame
  invisible(old)
}
#' @export
#' @rdname win_over
win_current_group <- function() sql_context$group_by

#' @export
#' @rdname win_over
win_current_order <- function() sql_context$order_by

#' @export
#' @rdname win_over
win_current_frame <- function() sql_context$frame

# Not exported, because you shouldn't need it
sql_current_con <- function() {
  sql_context$con
}

# Functions to manage information for special cases
set_current_context <- function(context) {
  old <- sql_context$context
  sql_context$context <- context
  invisible(old)
}

sql_current_context <- function() sql_context$context

local_context <- function(x, env = parent.frame()) {
  old <- set_current_context(x)
  withr::defer(set_current_context(old), envir = env)
  invisible()
}

# Where translation -------------------------------------------------------

uses_window_fun <- function(x, con, lq) {
  check_list(x)

  calls <- unlist(lapply(x, all_calls))
  win_f <- ls(envir = dbplyr_sql_translation(con)$window)
  any(calls %in% win_f)
}

is_aggregating <- function(x, non_group_cols, agg_f) {
  if (is_quosure(x)) {
    x <- quo_get_expr(x)
  }

  if (is_symbol(x)) {
    xc <- as_name(x)
    return(!(xc %in% non_group_cols))
  }

  if (is_call(x)) {
    fname <- as.character(x[[1]])
    if (fname %in% agg_f) {
      return(TRUE)
    }

    return(all(purrr::map_lgl(x[-1], is_aggregating, non_group_cols, agg_f)))
  }

  return(TRUE)
}

common_window_funs <- function() {
  ls(dbplyr_sql_translation(NULL)$window) # nocov
}

#' @noRd
#' @examples
#' translate_window_where(quote(1))
#' translate_window_where(quote(x))
#' translate_window_where(quote(x == 1))
#' translate_window_where(quote(x == 1 && y == 2))
#' translate_window_where(quote(n() > 10))
#' translate_window_where(quote(rank() > cumsum(AB)))
translate_window_where <- function(expr, window_funs = common_window_funs()) {
  switch(typeof(expr),
    logical = ,
    integer = ,
    double = ,
    complex = ,
    character = ,
    string = ,
    symbol = window_where(expr, list()),
    language = {
      if (is_formula(expr)) {
        translate_window_where(f_rhs(expr), window_funs)
      } else if (is_call(expr, name = window_funs)) {
        name <- unique_column_name()
        window_where(sym(name), set_names(list(expr), name))
      } else {
        args <- lapply(expr[-1], translate_window_where, window_funs = window_funs)
        expr <- call2(node_car(expr), splice(lapply(args, "[[", "expr")))

        window_where(
          expr = expr,
          comp = unlist(lapply(args, "[[", "comp"), recursive = FALSE)
        )

      }
    },
    cli_abort("Unknown type: {typeof(expr)}") # nocov
  )
}


#' @noRd
#' @examples
#' translate_window_where_all(list(quote(x == 1), quote(n() > 2)))
#' translate_window_where_all(list(quote(cumsum(x) == 10), quote(n() > 2)))
translate_window_where_all <- function(x, window_funs = common_window_funs()) {
  out <- lapply(x, translate_window_where, window_funs = window_funs)

  list(
    expr = unlist(lapply(out, "[[", "expr"), recursive = FALSE),
    comp = unlist(lapply(out, "[[", "comp"), recursive = FALSE)
  )
}

window_where <- function(expr, comp) {
  stopifnot(is.call(expr) || is.name(expr) || is.atomic(expr) || is.null(expr))
  check_list(comp)

  list(
    expr = expr,
    comp = comp
  )
}
