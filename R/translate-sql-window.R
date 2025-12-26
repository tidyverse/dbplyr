#' SQL helpers for window functions
#'
#' @description
#' These functions help you create custom window SQL translations when
#' implementing a new backend. They are typically used within [sql_translator()]
#' to define how R window functions should be translated to SQL.
#'
#' * `win_over()` makes it easy to generate the window function specification.
#' * `win_absent()`, `win_rank()`, `win_aggregate()`, and `win_cumulative()`
#'   provide helpers for constructing common types of window functions.
#' * `win_current_group()` and `win_current_order()` allow you to access
#'   the grouping and order context set up by [group_by()] and [arrange()].
#'
#' @param expr The window expression.
#' @param partition Variables to partition over.
#' @param order Variables to order by.
#' @param frame A numeric vector of length two defining the frame.
#' @param f The name of an SQL function as a string.
#' @param empty_order A logical value indicating whether to order by NULL if
#'   `order` is not specified.
#' @param con Database connection.
#' @family SQL translation helpers
#' @name sql_translation_window
#' @export
#' @examples
#' con <- simulate_dbi()
#'
#' win_over(sql("avg(x)"), con = con)
#' win_over(sql("avg(x)"), "y", con = con)
#' win_over(sql("avg(x)"), order = "y", con = con)
#' win_over(sql("avg(x)"), order = c("x", "y"), con = con)
#' win_over(sql("avg(x)"), frame = c(-Inf, 0), order = "y", con = con)
win_over <- function(
  expr,
  partition = NULL,
  order = NULL,
  frame = NULL,
  con = sql_current_con()
) {
  if (length(partition) > 0) {
    partition <- as_ident_or_sql(partition)
    partition <- sql_glue2(con, "PARTITION BY {partition}")
  }

  if (length(order) > 0) {
    order <- as_ident_or_sql(order)
    order <- sql_glue2(con, "ORDER BY {order}")
  }
  if (length(frame) > 0) {
    if (length(order) == 0) {
      cli::cli_warn(c(
        "Windowed expression `{expr}` does not have explicit order.",
        i = "Please use {.fun arrange}, {.fun window_order}, or {.arg .order} to make deterministic."
      ))
    }

    if (is.numeric(frame)) {
      frame <- rows(frame[1], frame[2])
    }
    frame <- sql_glue2(con, "ROWS {.sql frame}")
  }

  over <- sql_collapse(c(partition, order, frame), parens = TRUE)

  if (sql_context$register_windows) {
    win_register(over)
  } else {
    over <- win_get(over, con)
  }

  sql_glue2(con, "{expr} OVER {over}")
}

win_register_activate <- function() {
  sql_context$register_windows <- TRUE
}

win_register_deactivate <- function() {
  sql_context$register_windows <- FALSE
}

win_register <- function(over) {
  sql_context$windows <- sql(c(sql_context$windows, over))
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
  sql_context$windows <- sql()
}

rows <- function(from = -Inf, to = 0) {
  if (from >= to) {
    cli_abort("{.arg from} ({from}) must be less than {.arg to} ({to})")
  }

  dir <- function(x) if (x < 0) "PRECEDING" else "FOLLOWING"
  val <- function(x) if (is.finite(x)) as.integer(abs(x)) else "UNBOUNDED"
  bound <- function(x) {
    if (x == 0) {
      sql("CURRENT ROW")
    } else {
      sql(paste(val(x), dir(x)))
    }
  }

  if (to == 0) {
    sql(bound(from))
  } else {
    sql_glue("BETWEEN {bound(from)} AND {bound(to)}")
  }
}

#' @rdname sql_translation_window
#' @export
win_rank <- function(f, empty_order = FALSE) {
  check_bool(empty_order)

  function(order = NULL) {
    group <- win_current_group()
    order <- unwrap_order_expr({{ order }}, f = f)
    con <- sql_current_con()

    if (!is_null(order)) {
      order_over <- translate_sql_(order, con = con)

      # Ensuring NULLs get a NULL rank in SQL requires two pieces:
      # * We need to ensure that a NULL gets a rank of NULL. We can't do this
      #   inside of RANK() so we use a CASE WHEN outside of it
      # * We need to ensure those NULLs don't affect the ranks of other
      #   values. We do this by PARTITIONING the NULLs into their own bucket
      #   (NULLS LAST is not yet widely supported)

      order_symbols <- purrr::map_if(
        order,
        \(x) quo_is_call(x, "desc", n = 1L),
        \(x) call_args(x)[[1L]]
      )

      is_na_exprs <- purrr::map(order_symbols, \(sym) expr(is.na(!!sym)))
      is_missing <- purrr::reduce(is_na_exprs, function(lhs, rhs) {
        call2("|", lhs, rhs)
      })
      # e.g. X IS NULL OR Y IS NULL
      not_missing <- call2("!", is_missing)

      cond <- translate_sql(
        (case_when(!!is_missing ~ 1L, TRUE ~ 0L)),
        con = con
      )
      group <- sql(group, cond)
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
      sql_glue("{.sql f}()"),
      partition = group,
      order = order_over,
      frame = win_current_frame()
    )

    if (is_null(order)) {
      rank_sql
    } else {
      translate_sql(case_when(!!not_missing ~ !!rank_sql), con = con)
    }
  }
}

#' @rdname sql_translation_window
#' @export
win_aggregate <- function(f) {
  function(x, na.rm = FALSE) {
    sql_check_na_rm(na.rm)
    frame <- win_current_frame()

    win_over(
      sql_glue("{.sql f}({x})"),
      partition = win_current_group(),
      order = if (!is.null(frame)) win_current_order(),
      frame = frame
    )
  }
}

#' @rdname sql_translation_window
#' @export
win_aggregate_2 <- function(f) {
  function(x, y) {
    frame <- win_current_frame()

    win_over(
      sql_glue("{.sql f}({x}, {y})"),
      partition = win_current_group(),
      order = if (!is.null(frame)) win_current_order(),
      frame = frame
    )
  }
}

#' @rdname sql_translation_window
#' @usage NULL
#' @export
win_recycled <- win_aggregate


#' @rdname sql_translation_window
#' @export
win_cumulative <- function(f) {
  function(x, order = NULL) {
    win_over(
      sql_glue("{.sql f}({x})"),
      partition = win_current_group(),
      order = order %||% win_current_order(),
      frame = c(-Inf, 0)
    )
  }
}

sql_nth <- function(
  x,
  n,
  order_by = NULL,
  na_rm = FALSE,
  ignore_nulls = c("inside", "outside", "bool"),
  error_call = caller_env()
) {
  check_bool(na_rm, call = error_call)
  ignore_nulls <- arg_match(ignore_nulls, error_call = error_call)
  con <- sql_current_con()

  frame <- win_current_frame()
  args <- translate_sql(!!x, con = con)
  if (n == 1) {
    f <- "FIRST_VALUE"
  } else if (is.infinite(n) && n > 0) {
    f <- "LAST_VALUE"
    frame <- frame %||% c(-Inf, Inf)
  } else {
    f <- "NTH_VALUE"
    if (is.numeric(n)) {
      n <- as.integer(n)
    }
    args <- sql_glue2(con, "{args}, {n}")
  }

  if (na_rm) {
    if (ignore_nulls == "inside") {
      sql_expr <- "{.sql f}({args} IGNORE NULLS)"
    } else if (ignore_nulls == "outside") {
      sql_expr <- "{.sql f}({args}) IGNORE NULLS"
    } else {
      sql_expr <- "{.sql f}({args}, TRUE)"
    }
  } else {
    sql_expr <- "{.sql f}({args})"
  }

  win_over(
    sql_glue2(con, sql_expr),
    win_current_group(),
    order_by %||% win_current_order(),
    frame
  )
}

#' @rdname sql_translation_window
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
#' @rdname sql_translation_window
win_current_group <- function() sql_context$group_by

#' @export
#' @rdname sql_translation_window
win_current_order <- function() sql_context$order_by

#' @export
#' @rdname sql_translation_window
win_current_frame <- function() sql_context$frame

# Not exported, because you shouldn't need it
sql_current_con <- function() {
  sql_context$con
}

local_con <- function(con, frame = caller_env()) {
  check_con(con)

  old <- sql_context$con
  withr::defer(sql_context$con <- old, envir = frame)

  sql_context$con <- con
  invisible(old)
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
  any(calls %in% window_funs(con))
}

window_funs <- function(con = simulate_dbi()) {
  env_names(sql_translation(con)$window)
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

#' @noRd
#' @examples
#' translate_window_where(quote(1))
#' translate_window_where(quote(x))
#' translate_window_where(quote(x == 1))
#' translate_window_where(quote(x == 1 && y == 2))
#' translate_window_where(quote(n() > 10))
#' translate_window_where(quote(rank() > cumsum(AB)))
translate_window_where <- function(expr, window_funs = NULL) {
  window_funs <- window_funs %||% window_funs()

  switch(
    typeof(expr),
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
        args <- lapply(
          expr[-1],
          translate_window_where,
          window_funs = window_funs
        )
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
translate_window_where_all <- function(x, window_funs = NULL) {
  window_funs <- window_funs %||% window_funs()
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
