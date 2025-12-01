#' SQL helpers for scalar functions
#'
#' These functions help you create custom scalar SQL translations when
#' implementing a new backend. They are typically used within [sql_translator()]
#' to define how R functions should be translated to SQL.
#'
#' @param f The name of the SQL function as a string.
#' @param pad If `TRUE`, the default, pad the infix operator with spaces.
#' @param n For `sql_prefix()`, an optional number of arguments to expect.
#'   Will signal error if not correct.
#' @param type SQL type name as a string.
#' @param rand_expr A SQL expression that generates random numbers.
#' @param min,max Range of random values.
#'
#' @section Helper functions:
#'
#' * `sql_infix()` creates SQL infix operators like `+`, `-`, `*`, `/`.
#' * `sql_prefix()` creates SQL prefix functions like `ABS()`, `SQRT()`.
#' * `sql_cast()` creates SQL cast expressions like `CAST(x AS type)`.
#' * `sql_try_cast()` creates SQL try_cast expressions (for safe casting).
#' * `sql_log()` creates a SQL logarithm function with optional base.
#' * `sql_cot()` creates a SQL cotangent function (as `1 / TAN(x)`).
#' * `sql_runif()` creates a SQL expression for generating uniform random
#'   numbers.
#'
#' @seealso
#' * [sql_translator()] for creating SQL translators.
#' * [sql_translation_string] for string manipulation helpers.
#' * [sql_translation_agg] for aggregation function helpers.
#'
#' @name sql_translation_scalar
NULL

#' @rdname sql_translation_scalar
#' @export
sql_infix <- function(f, pad = TRUE) {
  # Unquoting involving infix operators easily create abstract syntax trees
  # without parentheses where they are needed for printing and translation.
  # For example `expr(!!expr(2 - 1) * x))`
  #
  # See https://adv-r.hadley.nz/quasiquotation.html#non-standard-ast
  # for more information.
  #
  # This is fixed with `escape_infix_expr()`
  # see https://github.com/tidyverse/dbplyr/issues/634
  check_string(f)
  check_bool(pad)
  f <- sql(f)

  function(x, y) {
    x <- escape_infix_expr(enexpr(x), x)
    y <- escape_infix_expr(enexpr(y), y)

    if (is.null(x)) {
      if (pad) {
        sql <- "{f} {.val y}"
      } else {
        sql <- "{f}{.val y}"
      }
    } else {
      if (pad) {
        sql <- "{.val x} {f} {.val y}"
      } else {
        sql <- "{.val x}{f}{.val y}"
      }
    }
    glue_sql2(sql_current_con(), sql)
  }
}

escape_infix_expr <- function(xq, x, escape_unary_minus = FALSE) {
  infix_calls <- c("+", "-", "*", "/", "%%", "^")
  is_infix <- is_call(xq, infix_calls, n = 2)
  is_unary_minus <- escape_unary_minus &&
    is_call(xq, "-", n = 1) &&
    !is_atomic(x, n = 1)

  if (is_infix || is_unary_minus) {
    enpared <- glue_sql2(sql_current_con(), "({.val x})")
    return(enpared)
  }

  x
}

#' @rdname sql_translation_scalar
#' @export
sql_prefix <- function(f, n = NULL) {
  check_string(f)

  function(...) {
    args <- list(...)
    if (!is.null(n) && length(args) != n) {
      cli_abort(
        "Invalid number of args to SQL function {f}",
        i = "Expecting {n} and got {length(args)}"
      )
    }
    if (any(names2(args) != "")) {
      cli::cli_warn("Named arguments ignored for SQL {f}")
    }
    glue_sql2(sql_current_con(), "{f}({.val args*})")
  }
}

#' @rdname sql_translation_scalar
#' @export
sql_cast <- function(type) {
  type <- sql(type)
  function(x) {
    sql_expr(cast(!!x %as% !!type))
  }
}

#' @rdname sql_translation_scalar
#' @export
sql_try_cast <- function(type) {
  type <- sql(type)
  function(x) {
    sql_expr(try_cast(!!x %as% !!type))
    # try_cast available in MSSQL 2012+
  }
}

#' @rdname sql_translation_scalar
#' @export
sql_log <- function() {
  function(x, base = exp(1)) {
    if (isTRUE(all.equal(base, exp(1)))) {
      sql_expr(ln(!!x))
    } else {
      sql_expr(log(!!x) / log(!!base))
    }
  }
}


#' @rdname sql_translation_scalar
#' @export
sql_cot <- function() {
  function(x) {
    sql_expr(1L / tan(!!x))
  }
}

#' @rdname sql_translation_scalar
#' @export
sql_runif <- function(rand_expr, n = n(), min = 0, max = 1) {
  n_expr <- quo_get_expr(enquo(n))
  if (!is_call(n_expr, "n", n = 0)) {
    cli_abort("Only {.code n = n()} is supported.")
  }

  rand_expr <- enexpr(rand_expr)
  range <- max - min
  if (range != 1) {
    rand_expr <- expr(!!rand_expr * !!range)
  }

  if (min != 0) {
    rand_expr <- expr(!!rand_expr + !!min)
  }

  sql_expr(!!rand_expr)
}

utils::globalVariables(c("%as%", "cast", "ln", "try_cast"))
