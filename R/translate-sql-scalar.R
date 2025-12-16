#' SQL helpers for scalar functions
#'
#' @description
#' These functions help you create custom scalar SQL translations when
#' implementing a new backend. They are typically used within [sql_translator()]
#' to define how R functions should be translated to SQL.
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
#' @param f The name of the SQL function as a string.
#' @family SQL translation helpers
#' @name sql_translation_scalar
NULL

#' @rdname sql_translation_scalar
#' @param pad If `TRUE`, the default, pad the infix operator with spaces.
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

  function(x, y) {
    x <- escape_infix_expr(enexpr(x), x)
    y <- escape_infix_expr(enexpr(y), y)

    if (is.null(x)) {
      if (pad) {
        sql <- "{.sql f} {y}"
      } else {
        sql <- "{.sql f}{y}"
      }
    } else {
      if (pad) {
        sql <- "{x} {.sql f} {y}"
      } else {
        sql <- "{x}{.sql f}{y}"
      }
    }
    sql_glue(sql)
  }
}

escape_infix_expr <- function(xq, x, escape_unary_minus = FALSE) {
  infix_calls <- c("+", "-", "*", "/", "%%", "^")
  is_infix <- is_call(xq, infix_calls, n = 2)
  is_unary_minus <- escape_unary_minus &&
    is_call(xq, "-", n = 1) &&
    !is_atomic(x, n = 1)

  if (is_infix || is_unary_minus) {
    sql(paste0("(", x, ")"))
  } else {
    x
  }
}

#' @rdname sql_translation_scalar
#' @param n For `sql_prefix()`, an optional number of arguments to expect.
#'   Will signal error if not correct.
#' @export
sql_prefix <- function(f, n = NULL) {
  check_string(f)

  function(...) {
    args <- list(...)
    if (!is.null(n) && ...length() != n) {
      cli_abort(
        "Invalid number of args to SQL function {f}",
        i = "Expecting {n} and got {length(args)}"
      )
    }
    if (any(names2(args) != "")) {
      cli::cli_warn("Named arguments ignored for SQL {f}")
    }
    sql_glue("{.sql f}({...})")
  }
}

#' @rdname sql_translation_scalar
#' @param type SQL type name as a string.
#' @export
sql_cast <- function(type) {
  function(x) {
    sql_glue("CAST({x} AS {.sql type})")
  }
}

#' @rdname sql_translation_scalar
#' @export
sql_try_cast <- function(type) {
  function(x) {
    sql_glue("TRY_CAST({x} AS {.sql type})")
    # try_cast available in MSSQL 2012+
  }
}

#' @rdname sql_translation_scalar
#' @export
sql_log <- function() {
  function(x, base = exp(1)) {
    if (isTRUE(all.equal(base, exp(1)))) {
      sql_glue("LN({x})")
    } else {
      sql_glue("LOG({x}) / LOG({base})")
    }
  }
}


#' @rdname sql_translation_scalar
#' @export
sql_cot <- function() {
  function(x) {
    sql_glue("1 / TAN({x})")
  }
}

#' @rdname sql_translation_scalar
#' @param rand_expr An string giving an SQL expression that generates a
#'   random number between 0 and 1, e.g. `"RANDOM()"`.
#' @param min,max Range of random values.
#' @export
sql_runif <- function(rand_expr, n = n(), min = 0, max = 1) {
  rand_expr <- enexpr(rand_expr)
  if (!is_string(rand_expr)) {
    # for backward compatibility
    rand_expr <- sql_expr(!!rand_expr)
  }

  n_expr <- quo_get_expr(enquo(n))
  if (!is_call(n_expr, "n", n = 0)) {
    cli_abort("Only {.code n = n()} is supported.")
  }

  range <- max - min
  if (range != 1) {
    rand_expr <- sql_glue("{.sql rand_expr} * {range}")
  }

  if (min != 0) {
    rand_expr <- sql_glue("{.sql rand_expr} + {min}")
  }

  sql(rand_expr)
}
