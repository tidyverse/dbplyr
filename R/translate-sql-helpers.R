#' Create an sql translator
#'
#' When creating a package that maps to a new SQL based src, you'll often
#' want to provide some additional mappings from common R commands to the
#' commands that your tbl provides. These three functions make that
#' easy.
#'
#' @section Helper functions:
#'
#' `sql_infix()` and `sql_prefix()` create default SQL infix and prefix
#' functions given the name of the SQL function. They don't perform any input
#' checking, but do correctly escape their input, and are useful for
#' quickly providing default wrappers for a new SQL variant.
#'
#' @keywords internal
#' @seealso [win_over()] for helper functions for window functions.
#' @param scalar,aggregate,window The three families of functions than an
#'   SQL variant can supply.
#' @param ...,.funs named functions, used to add custom converters from standard
#'  R functions to sql functions. Specify individually in `...`, or
#'  provide a list of `.funs`
#' @param .parent the sql variant that this variant should inherit from.
#'   Defaults to `base_agg` which provides a standard set of
#'   mappings for the most common operators and functions.
#' @param f the name of the sql function as a string
#' @param f_r the name of the r function being translated as a string
#' @param n for `sql_infix()`, an optional number of arguments to expect.
#'   Will signal error if not correct.
#' @seealso [sql()] for an example of a more customised sql
#'   conversion function.
#' @export
#' @examples
#' # An example of adding some mappings for the statistical functions that
#' # postgresql provides: http://bit.ly/K5EdTn
#'
#' postgres_agg <- sql_translator(.parent = base_agg,
#'   cor = sql_aggregate_2("CORR"),
#'   cov = sql_aggregate_2("COVAR_SAMP"),
#'   sd =  sql_aggregate("STDDEV_SAMP", "sd"),
#'   var = sql_aggregate("VAR_SAMP", "var")
#' )
#'
#' # Next we have to simulate a connection that uses this variant
#' con <- simulate_dbi("TestCon")
#' sql_translation.TestCon <- function(x) {
#'   sql_variant(
#'     base_scalar,
#'     postgres_agg,
#'     base_no_win
#'   )
#' }
#'
#' translate_sql(cor(x, y), con = con, window = FALSE)
#' translate_sql(sd(income / years), con = con, window = FALSE)
#'
#' # Any functions not explicitly listed in the converter will be translated
#' # to sql as is, so you don't need to convert all functions.
#' translate_sql(regr_intercept(y, x), con = con)
sql_variant <- function(scalar = sql_translator(),
                        aggregate = sql_translator(),
                        window = sql_translator()) {
  check_environment(scalar)
  check_environment(aggregate)
  check_environment(window)

  # Need to check that every function in aggregate also occurs in window
  missing <- setdiff(ls(aggregate), ls(window))
  if (length(missing) > 0) {
    warn(paste0(
      "Translator is missing window variants of the following aggregate functions:\n",
      paste0("* ", missing, "\n", collapse = "")
    ))
  }

  aggregate_fns <- ls(envir = aggregate)

  # An ensure that every window function is flagged in aggregate context
  missing <- setdiff(ls(window), ls(aggregate))
  missing_funs <- lapply(missing, sql_aggregate_win)
  env_bind(aggregate, !!!set_names(missing_funs, missing))

  structure(
    list(scalar = scalar, aggregate = aggregate, window = window, aggregate_fns = aggregate_fns),
    class = "sql_variant"
  )
}

is.sql_variant <- function(x) inherits(x, "sql_variant")

#' @export
print.sql_variant <- function(x, ...) {
  wrap_ls <- function(x, ...) {
    vars <- sort(ls(envir = x))
    wrapped <- strwrap(paste0(vars, collapse = ", "), ...)
    if (identical(wrapped, "")) return()
    paste0(wrapped, "\n", collapse = "")
  }

  cat("<sql_variant>\n")
  cat(wrap_ls(
    x$scalar,
    prefix = "scalar:    "
  ))
  cat(wrap_ls(
    x$aggregate,
    prefix = "aggregate: "
  ))
  cat(wrap_ls(
    x$window,
    prefix = "window:    "
  ))
}

#' @export
names.sql_variant <- function(x) {
  c(ls(envir = x$scalar), ls(envir = x$aggregate), ls(envir = x$window))
}

#' @export
#' @rdname sql_variant
sql_translator <- function(...,
                           .funs = list(),
                           .parent = new.env(parent = emptyenv())) {
  funs <- c(list2(...), .funs)
  if (length(funs) == 0) return(.parent)

  if (anyDuplicated(names(funs))) {
    bullets <- unique(names(funs)[duplicated(names(funs))])
    cli_abort(c(
      "Duplicate names in {.fun sql_translator}",
      set_names(bullets, "*")
    ))
  }

  list2env(funs, copy_env(.parent))
}

copy_env <- function(from, to = NULL, parent = parent.env(from)) {
  list2env(as.list(from), envir = to, parent = parent)
}

#' @rdname sql_variant
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
    is_call(xq, "-", n = 1) && !is_atomic(x, n = 1)

  if (is_infix || is_unary_minus) {
    enpared <- glue_sql2(sql_current_con(), "({.val x})")
    return(enpared)
  }

  x
}

#' @rdname sql_variant
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

#' @rdname sql_variant
#' @export
sql_aggregate <- function(f, f_r = f) {
  check_string(f)

  function(x, na.rm = FALSE) {
    check_na_rm(na.rm)
    glue_sql2(sql_current_con(), "{f}({.val x})")
  }
}

#' @rdname sql_variant
#' @export
sql_aggregate_2 <- function(f) {
  check_string(f)

  function(x, y) {
    glue_sql2(sql_current_con(), "{f}({.val x}, {.val y})")
  }
}

#' @rdname sql_variant
#' @export
sql_aggregate_n <- function(f, f_r = f) {
  check_string(f)

  function(..., na.rm = FALSE) {
    check_na_rm(na.rm)
    dots <- list(...)
    glue_sql2(sql_current_con(), "{f}({.val dots*})")
  }
}

sql_aggregate_win <- function(f) {
  force(f)

  function(...) {
    cli_abort("{.fun {f}} is only available in a windowed ({.fun mutate}) context")
  }
}

check_na_rm <- function(na.rm) {
  if (identical(na.rm, TRUE)) {
    return()
  }

  cli::cli_warn(
    c(
      "Missing values are always removed in SQL aggregation functions.",
      "Use {.code na.rm = TRUE} to silence this warning"
    ),
    .frequency = "regularly",
    .frequency_id = "dbplyr_check_na_rm"
  )
}

#' @rdname sql_variant
#' @export
sql_not_supported <- function(f) {
  check_string(f)

  function(...) {
    cli_abort("{.fun {f}} is not available in this SQL variant.")
  }
}

sql_agg_not_supported <- function(f, backend) {
  check_string(f)

  msg <- "Translation of {.fun {f}} in {.fun summarise} is not supported"

  if (is.null(backend)) {
    msg <- paste0(msg, " on database backends.")
  } else {
    msg <- paste0(msg, " for {backend}.")
  }

  function(...) {
    dots <- enexprs(...)
    f_call_str <- as_label(call2(f, !!!dots))
    msg <- c(
      msg,
      i = "Use a combination of {.fun distinct} and {.fun mutate} for the same result:",
      " " = "{.code mutate(<col> = {f_call_str}) %>% distinct(<col>)}"
    )
    cli_abort(msg)
  }
}

sql_win_not_supported <- function(f, backend) {
  check_string(f)

  msg <- "Translation of {.fun {f}} in {.fun mutate} is not supported"

  if (is.null(backend)) {
    msg <- paste0(msg, " on database backends.")
  } else {
    msg <- paste0(msg, " for {backend}.")
  }

  function(...) {
    dots <- enexprs(...)
    f_call_str <- as_label(call2(f, !!!dots))
    msg <- c(
      msg,
      i = "Use a combination of {.fun summarise} and {.fun left_join} instead:",
      " " = "{.code df %>% left_join(summarise(<col> = {f_call_str}))}."
    )
    cli_abort(msg)
  }
}

#' @rdname sql_variant
#' @export
sql_cast <- function(type) {
  type <- sql(type)
  function(x) {
    sql_expr(cast(!!x %as% !!type))
  }
}

#' @rdname sql_variant
#' @export
sql_try_cast <- function(type) {
  type <- sql(type)
  function(x) {
    sql_expr(try_cast(!!x %as% !!type))
    # try_cast available in MSSQL 2012+
  }
}

#' @rdname sql_variant
#' @export
sql_log <- function() {
  function(x, base = exp(1)){
    if (isTRUE(all.equal(base, exp(1)))) {
      sql_expr(ln(!!x))
    } else {
      sql_expr(log(!!x) / log(!!base))
    }
  }
}


#' @rdname sql_variant
#' @export
sql_cot <- function(){
  function(x){
    sql_expr(1L / tan(!!x))
  }
}

#' @rdname sql_variant
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
