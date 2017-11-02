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
#'   cor = sql_aggregate_2("corr"),
#'   cov = sql_aggregate_2("covar_samp"),
#'   sd =  sql_aggregate("stddev_samp"),
#'   var = sql_aggregate("var_samp")
#' )
#' postgres_var <- sql_variant(
#'   base_scalar,
#'   postgres_agg,
#'   base_no_win
#' )
#'
#' # Next we have to simulate a connection that uses this variant
#' con <- structure(
#'   list(),
#'   class = c("TestCon", "DBITestConnection", "DBIConnection")
#' )
#' sql_translate_env.TestCon <- function(x) postgres_var
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
  stopifnot(is.environment(scalar))
  stopifnot(is.environment(aggregate))
  stopifnot(is.environment(window))

  # Need to check that every function in aggregate also occurs in window
  missing <- setdiff(ls(aggregate), ls(window))
  if (length(missing) > 0) {
    warn(paste0(
      "Translator is missing window variants of the following aggregate functions:\n",
      paste0("* ", missing, "\n", collapse = "")
    ))
  }

  structure(
    list(scalar = scalar, aggregate = aggregate, window = window),
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
sql_translator <- function(..., .funs = list(),
                           .parent = new.env(parent = emptyenv())) {
  funs <- c(list(...), .funs)
  if (length(funs) == 0) return(.parent)

  list2env(funs, copy_env(.parent))
}

copy_env <- function(from, to = NULL, parent = parent.env(from)) {
  list2env(as.list(from), envir = to, parent = parent)
}

#' @rdname sql_variant
#' @export
sql_infix <- function(f) {
  assert_that(is_string(f))

  f <- toupper(f)
  function(x, y) {
    build_sql(x, " ", sql(f), " ", y)
  }
}

#' @rdname sql_variant
#' @export
sql_prefix <- function(f, n = NULL) {
  assert_that(is_string(f))

  f <- toupper(f)
  function(...) {
    args <- list(...)
    if (!is.null(n) && length(args) != n) {
      stop(
        "Invalid number of args to SQL ", f, ". Expecting ", n,
        call. = FALSE
      )
    }
    if (any(names2(args) != "")) {
      warning("Named arguments ignored for SQL ", f, call. = FALSE)
    }
    build_sql(sql(f), args)
  }
}

#' @rdname sql_variant
#' @export
sql_aggregate <- function(f) {
  assert_that(is_string(f))
  f <- toupper(f)

  function(x, na.rm = FALSE) {
    check_na_rm(f, na.rm)
    build_sql(sql(f), list(x))
  }
}

#' @rdname sql_variant
#' @export
sql_aggregate_2 <- function(f) {
  assert_that(is_string(f))
  f <- toupper(f)

  function(x, y) {
    build_sql(sql(f), list(x, y))
  }
}


check_na_rm <- function(f, na.rm) {
  if (identical(na.rm, TRUE)) {
    return()
  }

  warning(
    "Missing values are always removed in SQL.\n",
    "Use `", f, "(x, na.rm = TRUE)` to silence this warning",
    call. = FALSE
  )
}

#' @rdname sql_variant
#' @export
sql_not_supported <- function(f) {
  assert_that(is_string(f))

  f <- toupper(f)
  function(...) {
    stop(f, " is not available in this SQL variant", call. = FALSE)
  }
}

#' @rdname sql_variant
#' @export
sql_cast <- function(type) {
  type <- sql(type)
  function(x) {
    sql_expr(cast(UQ(x) %as% UQ(type)))
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

globalVariables(c("%as%", "cast", "ln"))
