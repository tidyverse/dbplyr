#' Create an SQL translator
#'
#' `sql_variant()` creates a SQL variant, a list of translators for scalar,
#' aggregate, and window functions. `sql_translator()` creates a translator,
#' an environment containing R to SQL translations. When creating a backend,
#' you'll use these functions to customize how R functions are converted to
#' SQL.
#'
#' @param scalar,aggregate,window The three families of functions that an SQL
#'   variant can supply.
#' @param ...,.funs Named functions, used to add custom converters from standard
#'   R functions to SQL functions. Specify individually in `...`, or provide a
#'   list of `.funs`.
#' @param .parent The SQL variant that this variant should inherit from.
#'   Defaults to `base_agg` which provides a standard set of mappings for the
#'   most common operators and functions.
#'
#' @section Base translators:
#'
#' dbplyr provides the following base translators that implement standard SQL
#' semantics:
#'
#' * `base_scalar` - scalar functions and operators
#' * `base_agg` - aggregate functions
#' * `base_win` - window functions
#' * `base_no_win` - versions of window functions that throw errors
#'
#' @family SQL translation helpers
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
sql_variant <- function(
  scalar = sql_translator(),
  aggregate = sql_translator(),
  window = sql_translator()
) {
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
    list(
      scalar = scalar,
      aggregate = aggregate,
      window = window,
      aggregate_fns = aggregate_fns
    ),
    class = "sql_variant"
  )
}

is.sql_variant <- function(x) inherits(x, "sql_variant")

#' @export
print.sql_variant <- function(x, ...) {
  wrap_ls <- function(x, ...) {
    vars <- sort(ls(envir = x))
    wrapped <- strwrap(paste0(vars, collapse = ", "), ...)
    if (identical(wrapped, "")) {
      return()
    }
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
sql_translator <- function(
  ...,
  .funs = list(),
  .parent = new.env(parent = emptyenv())
) {
  funs <- c(list2(...), .funs)
  if (length(funs) == 0) {
    return(.parent)
  }

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
