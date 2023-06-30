#' Create a local lazy tibble
#'
#' These functions are useful for testing SQL generation without having to
#' have an active database connection. See [simulate_dbi()] for a list
#' available database simulations.
#'
#' @keywords internal
#' @export
#' @examples
#' library(dplyr)
#' df <- data.frame(x = 1, y = 2)
#'
#' df_sqlite <- tbl_lazy(df, con = simulate_sqlite())
#' df_sqlite %>% summarise(x = sd(x, na.rm = TRUE)) %>% show_query()
tbl_lazy <- function(df, con = NULL, ..., name = "df") {
  check_dots_empty0(...)
  con <- con %||% sql_current_con() %||% simulate_dbi()
  subclass <- class(con)[[1]]

  dplyr::make_tbl(
    purrr::compact(c(subclass, "lazy")),
    lazy_query = lazy_query_local(df, name),
    src = src_dbi(con)
  )
}
methods::setOldClass(c("tbl_lazy", "tbl"))

#' @export
#' @rdname tbl_lazy
lazy_frame <- function(..., con = NULL, .name = "df") {
  con <- con %||% sql_current_con() %||% simulate_dbi()
  tbl_lazy(tibble(...), con = con, name = .name)
}

#' @export
dimnames.tbl_lazy <- function(x) {
  list(NULL, op_vars(x$lazy_query))
}

#' @export
dim.tbl_lazy <- function(x) {
  c(NA, length(op_vars(x$lazy_query)))
}

#' @export
print.tbl_lazy <- function(x, ...) {
  show_query(x)
}

#' @export
as.data.frame.tbl_lazy <- function(x, row.names, optional, ...) {
  cli_abort("Can not coerce {.cls tbl_lazy} to {.cls data.frame}")
}

#' @importFrom dplyr same_src
#' @export
same_src.tbl_lazy <- function(x, y) {
  inherits(y, "tbl_lazy")
}

#' @importFrom dplyr tbl_vars
#' @export
tbl_vars.tbl_lazy <- function(x) {
  op_vars(x$lazy_query)
}

#' @importFrom dplyr groups
#' @export
groups.tbl_lazy <- function(x) {
  lapply(group_vars(x), as.name)
}

# nocov start
# Manually registered in zzz.R
group_by_drop_default.tbl_lazy <- function(x) {
  TRUE
}
# nocov end

#' @importFrom dplyr group_vars
#' @export
group_vars.tbl_lazy <- function(x) {
  op_grps(x$lazy_query)
}

is_tbl_lazy <- function(x) {
  inherits(x, "tbl_lazy")
}

#' @importFrom tidyselect tidyselect_data_proxy tidyselect_data_has_predicates
#' @export
tidyselect_data_proxy.tbl_lazy <- function(x) {
  vars <- op_vars(x)
  out <- as_tibble(rep_named(vars, list(logical())), .name_repair = "minimal")
  group_by(out, !!!syms(group_vars(x)))
}

#' @export
tidyselect_data_has_predicates.tbl_lazy <- function(x) {
  FALSE
}

the <- new_environment()
the$warned_on_tbl_lazy_names <- FALSE

#' @export
names.tbl_lazy <- function(x) {
  if (is_direct(rlang::caller_env())) {
    cli::cli_inform(c(
      `!` = "The {.fn names} method of {.cls tbl_lazy} is not intended to be called.",
      i = "Did you mean {.fn colnames}?"
    ))
  }
  NextMethod("names")
}

#' @export
`$.tbl_lazy` <- function(x, name) {
  unexpected_name <- !name %in% c("lazy_query", "src")

  if (unexpected_name) {
    cli::cli_abort(c(
      "The {.fn $} method of {.cls tbl_lazy} is not intended to be called.",
      i = "Use {.fn dplyr::pull} to get the values in a column."
    ))
  }
  NextMethod("$")
}

# COPIED FROM lifecycle
# https://github.com/r-lib/lifecycle/blob/9417eca8f5091f95b4569fb6c388e4394e2b2157/R/deprecate.R#L358
is_direct <- function(env) {
  env_inherits_global(env) || from_testthat(env)
}

env_inherits_global <- function(env) {
  # `topenv(emptyenv())` returns the global env. Return `FALSE` in
  # that case to allow passing the empty env when the
  # soft-deprecation should not be promoted to deprecation based on
  # the caller environment.
  if (is_reference(env, empty_env())) {
    return(FALSE)
  }

  is_reference(topenv(env), global_env())
}

# TRUE if we are in unit tests and the package being tested is the
# same as the package that called
from_testthat <- function(env) {
  tested_package <- Sys.getenv("TESTTHAT_PKG")
  if (!nzchar(tested_package)) {
    return(FALSE)
  }

  top <- topenv(env)
  if (!is_namespace(top)) {
    return(FALSE)
  }

  # Test for environment names rather than reference/contents because
  # testthat clones the namespace
  identical(ns_env_name(top), tested_package)
}
