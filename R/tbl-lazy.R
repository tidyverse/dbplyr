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
#' df_sqlite <- tbl_lazy(df, src = simulate_sqlite())
#' df_sqlite %>% summarise(x = sd(x, na.rm = TRUE)) %>% show_query()
tbl_lazy <- function(df, src = simulate_dbi()) {
  subclass <- class(src)[[1]]

  make_tbl(
    purrr::compact(c(subclass, "lazy")),
    ops = op_base_local(df),
    src = src
  )
}
setOldClass(c("tbl_lazy", "tbl"))

#' @export
#' @rdname tbl_lazy
lazy_frame <- function(..., src = simulate_dbi()) {
  tbl_lazy(tibble(...), src = src)
}


#' @export
dimnames.tbl_lazy <- function(x) {
  list(NULL, op_vars(x$ops))
}

#' @export
dim.tbl_lazy <- function(x) {
  c(NA, length(op_vars(x$ops)))
}

#' @export
as.data.frame.tbl_lazy <- function(x, row.names, optional, ...) {
  message("Use show_query() to see generated SQL")
  data.frame()
}

#' @export
same_src.tbl_lazy <- function(x, y) {
  inherits(y, "tbl_lazy")
}

#' @export
tbl_vars.tbl_lazy <- function(x) {
  op_vars(x$ops)
}

#' @export
groups.tbl_lazy <- function(x) {
  lapply(group_vars(x), as.name)
}

#' @export
group_vars.tbl_lazy <- function(x) {
  op_grps(x$ops)
}

# lazyeval ----------------------------------------------------------------

#' @export
filter_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  dots <- partial_eval(dots, vars = op_vars(.data))
  add_op_single("filter", .data, dots = dots)
}
#' @export
arrange_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  arrange(.data, !!! dots)
}
#' @export
select_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  add_op_single("select", .data, dots = dots)
}
#' @export
rename_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  dots <- partial_eval(dots, vars = op_vars(.data))
  add_op_single("rename", .data, dots = dots)
}
#' @export
summarise_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  dots <- partial_eval(dots, vars = op_vars(.data))
  add_op_single("summarise", .data, dots = dots)
}
#' @export
mutate_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  dots <- partial_eval(dots, vars = op_vars(.data))
  add_op_single("mutate", .data, dots = dots)
}
#' @export
group_by_.tbl_lazy <- function(.data, ..., .dots = list(), add = FALSE) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  group_by(.data, !!! dots, add = add)
}
#' @export
distinct_.tbl_lazy <- function(.data, ..., .dots = list(), .keep_all = FALSE) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  distinct(.data, !!! dots, .keep_all = .keep_all)
}
