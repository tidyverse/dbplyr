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
tbl_lazy <- function(df, con = simulate_dbi(), src = NULL) {

  if (!is.null(src)) {
    warn("`src` is deprecated; please use `con` instead")
    con <- src
  }
  subclass <- class(con)[[1]]

  make_tbl(
    purrr::compact(c(subclass, "lazy")),
    ops = op_base_local(df),
    src = src_dbi(con)
  )
}
setOldClass(c("tbl_lazy", "tbl"))

#' @export
#' @rdname tbl_lazy
lazy_frame <- function(..., con = simulate_dbi(), src = NULL) {
  tbl_lazy(tibble(...), con = con, src = src)
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
print.tbl_lazy <- function(x, ...) {
  show_query(x)
}

#' @export
as.data.frame.tbl_lazy <- function(x, row.names, optional, ...) {
  stop("Can not coerce `tbl_lazy` to data.frame", call. = FALSE)
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

# Manually registered in zzz.R
group_by_drop_default.tbl_lazy <- function(x) {
  TRUE
}

#' @export
group_vars.tbl_lazy <- function(x) {
  op_grps(x$ops)
}

# lazyeval ----------------------------------------------------------------

# nocov start
#' @export
filter_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  filter(.data, !!!dots)
}
#' @export
arrange_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  arrange(.data, !!!dots)
}
#' @export
select_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  select(.data, !!!dots)
}
#' @export
rename_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  rename(.data, !!!dots)
}
#' @export
summarise_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  summarise(.data, !!!dots)
}
#' @export
mutate_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  mutate(.data, !!!dots)
}
#' @export
group_by_.tbl_lazy <- function(.data, ..., .dots = list(), add = FALSE) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  group_by(.data, !!!dots, add = add)
}
#' @export
distinct_.tbl_lazy <- function(.data, ..., .dots = list(), .keep_all = FALSE) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  distinct(.data, !!! dots, .keep_all = .keep_all)
}
#' @export
do_.tbl_sql <- function(.data, ..., .dots = list(), .chunk_size = 1e4L) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  do(.data, !!! dots, .chunk_size = .chunk_size)
}
# nocov end
