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
tbl_lazy <- function(df, con = NULL, src = NULL, name = "df") {

  if (!is.null(src)) {
    warn("`src` is deprecated; please use `con` instead")
    con <- src
  }
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
lazy_frame <- function(..., con = NULL, src = NULL, .name = "df") {
  con <- con %||% sql_current_con() %||% simulate_dbi()
  tbl_lazy(tibble(...), con = con, src = src, name = .name)
}

#' @export
dimnames.tbl_lazy <- function(x) {
  list(NULL, op_vars(x$lazy_query))
}

#' @export
names.tbl_lazy <- function(x) {
  colnames(x)
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
