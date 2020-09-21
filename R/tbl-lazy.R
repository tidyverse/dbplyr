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
tbl_lazy <- function(df, con = NULL, src = NULL) {

  if (!is.null(src)) {
    warn("`src` is deprecated; please use `con` instead")
    con <- src
  }
  con <- con %||% sql_current_con() %||% simulate_dbi()
  subclass <- class(con)[[1]]

  dplyr::make_tbl(
    purrr::compact(c(subclass, "lazy")),
    ops = op_base_local(df),
    src = src_dbi(con)
  )
}
setOldClass(c("tbl_lazy", "tbl"))

#' @export
#' @rdname tbl_lazy
lazy_frame <- function(..., con = NULL, src = NULL) {
  con <- con %||% sql_current_con() %||% simulate_dbi()
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

#' @importFrom dplyr same_src
#' @export
same_src.tbl_lazy <- function(x, y) {
  inherits(y, "tbl_lazy")
}

#' @importFrom dplyr tbl_vars
#' @export
tbl_vars.tbl_lazy <- function(x) {
  op_vars(x$ops)
}

#' @importFrom dplyr groups
#' @export
groups.tbl_lazy <- function(x) {
  lapply(group_vars(x), as.name)
}

# Manually registered in zzz.R
group_by_drop_default.tbl_lazy <- function(x) {
  TRUE
}

#' @importFrom dplyr group_vars
#' @export
group_vars.tbl_lazy <- function(x) {
  op_grps(x$ops)
}
