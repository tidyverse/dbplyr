#' Lazy operations
#'
#' This set of S3 classes describe the action of dplyr verbs. These are
#' currently used for SQL sources to separate the description of operations
#' in R from their computation in SQL. This API is very new so is likely
#' to evolve in the future.
#'
#' `op_vars()` and `op_grps()` compute the variables and groups from
#' a sequence of lazy operations. `op_sort()` and `op_frame()` tracks the
#' order and frame for use in window functions.
#'
#' @keywords internal
#' @name lazy_ops
NULL

# Base constructors -------------------------------------------------------

#' @export
#' @rdname lazy_ops
lazy_query_base <- function(x, vars, name = NULL, class = character()) {
  stopifnot(is.character(vars))

  structure(
    list(
      x = x,
      vars = vars
    ),
    class = c(paste0("lazy_query_base_", class), "lazy_query_base", "lazy_query")
  )
}

lazy_query_local <- function(df, name = NULL) {
  # if (!is.ident(name)) {
  #   vctrs::vec_assert(name, character(), size = 1)
  #   name <- ident(name)
  # }
  lazy_query_base(df, names(df), name = name, class = "local")
}

lazy_query_remote <- function(x, vars) {
  lazy_query_base(x, vars, class = "remote")
}

#' @export
print.lazy_query_base_remote <- function(x, ...) {
  if (inherits(x$x, "ident")) {
    cat("From: ", x$x, "\n", sep = "")
  } else {
    cat("From: <derived table>\n")
  }

  cat("<Table: ", x$x, ">\n", sep = "")
}

#' @export
print.lazy_query_base_local <- function(x, ...) {
  cat("<Local data frame> ", dplyr::dim_desc(x$x), "\n", sep = "")
}

#' @export
sql_build.lazy_query_base_remote <- function(op, con, ...) {
  op$x
}

#' @export
sql_build.lazy_query_base_local <- function(op, con, ...) {
  # TODO
  ident("df")
  # op$name
}

# op_grps -----------------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_grps <- function(op) UseMethod("op_grps")
#' @export
op_grps.tbl_lazy <- function(op) op_grps(op$lazy_query)
#' @export
op_grps.lazy_query_base <- function(op) character()

# op_vars -----------------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_vars <- function(op) UseMethod("op_vars")
#' @export
op_vars.tbl_lazy <- function(op) op_vars(op$lazy_query)
#' @export
op_vars.lazy_query_base <- function(op) op$vars

# op_sort -----------------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_sort <- function(op) UseMethod("op_sort")
#' @export
op_sort.tbl_lazy <- function(op) op_sort(op$lazy_query)
#' @export
op_sort.lazy_query_base <- function(op) NULL

# op_frame ----------------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_frame <- function(op) UseMethod("op_frame")
#' @export
op_frame.tbl_lazy <- function(op) op_frame(op$lazy_query)
#' @export
op_frame.lazy_query_base <- function(op) NULL

# Description -------------------------------------------------------------

op_rows <- function(op) "??"
op_cols <- function(op) length(op_vars(op))

op_desc <- function(op) UseMethod("op_desc")

#' @export
op_desc.lazy_query_base_remote <- function(op) {
  if (is.ident(op$x)) {
    paste0("table<", op$x, ">")
  } else {
    "SQL"
  }
}
