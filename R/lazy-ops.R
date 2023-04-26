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
lazy_base_query <- function(x, vars, class = character(), ...) {
  check_character(vars)

  lazy_query(
    query_type = c(paste0("base_", class), "base"),
    x = x,
    vars = vars,
    ...,
    group_vars = character(),
    order_vars = NULL,
    frame = NULL
  )
}

lazy_query_local <- function(df, name) {
  if (is_bare_character(name)) {
    name <- ident(name)
  }

  lazy_base_query(df, names(df), class = "local", name = name)
}

lazy_query_remote <- function(x, vars) {
  lazy_base_query(x, vars, class = "remote")
}

#' @export
print.lazy_base_remote_query <- function(x, ...) {
  if (inherits(x$x, "ident")) {
    cat("From: ", x$x, "\n", sep = "")
  } else {
    cat("From: <derived table>\n")
  }

  cat("<Table: ", x$x, ">\n", sep = "")
}

#' @export
print.lazy_base_local_query <- function(x, ...) {
  cat("<Local data frame> ", dplyr::dim_desc(x$x), "\n", sep = "")
}

#' @export
sql_build.lazy_base_remote_query <- function(op, con, ...) {
  as.sql(op$x, con = con)
}

#' @export
sql_build.lazy_base_local_query <- function(op, con, ...) {
  as.sql(op$name, con = con)
}

# op_grps -----------------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_grps <- function(op) UseMethod("op_grps")
#' @export
op_grps.tbl_lazy <- function(op) op_grps(op$lazy_query)
#' @export
op_grps.lazy_query <- function(op) op$group_vars %||% character()

# op_vars -----------------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_vars <- function(op) UseMethod("op_vars")
#' @export
op_vars.tbl_lazy <- function(op) op_vars(op$lazy_query)
#' @export
op_vars.lazy_base_query <- function(op) op$vars

# op_sort -----------------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_sort <- function(op) UseMethod("op_sort")
#' @export
op_sort.tbl_lazy <- function(op) op_sort(op$lazy_query)
#' @export
op_sort.lazy_query <- function(op) {
  # Renaming (like for groups) cannot be done because:
  # * `order_vars` is a list of quosures
  # * variables needed in sorting can be dropped
  op$order_vars
}

# op_frame ----------------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_frame <- function(op) UseMethod("op_frame")
#' @export
op_frame.tbl_lazy <- function(op) op_frame(op$lazy_query)
#' @export
op_frame.lazy_query <- function(op) op$frame

# Description -------------------------------------------------------------

op_rows <- function(op, rows_total = NA_integer_) {
  if (is.na(rows_total)) {
    "??"
  } else {
    big_mark(rows_total)
  }
}
op_cols <- function(op) {
  length(op_vars(op))
}

op_desc <- function(op) UseMethod("op_desc")

#' @export
op_desc.lazy_base_remote_query <- function(op) {
  if (is.ident(op$x)) {
    paste0("table<", op$x, ">")
  } else {
    "SQL"
  }
}
