#' Lazy operations
#'
#' @description
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
