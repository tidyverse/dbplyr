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
op_base <- function(x, vars, class = character()) {
  stopifnot(is.character(vars))

  structure(
    list(
      x = x,
      vars = vars
    ),
    class = c(paste0("op_base_", class), "op_base", "op")
  )
}

op_base_local <- function(df) {
  op_base(df, names(df), class = "local")
}

op_base_remote <- function(x, vars) {
  stopifnot(is.sql(x) || is.ident(x))
  op_base(x, vars, class = "remote")
}

#' @export
print.op_base_remote <- function(x, ...) {
  if (inherits(x$x, "ident")) {
    cat("From: ", x$x, "\n", sep = "")
  } else {
    cat("From: <derived table>\n")
  }

  cat("<Table: ", x$x, ">\n", sep = "")
}

#' @export
print.op_base_local <- function(x, ...) {
  cat("<Local data frame> ", dim_desc(x$x), "\n", sep = "")
}

#' @export
sql_build.op_base_remote <- function(op, con, ...) {
  op$x
}

#' @export
sql_build.op_base_local <- function(op, con, ...) {
  ident("df")
}

# Operators ---------------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_single <- function(name, x, dots = list(), args = list()) {
  structure(
    list(
      name = name,
      x = x,
      dots = dots,
      args = args
    ),
    class = c(paste0("op_", name), "op_single", "op")
  )
}

#' @export
#' @rdname lazy_ops
add_op_single <- function(name, .data, dots = list(), args = list()) {
  .data$ops <- op_single(name, x = .data$ops, dots = dots, args = args)
  .data
}

#' @export
print.op_single <- function(x, ...) {
  print(x$x)

  cat("-> ", x$name, "()\n", sep = "")
  for (dot in x$dots) {
    cat("   - ", deparse_trunc(dot), "\n", sep = "")
  }
}

#' @export
#' @rdname lazy_ops
op_double <- function(name, x, y, args = list()) {
  structure(
    list(
      name = name,
      x = x,
      y = y,
      args = args
    ),
    class = c(paste0("op_", name), "op_double", "op")
  )
}

# op_grps -----------------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_grps <- function(op) UseMethod("op_grps")
#' @export
op_grps.op_base <- function(op) character()
#' @export
op_grps.op_single <- function(op) op_grps(op$x)
#' @export
op_grps.op_double <- function(op) op_grps(op$x)
#' @export
op_grps.tbl_lazy <- function(op) op_grps(op$ops)

# op_vars -----------------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_vars <- function(op) UseMethod("op_vars")
#' @export
op_vars.op_base <- function(op) op$vars
#' @export
op_vars.op_single <- function(op) op_vars(op$x)
#' @export
op_vars.op_double <- function(op) stop("Not implemented", call. = FALSE)
#' @export
op_vars.tbl_lazy <- function(op) op_vars(op$ops)

# op_sort -----------------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_sort <- function(op) UseMethod("op_sort")
#' @export
op_sort.op_base <- function(op) NULL
#' @export
op_sort.op_single <- function(op) op_sort(op$x)
#' @export
op_sort.op_double <- function(op) op_sort(op$x)
#' @export
op_sort.tbl_lazy <- function(op) op_sort(op$ops)

# op_frame ----------------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_frame <- function(op) UseMethod("op_frame")
#' @export
op_frame.op_base <- function(op) NULL
#' @export
op_frame.op_single <- function(op) op_frame(op$x)
#' @export
op_frame.op_double <- function(op) op_frame(op$x)
#' @export
op_frame.tbl_lazy <- function(op) op_frame(op$ops)

# Description -------------------------------------------------------------

op_rows <- function(op) "??"
op_cols <- function(op) length(op_vars(op))

op_desc <- function(op) UseMethod("op_desc")

#' @export
op_desc.op <- function(x, ..., con = con) {
  "lazy query"
}

#' @export
op_desc.op_base_remote <- function(op) {
  if (is.ident(op$x)) {
    paste0("table<", op$x, ">")
  } else {
    "SQL"
  }
}

