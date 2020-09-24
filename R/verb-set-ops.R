#' SQL set operations
#'
#' These are methods for the dplyr generics `dplyr::intersect()`,
#' `dplyr::union()`, and `dplyr::setdiff()`. They are translated to
#' `INTERSECT`, `UNION`, and `EXCEPT` respectively.
#'
#' @inheritParams left_join.tbl_lazy
#' @param ... Not currently used; provided for future extensions.
#' @param all If `TRUE`, includes all matches in output, not just unique rows.
#' @name dbplyr-set_ops
#' @aliases NULL
NULL

# registered onLoad
#' @rdname dbplyr-set_ops
#' @importFrom dplyr intersect
intersect.tbl_lazy <- function(x, y, copy = FALSE, ..., all = FALSE) {
  add_op_set_op(x, y, "INTERSECT", all = all, copy = copy, ...)
}
# registered onLoad
#' @rdname dbplyr-set_ops
#' @importFrom dplyr union
union.tbl_lazy <- function(x, y, copy = FALSE, ..., all = FALSE) {
  add_op_set_op(x, y, "UNION", all = all, copy = copy, ...)
}
#' @export
#' @rdname dbplyr-set_ops
#' @importFrom dplyr union_all
union_all.tbl_lazy <- function(x, y, copy = FALSE, ...) {
  add_op_set_op(x, y, "UNION", all = TRUE, copy = copy, ...)
}
# registered onLoad
#' @rdname dbplyr-set_ops
#' @importFrom dplyr setdiff
setdiff.tbl_lazy <- function(x, y, copy = FALSE, ..., all = FALSE) {
  add_op_set_op(x, y, "EXCEPT", all = all, copy = copy, ...)
}

add_op_set_op <- function(x, y, type, copy = FALSE, ..., all = FALSE) {
  y <- auto_copy(x, y, copy)

  if (inherits(x$src$con, "SQLiteConnection")) {
    # LIMIT only part the compound-select-statement not the select-core.
    #
    # https://www.sqlite.org/syntax/compound-select-stmt.html
    # https://www.sqlite.org/syntax/select-core.html

    if (inherits(x$ops, "op_head") || inherits(y$ops, "op_head")) {
      stop("SQLite does not support set operations on LIMITs", call. = FALSE)
    }
  }

  # Ensure each has same variables
  vars <- union(op_vars(x), op_vars(y))
  x <- fill_vars(x, vars)
  y <- fill_vars(y, vars)

  x$ops <- op_double("set_op", x, y, args = list(type = type, all = all))
  x
}

fill_vars <- function(x, vars) {
  x_vars <- op_vars(x)
  if (identical(x_vars, vars)) {
    return(x)
  }

  new_vars <- lapply(set_names(vars), function(var) {
    if (var %in% x_vars) {
      sym(var)
    } else {
      NA
    }
  })

  x$ops <- op_select(x$ops, new_vars)
  x
}

#' @export
op_vars.op_set_op <- function(op) {
  union(op_vars(op$x), op_vars(op$y))
}

#' @export
sql_build.op_set_op <- function(op, con, ...) {
  # add_op_set_op() ensures that both have same variables
  set_op_query(op$x, op$y, type = op$args$type, all = op$args$type)
}
