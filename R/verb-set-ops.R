#' SQL set operations
#'
#' These are methods for the dplyr generics `dplyr::intersect()`,
#' `dplyr::union()`, and `dplyr::setdiff()`. They are translated to
#' `INTERSECT`, `UNION`, and `EXCEPT` respectively.
#'
#' @inheritParams left_join.tbl_lazy
#' @param ... Not currently used; provided for future extensions.
#' @param all If `TRUE`, includes all matches in output, not just unique rows.
# registered onLoad
#' @importFrom dplyr intersect
intersect.tbl_lazy <- function(x, y, copy = FALSE, ..., all = FALSE) {
  lazy_query <- add_set_op(x, y, "INTERSECT", copy = copy, ..., all = all)

  out <- add_op_set_op(x, y, "INTERSECT", copy = copy, ..., all = all)
  out$lazy_query <- lazy_query
  out
}
# registered onLoad
#' @importFrom dplyr union
#' @rdname intersect.tbl_lazy
union.tbl_lazy <- function(x, y, copy = FALSE, ..., all = FALSE) {
  lazy_query <- add_set_op(x, y, "UNION", copy = copy, ..., all = all)

  out <- add_op_set_op(x, y, "UNION", copy = copy, ..., all = all)
  out$lazy_query <- lazy_query
  out
}
#' @export
#' @importFrom dplyr union_all
#' @rdname intersect.tbl_lazy
union_all.tbl_lazy <- function(x, y, copy = FALSE, ...) {
  lazy_query <- add_set_op(x, y, "UNION ALL", copy = copy, ..., all = FALSE)

  out <- add_op_set_op(x, y, "UNION ALL", copy = copy, ..., all = FALSE)
  out$lazy_query <- lazy_query
  out
}
# registered onLoad
#' @importFrom dplyr setdiff
#' @rdname intersect.tbl_lazy
setdiff.tbl_lazy <- function(x, y, copy = FALSE, ..., all = FALSE) {
  lazy_query <- add_set_op(x, y, "EXCEPT", copy = copy, ..., all = all)

  out <- add_op_set_op(x, y, "EXCEPT", copy = copy, ..., all = all)
  out$lazy_query <- lazy_query
  out
}

add_set_op <- function(x, y, type, copy = FALSE, ..., all = FALSE) {
  y <- auto_copy(x, y, copy)

  if (inherits(x$src$con, "SQLiteConnection")) {
    # LIMIT only part the compound-select-statement not the select-core.
    #
    # https://www.sqlite.org/syntax/compound-select-stmt.html
    # https://www.sqlite.org/syntax/select-core.html

    if (identical(x$lazy_query$last_op, "head") || identical(y$lazy_query$last_op, "head")) {
      stop("SQLite does not support set operations on LIMITs", call. = FALSE)
    }
  }

  # Ensure each has same variables
  vars <- union(op_vars(x), op_vars(y))
  x <- fill_vars(x, vars)
  y <- fill_vars(y, vars)

  # lazy_set_op_query(x, y, type = type, all = all)
  lazy_set_op_query(x$lazy_query, y$lazy_query, type = type, all = all)
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

  transmute(x, !!!new_vars)
}

#' @export
op_vars.op_set_op <- function(op) {
  union(op_vars(op$x), op_vars(op$y))
}

#' @export
sql_build.op_set_op <- function(op, con, ...) {
  # add_op_set_op() ensures that both have same variables
  set_op_query(op$x, op$y, type = op$args$type, all = op$args$all)
}
