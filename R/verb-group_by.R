# group_by ----------------------------------------------------------------

#' @export
#' @importFrom dplyr group_by
group_by.tbl_lazy <- function(.data, ..., .add = FALSE, add = NULL, .drop = TRUE) {
  dots <- quos(...)
  dots <- partial_eval_dots(dots, vars = op_vars(.data))

  if (!missing(add)) {
    lifecycle::deprecate_warn("1.0.0", "dplyr::group_by(add = )", "group_by(.add = )")
    .add <- add
  }

  if (!identical(.drop, TRUE)) {
    stop("`.drop` is not supported with database backends", call. = FALSE)
  }

  if (length(dots) == 0) {
    return(.data)
  }

  if (".add" %in% names(formals("group_by"))) {
    groups <- dplyr::group_by_prepare(.data, !!!dots, .add = .add)
  } else {
    groups <- dplyr::group_by_prepare(.data, !!!dots, add = .add)
  }
  names <- purrr::map_chr(groups$groups, as_string)

  add_op_single("group_by",
    groups$data,
    dots = set_names(groups$groups, names),
    args = list(add = FALSE)
  )
}

#' @export
op_desc.op_group_by <- function(x, ...) {
  op_desc(x$x, ...)
}

#' @export
op_grps.op_group_by <- function(op) {
  if (isTRUE(op$args$add)) {
    union(op_grps(op$x), names(op$dots))
  } else {
    names(op$dots)
  }
}

#' @export
sql_build.op_group_by <- function(op, con, ...) {
  sql_build(op$x, con, ...)
}

# ungroup -----------------------------------------------------------------

#' @importFrom dplyr ungroup
#' @export
ungroup.tbl_lazy <- function(x, ...) {
  add_op_single("ungroup", x)
}

#' @export
op_grps.op_ungroup <- function(op) {
  character()
}

#' @export
sql_build.op_ungroup <- function(op, con, ...) {
  sql_build(op$x, con, ...)
}
