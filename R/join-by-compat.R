new_join_by <- function(exprs, condition, filter, cross, x, y) {
  out <- list(
    exprs = exprs,
    condition = condition,
    filter = filter,
    cross = cross,
    x = x,
    y = y
  )
  structure(out, class = "dplyr_join_by")
}

# ------------------------------------------------------------------------------

# Internal generic
as_join_by <- function(x, error_call = caller_env()) {
  UseMethod("as_join_by")
}

# TODO they might not be needed?
#' @export
as_join_by.default <- function(x, error_call = caller_env()) {
  message <- glue(paste0(
    "`by` must be a (named) character vector, list, `join_by()` result, ",
    "or NULL, not {obj_type_friendly(x)}."
  ))
  abort(message, call = error_call)
}

#' @export
as_join_by.dplyr_join_by <- function(x, error_call = caller_env()) {
  x
}

#' @export
as_join_by.character <- function(x, error_call = caller_env()) {
  x_names <- names(x) %||% x
  y_names <- unname(x)

  # If x partially named, assume unnamed are the same in both tables
  x_names[x_names == ""] <- y_names[x_names == ""]

  finalise_equi_join_by(x_names, y_names)
}

#' @export
as_join_by.list <- function(x, error_call = caller_env()) {
  # TODO: check lengths
  x_names <- x[["x"]]
  y_names <- x[["y"]]
  finalise_equi_join_by(x_names, y_names)
}

finalise_equi_join_by <- function(x_names, y_names) {
  n <- length(x_names)

  exprs <- purrr::map2(x_names, y_names, function(x, y) expr(!!x == !!y))
  condition <- vctrs::vec_rep("==", times = n)
  filter <- vctrs::vec_rep("none", times = n)
  cross <- n == 0L

  new_join_by(
    exprs = exprs,
    condition = condition,
    filter = filter,
    cross = cross,
    x = x_names,
    y = y_names
  )
}

# ------------------------------------------------------------------------------

join_by_common <- function(x_names,
                           y_names,
                           ...,
                           error_call = caller_env()) {
  check_dots_empty0(...)

  by <- intersect(x_names, y_names)

  if (length(by) == 0) {
    message <- c(
      "`by` must be supplied when `x` and `y` have no common variables.",
      i = "Use `by = join_by()` to perform a cross-join."
    )
    abort(message, call = error_call)
  }

  # by_names <- tick_if_needed(by)
  by_names <- by
  by_names <- glue::glue_collapse(by_names, sep = ", ")
  inform(glue("Joining with `by = join_by({by_names})`"))

  finalise_equi_join_by(by, by)
}
