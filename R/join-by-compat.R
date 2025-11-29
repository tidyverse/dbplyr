new_join_by <- function(
  exprs = list(),
  condition = character(),
  filter = character(),
  x = character(),
  y = character()
) {
  out <- list(
    exprs = exprs,
    condition = condition,
    filter = filter,
    x = x,
    y = y
  )
  structure(out, class = "dplyr_join_by")
}

# ------------------------------------------------------------------------------

# Internal generic
dbplyr_as_join_by <- function(x, error_call = caller_env()) {
  UseMethod("dbplyr_as_join_by")
}

#' @export
dbplyr_as_join_by.default <- function(x, error_call = caller_env()) {
  message <- glue(paste0(
    "`by` must be a (named) character vector, list, `join_by()` result, ",
    "or NULL, not {obj_type_friendly(x)}."
  ))
  abort(message, call = error_call)
}

#' @export
dbplyr_as_join_by.dplyr_join_by <- function(x, error_call = caller_env()) {
  x
}

#' @export
dbplyr_as_join_by.character <- function(x, error_call = caller_env()) {
  x_names <- names(x) %||% x
  y_names <- unname(x)

  # If x partially named, assume unnamed are the same in both tables
  x_names[x_names == ""] <- y_names[x_names == ""]

  finalise_equi_join_by(x_names, y_names)
}

#' @export
dbplyr_as_join_by.list <- function(x, error_call = caller_env()) {
  # TODO: check lengths
  x_names <- x[["x"]]
  y_names <- x[["y"]]

  if (!is_character(x_names)) {
    abort("`by$x` must evaluate to a character vector.")
  }
  if (!is_character(y_names)) {
    abort("`by$y` must evaluate to a character vector.")
  }

  finalise_equi_join_by(x_names, y_names)
}

finalise_equi_join_by <- function(x_names, y_names) {
  n <- length(x_names)

  if (n == 0L) {
    abort(
      "Backwards compatible support for cross joins should have been caught earlier.",
      .internal = TRUE
    )
  }

  exprs <- purrr::map2(x_names, y_names, \(x, y) expr(!!x == !!y))
  condition <- vctrs::vec_rep("==", times = n)
  filter <- vctrs::vec_rep("none", times = n)

  new_join_by(
    exprs = exprs,
    condition = condition,
    filter = filter,
    x = x_names,
    y = y_names
  )
}

# ------------------------------------------------------------------------------

join_by_common <- function(x_names, y_names, ..., error_call = caller_env()) {
  check_dots_empty0(...)

  by <- intersect(x_names, y_names)

  if (length(by) == 0) {
    message <- c(
      "`by` must be supplied when `x` and `y` have no common variables.",
      i = "Use `cross_join()` to perform a cross-join."
    )
    abort(message, call = error_call)
  }

  by_names <- by
  by_names <- glue::glue_collapse(by_names, sep = ", ")
  inform(glue("Joining with `by = join_by({by_names})`"))

  finalise_equi_join_by(by, by)
}
