new_join_by <- function(
  x = character(),
  y = x,
  condition = "==",
  x_as = NULL,
  y_as = NULL
) {
  if (length(x) != length(y)) {
    cli::cli_abort(
      "{.arg x} and {.arg y} must have the same length.",
      .internal = TRUE
    )
  }
  condition <- vctrs::vec_recycle(condition, length(x), x_arg = "condition")
  check_string(x_as, allow_null = TRUE)
  check_string(y_as, allow_null = TRUE)

  list(
    x = x,
    y = y,
    condition = condition,
    x_as = x_as,
    y_as = y_as
  )
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
  cli::cli_abort(message, call = error_call)
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

  new_join_by(x_names, y_names)
}

#' @export
dbplyr_as_join_by.list <- function(x, error_call = caller_env()) {
  # TODO: check lengths
  x_names <- x[["x"]]
  y_names <- x[["y"]]

  if (!is_character(x_names)) {
    cli::cli_abort("`by$x` must evaluate to a character vector.")
  }
  if (!is_character(y_names)) {
    cli::cli_abort("`by$y` must evaluate to a character vector.")
  }

  new_join_by(x_names, y_names)
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
    cli::cli_abort(message, call = error_call)
  }

  by_names <- by
  by_names <- glue::glue_collapse(by_names, sep = ", ")
  inform(glue("Joining with `by = join_by({by_names})`"))

  new_join_by(by, by)
}
