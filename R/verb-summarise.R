#' Summarise each group to one row
#'
#' This is a method for the dplyr [summarise()] generic. It generates the
#' `SELECT` clause of the SQL query, and generally needs to be combined with
#' `group_by()`.
#'
#' @inheritParams arrange.tbl_lazy
#' @inheritParams dplyr::summarise
#' @param .groups \Sexpr[results=rd]{lifecycle::badge("experimental")} Grouping structure of the result.
#'
#'   * "drop_last": dropping the last level of grouping. This was the
#'   only supported option before version 1.0.0.
#'   * "drop": All levels of grouping are dropped.
#'   * "keep": Same grouping structure as `.data`.
#'
#'   When `.groups` is not specified, it defaults to "drop_last".
#'
#'   In addition, a message informs you of that choice, unless the result is ungrouped,
#'   the option "dplyr.summarise.inform" is set to `FALSE`,
#'   or when `summarise()` is called from a function in a package.
#' @inherit arrange.tbl_lazy return
#' @importFrom dplyr summarise
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' db <- memdb_frame(g = c(1, 1, 1, 2, 2), x = c(4, 3, 6, 9, 2))
#' db |>
#'   summarise(n()) |>
#'   show_query()
#'
#' db |>
#'   group_by(g) |>
#'   summarise(n()) |>
#'   show_query()
summarise.tbl_lazy <- function(.data, ..., .by = NULL, .groups = NULL) {
  check_groups(.groups)

  by <- compute_by(
    {{ .by }},
    .data,
    by_arg = ".by",
    data_arg = ".data",
    error_call = caller_env()
  )

  if (by$from_by) {
    .data$lazy_query$group_vars <- by$names
    .groups <- "drop"
  }

  dots <- summarise_eval_dots(.data, ...)
  .data$lazy_query <- add_summarise(
    .data,
    dots,
    .groups = .groups,
    env_caller = caller_env()
  )

  if (by$from_by) {
    .data$lazy_query$group_vars <- character()
  }
  .data
}

#' @export
#' @importFrom dplyr reframe
reframe.tbl_lazy <- function(.data, ..., .by = NULL) {
  stop_unsupported_function("reframe")
}

summarise_eval_dots <- function(.data, ..., error_call = caller_env()) {
  dots <- as.list(enquos(..., .named = TRUE))
  dot_names <- names2(exprs(...))
  was_named <- have_name(exprs(...))
  cur_data <- .data
  error_env <- new_environment()

  for (i in seq_along(dots)) {
    dot <- dots[[i]]
    dot_name <- dot_names[[i]]
    parent.env(error_env) <- quo_get_env(dot)
    dot <- quo_set_env(dot, error_env)

    dots[[i]] <- partial_eval_quo(
      dot,
      .data,
      error_call,
      dot_name,
      was_named[[i]]
    )
    cur_data <- summarise_bind_error(cur_data, dots, i, error_env)
  }

  # Remove names from any list elements
  is_list <- purrr::map_lgl(dots, is.list)
  names2(dots)[is_list] <- ""

  # Auto-splice list results from partial_eval_quo()
  dots[!is_list] <- lapply(dots[!is_list], list)
  unlist(dots, recursive = FALSE)
}

summarise_bind_error <- function(cur_data, dots, i, error_env) {
  quos <- dots[[i]]
  if (!is.list(quos)) {
    quos <- set_names(list(quos), names(dots)[[i]])
  }

  for (j in seq_along(quos)) {
    dot_name <- names(quos)[[j]]
    summarise_bind_error1(error_env, dot_name)
    # remove variable from `cur_data` so that `partial_eval_sym()` evaluates
    # variable in `error_env`
    cur_data$lazy_query <- add_select(
      cur_data$lazy_query,
      set_names(setdiff(colnames(cur_data), dot_name))
    )
  }

  cur_data
}

summarise_bind_error1 <- function(error_env, dot_name) {
  msg <- c(
    "In {.pkg dbplyr} you cannot use a variable created in the same {.fun summarise}.",
    x = "{.var {dot_name}} was created earlier in this {.fun summarise}.",
    i = "You need an extra {.fun mutate} step to use it."
  )
  env_bind_lazy(
    error_env,
    !!dot_name := cli::cli_abort(msg, call = NULL)
  )
}

check_groups <- function(.groups) {
  if (is_null(.groups)) {
    return()
  }

  if (.groups %in% c("drop_last", "drop", "keep")) {
    return()
  }

  cli_abort(
    c(
      paste0(
        "{.arg .groups} can't be {as_label(.groups)}",
        if (.groups == "rowwise") " in dbplyr"
      ),
      i = 'Possible values are NULL (default), "drop_last", "drop", and "keep"'
    ),
    call = caller_env()
  )
}

add_summarise <- function(.data, dots, .groups, env_caller) {
  lazy_query <- .data$lazy_query

  cur_grps <- op_grps(lazy_query)
  summarise_message(cur_grps, .groups, env_caller)

  new_grps <- switch(
    .groups %||% "drop_last",
    drop_last = cur_grps[-length(cur_grps)],
    keep = cur_grps,
    drop = character()
  )

  # ensure grouping variables are listed first
  vars <- c(cur_grps, setdiff(names(dots), cur_grps))
  select <- syms(set_names(vars))
  select[names(dots)] <- dots

  if (can_inline_summarise(lazy_query)) {
    lazy_query$select <- new_lazy_select(select, group_vars = new_grps)
    lazy_query$select_operation <- "summarise"
    lazy_query$group_by <- syms(cur_grps)
    lazy_query$group_vars <- new_grps
    lazy_query
  } else {
    lazy_select_query(
      x = lazy_query,
      select = select,
      group_by = syms(cur_grps),
      group_vars = new_grps,
      select_operation = "summarise"
    )
  }
}

can_inline_summarise <- function(lazy_query) {
  if (!is_lazy_select_query(lazy_query)) {
    return(FALSE)
  }

  if (lazy_query$select_operation == "summarise") {
    return(FALSE)
  }

  # Inline: GROUP BY is executed after WHERE but before SELECT, DISTINCT,
  # ORDER BY, and LIMIT. So we can inline if the previous query only has
  # WHERE or ORDER BY

  if (is_true(lazy_query$distinct)) {
    return(FALSE)
  }
  if (!is_null(lazy_query$limit)) {
    return(FALSE)
  }

  # Can only inline if previous SELECT is a pure projection
  if (!is_pure_projection(lazy_query$select$expr, lazy_query$select$name)) {
    return(FALSE)
  }

  TRUE
}

summarise_message <- function(grps, .groups, env_caller) {
  verbose <- summarise_verbose(.groups, env_caller)
  if (!verbose) {
    return(invisible())
  }

  n <- length(grps)
  if (n <= 1) {
    return(invisible())
  }

  cli::cli_inform(c(
    "!" = "Grouped output by {.val {grps[-n]}}.",
    i = "Override behaviour and silence this message with the {.arg .groups} argument.",
    i = "Or use {.arg .by} instead of {.fun group_by}."
  ))
  invisible()
}

summarise_verbose <- function(.groups, .env) {
  is.null(.groups) &&
    is_reference(topenv(.env), global_env()) &&
    !identical(getOption("dplyr.summarise.inform"), FALSE)
}

compute_by <- function(
  by,
  data,
  ...,
  by_arg = "by",
  data_arg = "data",
  error_call = caller_env()
) {
  check_dots_empty0(...)

  by <- enquo(by)
  check_by(
    by,
    data,
    by_arg = by_arg,
    data_arg = data_arg,
    error_call = error_call
  )

  if (is_grouped_lf(data)) {
    names <- group_vars(data)
    from_by <- FALSE
  } else {
    by <- eval_select_by(by, data, error_call = error_call)
    names <- by
    from_by <- TRUE
  }

  new_by(from_by = from_by, names = names)
}

is_grouped_lf <- function(data) {
  !is_empty(group_vars(data))
}

check_by <- function(
  by,
  data,
  ...,
  by_arg = "by",
  data_arg = "data",
  error_call = caller_env()
) {
  check_dots_empty0(...)

  if (quo_is_null(by)) {
    return(invisible(NULL))
  }

  if (is_grouped_lf(data)) {
    message <- paste0(
      "Can't supply {.arg {by_arg}} when ",
      "{.arg {data_arg}} is a grouped data frame."
    )
    cli::cli_abort(message, call = error_call)
  }

  invisible(NULL)
}

eval_select_by <- function(by, data, error_call = caller_env()) {
  out <- tidyselect::eval_select(
    expr = by,
    data = data,
    allow_rename = FALSE,
    error_call = error_call
  )
  names(out)
}

new_by <- function(from_by, names) {
  structure(list(from_by = from_by, names = names), class = "dbplyr_by")
}
