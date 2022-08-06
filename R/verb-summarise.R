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
#' db %>%
#'   summarise(n()) %>%
#'   show_query()
#'
#' db %>%
#'   group_by(g) %>%
#'   summarise(n()) %>%
#'   show_query()
summarise.tbl_lazy <- function(.data, ..., .groups = NULL) {
  check_groups(.groups)
  dots <- summarise_eval_dots(.data, ...)

  .data$lazy_query <- add_summarise(
    .data, dots,
    .groups = .groups,
    env_caller = caller_env()
  )
  .data
}

summarise_eval_dots <- function(.data, ..., error_call = caller_env()) {
  dots <- as.list(enquos(..., .named = TRUE))
  was_named <- have_name(exprs(...))
  cur_data <- .data
  error_env <- new_environment()

  for (i in seq_along(dots)) {
    dot <- dots[[i]]
    dot_name <- get_dot_name(dots, i, was_named)
    parent.env(error_env) <- quo_get_env(dot)
    dot <- quo_set_env(dot, error_env)

    dots[[i]] <- partial_eval_quo(dot, .data, error_call, dot_name, was_named[[i]])
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
    cur_data$lazy_query <- add_select(cur_data, set_names(list(NULL), dot_name), "select")
  }

  cur_data
}

summarise_bind_error1 <- function(error_env, dot_name) {
  msg <- cli::format_message(c(
    "In {.pkg dbplyr} you cannot use a variable created in the same {.fun summarise}.",
    x = "{.var {dot_name}} was created earlier in this {.fun summarise}.",
    i = "You need an extra {.fun mutate} step to use it."
  ))
  env_bind_lazy(error_env, !!dot_name := {abort(msg, call = NULL)})
}

check_groups <- function(.groups) {
  if (is_null(.groups)) {
    return()
  }

  if (.groups %in% c("drop_last", "drop", "keep")) {
    return()
  }

  cli_abort(c(
    paste0("{.arg .groups} can't be {as_label(.groups)}", if (.groups == "rowwise") " in dbplyr"),
    i = 'Possible values are NULL (default), "drop_last", "drop", and "keep"'
  ), call = caller_env())
}

add_summarise <- function(.data, dots, .groups, env_caller) {
  lazy_query <- .data$lazy_query

  grps <- op_grps(lazy_query)
  message_summarise <- summarise_message(grps, .groups, env_caller)

  .groups <- .groups %||% "drop_last"
  groups_out <- switch(.groups,
    drop_last = grps[-length(grps)],
    keep = grps,
    drop = character()
  )

  vars <- c(grps, setdiff(names(dots), grps))
  select <- syms(set_names(vars))
  select[names(dots)] <- dots

  lazy_select_query(
    x = lazy_query,
    last_op = "summarise",
    select = select,
    group_by = syms(grps),
    group_vars = groups_out,
    select_operation = "summarise",
    message_summarise = message_summarise
  )
}

summarise_message <- function(grps, .groups, env_caller) {
  verbose <- summarise_verbose(.groups, env_caller)
  n <- length(grps)
  if (!verbose || n <= 1) {
    return(NULL)
  }

  summarise_message <- cli::format_message("{.fun summarise} has grouped output by {.val {grps[-n]}}. You can override using the {.arg .groups} argument.")
}

summarise_verbose <- function(.groups, .env) {
  is.null(.groups) &&
    is_reference(topenv(.env), global_env()) &&
    !identical(getOption("dplyr.summarise.inform"), FALSE)
}
