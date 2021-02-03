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
  dots <- quos(..., .named = TRUE)
  dots <- partial_eval_dots(dots, vars = op_vars(.data))
  check_summarise_vars(dots)
  check_groups(.groups)

  add_op_single(
    "summarise",
    .data,
    dots = dots,
    args = list(.groups = .groups, env_caller = caller_env())
  )
}

# For each expression, check if it uses any newly created variables
check_summarise_vars <- function(dots) {
  for (i in seq_along(dots)) {
    used_vars <- all_names(get_expr(dots[[i]]))
    cur_vars <- names(dots)[seq_len(i - 1)]

    if (any(used_vars %in% cur_vars)) {
      stop(
        "`", names(dots)[[i]],
        "` refers to a variable created earlier in this summarise().\n",
        "Do you need an extra mutate() step?",
        call. = FALSE
      )
    }
  }
}

check_groups <- function(.groups) {
  if (is_null(.groups)) {
    return()
  }

  if (.groups %in% c("drop_last", "drop", "keep")) {
    return()
  }

  abort(c(
    paste0(
      "`.groups` can't be ", as_label(.groups),
      if (.groups == "rowwise") " in dbplyr"
    ),
    i = 'Possible values are NULL (default), "drop_last", "drop", and "keep"'
  ))
}

#' @export
op_vars.op_summarise <- function(op) {
  c(op_grps(op$x), names(op$dots))
}

#' @export
op_grps.op_summarise <- function(op) {
  grps <- op_grps(op$x)
  .groups <- op$args$.groups %||% "drop_last"

  switch(.groups,
    drop_last = grps[-length(grps)],
    keep = grps,
    drop = character()
  )
}

#' @export
op_sort.op_summarise <- function(op) NULL

#' @export
sql_build.op_summarise <- function(op, con, ...) {
  select_vars <- translate_sql_(op$dots, con, window = FALSE, context = list(clause = "SELECT"))
  group_vars <- op_grps(op$x)
  n <- length(group_vars)

  .groups <- op$args$.groups
  verbose <- summarise_verbose(.groups, op$args$env_caller)
  .groups <- .groups %||% "drop_last"

  if (verbose && n > 1) {
    new_groups <- glue::glue_collapse(paste0("'", group_vars[-n], "'"), sep = ", ")
    summarise_inform("has grouped output by {new_groups}")
  }

  group_vars <- c.sql(ident(group_vars), con = con)
  select_query(
    sql_build(op$x, con),
    select = c.sql(group_vars, select_vars, con = con),
    group_by = group_vars
  )
}

summarise_verbose <- function(.groups, .env) {
  is.null(.groups) &&
    is_reference(topenv(.env), global_env()) &&
    !identical(getOption("dplyr.summarise.inform"), FALSE)
}

summarise_inform <- function(..., .env = parent.frame()) {
  inform(paste0(
    "`summarise()` ", glue(..., .envir = .env), '. You can override using the `.groups` argument.'
  ))
}
