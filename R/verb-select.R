#' Subset, rename, and reorder columns using their names
#'
#' @description
#' These are methods for the dplyr [select()], [rename()], and [relocate()]
#' generics. They generate the `SELECT` clause of the SQL query.
#'
#' These functions do not support predicate functions, i.e. you can
#' not use `where(is.numeric)` to select all numeric variables.
#'
#' @inheritParams arrange.tbl_lazy
#' @inheritParams dplyr::select
#' @export
#' @importFrom dplyr select
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' db <- memdb_frame(x = 1, y = 2, z = 3)
#' db |> select(-y) |> show_query()
#' db |> relocate(z) |> show_query()
#' db |> rename(first = x, last = z) |> show_query()
select.tbl_lazy <- function(.data, ...) {
  loc <- tidyselect::eval_select(expr(c(...)), .data)
  loc <- ensure_group_vars(loc, .data, notify = TRUE)
  new_vars <- set_names(colnames(.data)[loc], names(loc))

  .data$lazy_query <- add_select(.data$lazy_query, new_vars)
  .data
}

#' @rdname select.tbl_lazy
#' @importFrom dplyr rename
#' @export
rename.tbl_lazy <- function(.data, ...) {
  loc <- tidyselect::eval_rename(expr(c(...)), .data)

  new_vars <- set_names(colnames(.data), colnames(.data))
  names(new_vars)[loc] <- names(loc)

  .data$lazy_query <- add_select(.data$lazy_query, new_vars)
  .data
}

#' @rdname select.tbl_lazy
#' @importFrom dplyr rename_with
#' @importFrom tidyselect everything
#' @inheritParams dplyr::rename_with
#' @export
rename_with.tbl_lazy <- function(.data, .fn, .cols = everything(), ...) {
  .fn <- as_function(.fn)
  cols <- tidyselect::eval_select(enquo(.cols), .data)

  new_vars <- set_names(op_vars(.data))
  names(new_vars)[cols] <- .fn(new_vars[cols], ...)

  .data$lazy_query <- add_select(.data$lazy_query, new_vars)
  .data
}

#' @rdname select.tbl_lazy
#' @importFrom dplyr relocate
#' @inheritParams dplyr::relocate
#' @export
relocate.tbl_lazy <- function(.data, ..., .before = NULL, .after = NULL) {
  loc <- tidyselect::eval_relocate(
    expr(c(...)),
    data = .data,
    before = enquo(.before),
    after = enquo(.after),
    before_arg = ".before",
    after_arg = ".after"
  )

  new_vars <- set_names(colnames(.data)[loc], names(loc))

  .data$lazy_query <- add_select(.data$lazy_query, new_vars)
  .data
}

# op_select ---------------------------------------------------------------

add_select <- function(lazy_query, vars) {
  check_character(vars)
  vars_data <- op_vars(lazy_query)

  if (is_identity(syms(vars), names(vars), vars_data)) {
    return(lazy_query)
  }

  # Update grouping/ordering with new variable names
  lazy_query$group_vars <- rename_groups(op_grps(lazy_query), vars)
  lazy_query$order_vars <- rename_order(op_sort(lazy_query), vars)

  is_join <- inherits(lazy_query, "lazy_multi_join_query") ||
    inherits(lazy_query, "lazy_rf_join_query") ||
    inherits(lazy_query, "lazy_semi_join_query")
  if (is_join) {
    idx <- vctrs::vec_match(vars, vars_data)

    lazy_query$vars <- vctrs::vec_slice(lazy_query$vars, idx)
    lazy_query$vars$name <- names(vars)
    return(lazy_query)
  }

  if (can_inline_select(lazy_query, vars)) {
    idx <- vctrs::vec_match(vars, vars_data)

    lazy_query$select <- vctrs::vec_slice(lazy_query$select, idx)
    lazy_query$select$name <- names(vars)
    lazy_query
  } else {
    lazy_select_query(
      x = lazy_query,
      select_operation = "select",
      select = syms(vars)
    )
  }
}

# select() modifies the SELECT clause
# * ORDER BY can reference SELECT columns by name
#   => computed columns used in ORDER BY must be preserved
# * DISTINCT operates on the SELECT result
#   => can only inline bijective projections (same columns, different order)
can_inline_select <- function(lazy_query, vars) {
  if (!is_lazy_select_query(lazy_query)) {
    return(FALSE)
  }

  is_mutate <- !purrr::map_lgl(lazy_query$select$expr, is_symbol)
  computed_columns <- lazy_query$select$name[is_mutate]
  order_vars <- purrr::map_chr(lazy_query$order_by, as_label)
  ordered_present <- all(intersect(computed_columns, order_vars) %in% vars)
  if (!ordered_present) {
    return(FALSE)
  }

  if (is_true(lazy_query$distinct)) {
    identical(sort(unname(vars)), op_vars(lazy_query))
  } else {
    TRUE
  }
}

rename_groups <- function(group_vars, select_vars) {
  old2new <- set_names(names(select_vars), select_vars)
  renamed <- group_vars %in% names(old2new)
  group_vars[renamed] <- old2new[group_vars[renamed]]
  group_vars
}

rename_order <- function(order_vars, select_vars) {
  # Drop any ordering variables not used in the output
  order_names <- purrr::map_chr(order_vars, \(var) all_names(var)[[1]])
  order_vars <- order_vars[order_names %in% select_vars]

  # Rename the remaining
  order_vars[] <- replace_sym(order_vars, select_vars, syms(names(select_vars)))
  order_vars
}

# Helpers ----------------------------------------------------------------------

# Selection, rename, or relocation
is_projection <- function(exprs) {
  purrr::every(exprs, is_symbol)
}

# Selection or relocation
is_pure_projection <- function(exprs, names) {
  if (!is_projection(exprs)) {
    return(FALSE)
  }

  expr_vars <- purrr::map_chr(unname(exprs), as_string)
  identical(expr_vars, names)
}

# Selects all variables
is_identity <- function(exprs, names, names_prev) {
  if (!is_pure_projection(exprs, names)) {
    return(FALSE)
  }

  identical(names, names_prev)
}

ensure_group_vars <- function(loc, data, notify = TRUE) {
  group_loc <- match(group_vars(data), colnames(data))
  missing <- setdiff(group_loc, loc)

  if (length(missing) > 0) {
    vars <- colnames(data)[missing]
    if (notify) {
      cli::cli_inform("Adding missing grouping variables: {.var {vars}}")
    }
    loc <- c(set_names(missing, vars), loc)
  }

  loc
}
