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
#' db %>% select(-y) %>% show_query()
#' db %>% relocate(z) %>% show_query()
#' db %>% rename(first = x, last = z) %>% show_query()
select.tbl_lazy <- function(.data, ...) {
  loc <- tidyselect::eval_select(expr(c(...)), .data)
  loc <- ensure_group_vars(loc, .data, notify = TRUE)
  new_vars <- set_names(colnames(.data)[loc], names(loc))

  .data$lazy_query <- add_select(.data$lazy_query, new_vars)
  .data
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

  dplyr::select(.data, !!!loc)
}

#' Simulate variables to use in tidyselect
#'
#' @param x A lazy table
#' @param drop_groups Should groups be dropped?
#'
#' @return A 0 row tibble with the same columns names, and, if possible, types, as `x`.
#'
#' @export
#' @keywords internal
simulate_vars <- function (x, drop_groups = FALSE) {
  # keep this for now as this might be used by other packages
  UseMethod("simulate_vars")
}

#' @export
simulate_vars.tbl_lazy <- function(x, drop_groups = FALSE) {
  if (drop_groups) {
    vars <- setdiff(op_vars(x), op_grps(x))
  } else {
    vars <- op_vars(x)
  }

  as_tibble(rep_named(vars, list(logical())), .name_repair = "minimal")
}

#' @rdname simulate_vars
#' @export
simulate_vars_is_typed <- function(x) UseMethod("simulate_vars_is_typed")
#' @export
simulate_vars_is_typed.tbl_lazy <- function(x) FALSE

# op_select ---------------------------------------------------------------

add_select <- function(lazy_query, vars) {
  check_character(vars)
  vars_data <- op_vars(lazy_query)

  if (is_identity(syms(vars), names(vars), vars_data)) {
    return(lazy_query)
  }

  lazy_query <- rename_groups(lazy_query, vars)
  lazy_query <- rename_order(lazy_query, vars)

  is_join <- inherits(lazy_query, "lazy_multi_join_query") ||
    inherits(lazy_query, "lazy_rf_join_query") ||
    inherits(lazy_query, "lazy_semi_join_query")
  if (is_join) {
    idx <- vctrs::vec_match(vars, vars_data)

    lazy_query$vars <- vctrs::vec_slice(lazy_query$vars, idx)
    lazy_query$vars$name <- names(vars)
    return(lazy_query)
  }

  is_select <- is_lazy_select_query(lazy_query)
  select_can_be_inlined <- is_bijective_projection(vars, vars_data) || !is_true(lazy_query$distinct)
  if (is_select && select_can_be_inlined) {
    idx <- vctrs::vec_match(vars, vars_data)

    lazy_query$select <- vctrs::vec_slice(lazy_query$select, idx)
    lazy_query$select$name <- names(vars)

    return(lazy_query)
  }

  lazy_select_query(
    x = lazy_query,
    select_operation = "select",
    select = syms(vars)
  )
}

is_bijective_projection <- function(vars, names_prev) {
  vars <- unname(vars)
  identical(sort(vars), names_prev)
}

rename_groups <- function(lazy_query, vars) {
  old2new <- set_names(names(vars), vars)
  grps <- op_grps(lazy_query)
  renamed <- grps %in% names(old2new)
  grps[renamed] <- old2new[grps[renamed]]

  lazy_query$group_vars <- grps
  lazy_query
}

rename_order <- function(lazy_query, vars) {
  old2new <- set_names(names(vars), vars)
  order <- op_sort(lazy_query)

  is_desc <- purrr::map_lgl(
    order,
    ~ if (is_quosure(.x)) {
      quo_is_call(.x, "desc", n = 1L)
    } else {
      is_call(.x, "desc", n = 1L)
    }
  )

  order <- purrr::map_if(order, is_desc, ~ call_args(.x)[[1L]])
  order <- purrr::map_chr(order, as_name)

  keep <- order %in% names(old2new)
  order[keep] <- syms(old2new[order[keep]])

  order <- purrr::map_if(order, is_desc, ~ call2("desc", .x))
  lazy_query$order_vars <- order[keep]
  lazy_query
}

is_projection <- function(exprs) {
  purrr::every(exprs, is_symbol)
}

is_pure_projection <- function(exprs, names) {
  if (!is_projection(exprs)) {
    return(FALSE)
  }

  expr_vars <- purrr::map_chr(unname(exprs), as_string)
  identical(expr_vars, names)
}

is_identity <- function(exprs, names, names_prev) {
  if (!is_pure_projection(exprs, names)) {
    return(FALSE)
  }

  identical(names, names_prev)
}
