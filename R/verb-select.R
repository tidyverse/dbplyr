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
  select_can_be_inlined <- select_can_be_inlined(lazy_query, vars)
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

select_can_be_inlined <- function(lazy_query, vars) {
  # all computed columns used for ordering (if any) must be present
  computed_flag <- purrr::map_lgl(lazy_query$select$expr, is_quosure)
  computed_columns <- lazy_query$select$name[computed_flag]

  order_vars <- purrr::map_chr(lazy_query$order_by, as_label)
  ordered_present <- all(intersect(computed_columns, order_vars) %in% vars)

  # if the projection is distinct, it must be bijective
  is_distinct <- is_true(lazy_query$distinct)
  is_bijective_projection <- identical(sort(unname(vars)), op_vars(lazy_query))
  distinct_is_bijective <- !is_distinct || is_bijective_projection

  ordered_present && distinct_is_bijective
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
    function(x) {
      if (is_quosure(x)) {
        quo_is_call(x, "desc", n = 1L)
      } else {
        is_call(x, "desc", n = 1L)
      }
    }
  )

  order <- purrr::map_if(order, is_desc, \(x) call_args(x)[[1L]])
  order <- purrr::map_chr(order, as_name)

  keep <- order %in% names(old2new)
  order[keep] <- syms(old2new[order[keep]])

  order <- purrr::map_if(order, is_desc, \(x) call2("desc", x))
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
