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

  .data$lazy_query <- add_select(.data, syms(new_vars))
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

  .data$lazy_query <- add_select(.data, syms(new_vars))
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

  .data$lazy_query <- add_select(.data, syms(new_vars))
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

add_select <- function(.data, vars, op = c("select", "mutate")) {
  op <- match.arg(op, c("select", "mutate"))
  lazy_query <- .data$lazy_query

  # drop NULLs
  vars <- purrr::discard(vars, ~ (is_quosure(.x) && quo_is_null(.x)) || is.null(.x))
  if (is_identity(vars, names(vars), op_vars(.data))) {
    return(lazy_query)
  }

  symbols <- purrr::keep(vars, is_symbol)
  new2old <- purrr::map_chr(symbols, as_string)
  old2new <- set_names(names(new2old), new2old)

  grps <- op_grps(.data)
  renamed <- grps %in% names(old2new)
  grps[renamed] <- old2new[grps[renamed]]

  if (is_projection(vars)) {
    sel_vars <- purrr::map_chr(vars, as_string)
    names_prev <- op_vars(lazy_query)
    idx <- vctrs::vec_match(sel_vars, names_prev)

    out <- lazy_query
    out$group_vars <- grps

    if (inherits(lazy_query, "lazy_join_query")) {
      out$vars$alias <- names(sel_vars)
      out$vars$x <- lazy_query$vars$x[idx]
      out$vars$y <- lazy_query$vars$y[idx]

      return(out)
    }

    if (inherits(lazy_query, "lazy_semi_join_query")) {
      out$vars <- vctrs::vec_slice(out$vars, idx)
      out$vars$name <- names(sel_vars)

      return(out)
    }

    if (identical(lazy_query$last_op, "select") || identical(lazy_query$last_op, "mutate")) {
      out$select <- vctrs::vec_slice(lazy_query$select, idx)
      out$select$name <- names(vars)

      return(out)
    }
  }

  if (identical(lazy_query$last_op, "select") || identical(lazy_query$last_op, "mutate")) {
    # Special optimisation when applied to pure projection() - this is
    # conservative and we could expand to any op_select() if combined with
    # the logic in get_mutate_layers()
    select <- lazy_query$select
    if (is_pure_projection(select$expr, select$name)) {
      if (op == "select") {
          lazy_query$select <- update_lazy_select(select, vars)
        } else {
          lazy_query$select <- new_lazy_select(
            vars,
            group_vars = op_grps(lazy_query),
            order_vars = op_sort(lazy_query),
            frame = op_frame(lazy_query)
          )
        }
        return(lazy_query)
    }
  }

  lazy_select_query(
    x = lazy_query,
    last_op = op,
    select = vars,
    group_vars = grps
  )
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

fix_call <- function(expr, call = caller_env()) {
  withCallingHandlers(expr, error = function(cnd) {
    cnd$call <- call
    cnd_signal(cnd)
  })
}
