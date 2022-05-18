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
  sim_data <- simulate_vars(.data)
  sim_data <- group_by(sim_data, !!!syms(group_vars(.data)))
  loc <- tidyselect::eval_select(expr(c(...)), sim_data)
  loc <- ensure_group_vars(loc, sim_data, notify = TRUE)
  new_vars <- set_names(names(sim_data)[loc], names(loc))

  .data$lazy_query <- add_select(.data, syms(new_vars))
  .data
}

ensure_group_vars <- function(loc, data, notify = TRUE) {
  group_loc <- match(group_vars(data), colnames(data))
  missing <- setdiff(group_loc, loc)

  if (length(missing) > 0) {
    vars <- names(data)[missing]
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
  sim_data <- simulate_vars(.data)
  loc <- tidyselect::eval_rename(expr(c(...)), sim_data)

  new_vars <- set_names(names(sim_data), names(sim_data))
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
  cols <- tidyselect::eval_select(enquo(.cols), simulate_vars(.data))

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
  # Hack: We want to use `dplyr::relocate.data.frame()` instead of reimplementing it.
  # Because `relocate()` can rename columns we use an attribute to store the
  # original column position.
  sim_data <- simulate_vars(.data)
  for (i in seq_along(sim_data)) {
    attr(sim_data[[i]], "dbplyr_org_pos") <- i
  }

  new_vars <- dplyr::relocate(
    sim_data,
    ...,
    .before = {{.before}},
    .after = {{.after}}
  )

  old_vars <- colnames(sim_data)
  vars_mapping <- purrr::map_chr(
    new_vars,
    ~ old_vars[[attr(.x, "dbplyr_org_pos")]]
  )

  .data$lazy_query <- add_select(.data, syms(vars_mapping))
  .data
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
  vars <- purrr::discard(vars, ~ is_quosure(.x) && quo_is_null(.x))
  if (selects_same_variables(.data, vars)) {
    return(lazy_query)
  }

  if (length(lazy_query$last_op) == 1 && lazy_query$last_op %in% c("select", "mutate")) {
    # Special optimisation when applied to pure projection() - this is
    # conservative and we could expand to any op_select() if combined with
    # the logic in get_mutate_layers()
    select <- lazy_query$select

    if (purrr::every(vars, is.symbol)) {
      # if current operation is pure projection
      # we can just subset the previous selection
      sel_vars <- purrr::map_chr(vars, as_string)
      lazy_query$select <- update_lazy_select(select, sel_vars)

      return(lazy_query)
    }

    prev_vars <- select$expr
    if (purrr::every(prev_vars, is.symbol)) {
      # if previous operation is pure projection
      sel_vars <- purrr::map_chr(prev_vars, as_string)
      if (all(select$name == sel_vars)) {
        # and there's no renaming
        # we can just ignore the previous step
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
  }

  lazy_select_query(
    x = lazy_query,
    last_op = op,
    select = vars
  )
}

selects_same_variables <- function(x, vars) {
  if (!all(vapply(vars, is_symbol, logical(1)))) {
    return(FALSE)
  }

  if (!identical(op_vars(x), names(vars))) {
    return(FALSE)
  }

  identical(syms(op_vars(x)), unname(vars))
}
