#' Expand SQL tables to include all possible combinations of values
#'
#' @description
#' This is a method for the [tidyr::expand] generics. It doesn't sort the
#' result explicitly, so the order might be different to what `expand()`
#' returns for data frames.
#'
#' @param data A lazy data frame backed by a database query.
#' @param ... Specification of columns to expand. See [tidyr::expand] for
#' more details.
#' @inheritParams tibble::as_tibble
#' @inherit arrange.tbl_lazy return
#' @examplesIf rlang::is_installed("tidyr", version = "1.0.0")
#' fruits <- memdb_frame(
#'   type   = c("apple", "orange", "apple", "orange", "orange", "orange"),
#'   year   = c(2010, 2010, 2012, 2010, 2010, 2012),
#'   size = c("XS", "S",  "M", "S", "S", "M"),
#'   weights = rnorm(6)
#' )
#'
#' # All possible combinations ---------------------------------------
#' fruits %>% tidyr::expand(type)
#' fruits %>% tidyr::expand(type, size)
#'
#' # Only combinations that already appear in the data ---------------
#' fruits %>% tidyr::expand(nesting(type, size))
expand.tbl_lazy <- function(data, ..., .name_repair = "check_unique") {
  dots <- purrr::discard(quos(...), quo_is_null)

  if (is_empty(dots)) {
    cli_abort("Must supply variables in `...`")
  }

  distinct_tbl_vars <- purrr::map(dots, extract_expand_dot_vars, call = current_env())

  # now that `nesting()` has been unpacked resolve name conflicts
  out_names <- names(exprs_auto_name(purrr::flatten(distinct_tbl_vars)))
  out_names_repaired <- vctrs::vec_as_names(out_names, repair = .name_repair)

  ns <- lengths(distinct_tbl_vars)
  indices <- vctrs::vec_rep_each(seq_along(distinct_tbl_vars), ns)
  out_names_list <- vctrs::vec_split(out_names_repaired, indices)$val

  distinct_tables <- purrr::map2(
    distinct_tbl_vars, out_names_list,
    ~ {
      args <- set_names(.x, .y)
      distinct(data, !!!args)
    }
  )

  by <- group_vars(data)
  if (is_empty(by)) {
    purrr::reduce(distinct_tables, cross_join)
  } else {
    # In this case a `full_join()` and a `left_join()` produce the same result
    # but a `left_join()` produces much nicer SQL for SQLite
    purrr::reduce(distinct_tables, left_join, by = group_vars(data))
  }
}

extract_expand_dot_vars <- function(dot, call) {
  # ugly hack to deal with `nesting()`
  if (!quo_is_call(dot, name = "nesting", ns = c("", "tidyr"))) {
    return(list(quo_get_expr(dot)))
  }

  x_expr <- quo_get_expr(dot)
  args <- call_args(x_expr)

  repair <- args[[".name_repair"]] %||% "check_unique"
  args[[".name_repair"]] <- NULL

  args_named <- exprs_auto_name(args)
  nms <- vctrs::vec_as_names(names(args_named), repair = repair, call = call)
  exprs(!!!set_names(args_named, nms))
}

#' Complete a SQL table with missing combinations of data
#'
#' Turns implicit missing values into explicit missing values. This is a method
#' for the [tidyr::complete()] generic.
#'
#' @inheritParams expand.tbl_lazy
#' @param fill A named list that for each variable supplies a single value to
#' use instead of NA for missing combinations.
#'
#' @inherit arrange.tbl_lazy return
#'
#' @examplesIf rlang::is_installed("tidyr", version = "1.0.0")
#' df <- memdb_frame(
#'   group = c(1:2, 1),
#'   item_id = c(1:2, 2),
#'   item_name = c("a", "b", "b"),
#'   value1 = 1:3,
#'   value2 = 4:6
#' )
#'
#' df %>% tidyr::complete(group, nesting(item_id, item_name))
#'
#' # You can also choose to fill in missing values
#' df %>% tidyr::complete(group, nesting(item_id, item_name), fill = list(value1 = 0))
complete.tbl_lazy <- function(data, ..., fill = list()) {
  full <- tidyr::expand(data, ...)

  full <- full_join(full, data, by = colnames(full))
  tidyr::replace_na(full, replace = fill)
}

#' Replace NAs with specified values
#'
#' This is a method for the [tidyr::replace_na()] generic.
#'
#' @param data A pair of lazy data frame backed by database queries.
#' @param replace A named list of values, with one value for each column that
#' has NA values to be replaced.
#' @param ... Unused; included for compatibility with generic.
#'
#' @inherit arrange.tbl_lazy return
#'
#' @examplesIf rlang::is_installed("tidyr", version = "1.0.0")
#' df <- memdb_frame(x = c(1, 2, NA), y = c("a", NA, "b"))
#' df %>% tidyr::replace_na(list(x = 0, y = "unknown"))
replace_na.tbl_lazy <- function(data, replace = list(), ...) {
  check_list(replace)
  check_named(replace)
  replace <- replace[names(replace) %in% colnames(data)]

  if (is_empty(replace)) {
    return(data)
  }

  coalesce_expr <- purrr::imap(
    replace,
    function(value, col) {
      quo(coalesce(!!sym(col), !!value))
    }
  )

  mutate(data, !!!coalesce_expr)
}

utils::globalVariables(c("coalesce", "expand", "replace_na"))
