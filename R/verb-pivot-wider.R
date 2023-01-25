#' Pivot data from long to wide
#'
#' @description
#' `pivot_wider()` "widens" data, increasing the number of columns and
#' decreasing the number of rows. The inverse transformation is
#' `pivot_longer()`.
#' Learn more in `vignette("pivot", "tidyr")`.
#'
#' Note that `pivot_wider()` is not and cannot be lazy because we need to look
#' at the data to figure out what the new column names will be.
#'
#' @details
#' The big difference to `pivot_wider()` for local data frames is that
#' `values_fn` must not be `NULL`. By default it is `max()` which yields
#' the same results as for local data frames if the combination of `id_cols`
#' and `value` column uniquely identify an observation.
#' Mind that you also do not get a warning if an observation is not uniquely
#' identified.
#'
#' The translation to SQL code basically works as follows:
#'
#' 1. Get unique keys in `names_from` column.
#' 2. For each key value generate an expression of the form:
#'     ```sql
#'     value_fn(
#'       CASE WHEN (`names from column` == `key value`)
#'       THEN (`value column`)
#'       END
#'     ) AS `output column`
#'     ```
#' 3. Group data by id columns.
#' 4. Summarise the grouped data with the expressions from step 2.
#'
#' @param data A lazy data frame backed by a database query.
#' @param id_cols A set of columns that uniquely identifies each observation.
#' @param id_expand Unused; included for compatibility with the generic.
#' @param names_from,values_from A pair of
#'   arguments describing which column (or columns) to get the name of the
#'   output column (`names_from`), and which column (or columns) to get the
#'   cell values from (`values_from`).
#'
#'   If `values_from` contains multiple values, the value will be added to the
#'   front of the output column.
#' @param names_prefix String added to the start of every variable name.
#' @param names_sep If `names_from` or `values_from` contains multiple
#'   variables, this will be used to join their values together into a single
#'   string to use as a column name.
#' @param names_glue Instead of `names_sep` and `names_prefix`, you can supply
#'   a glue specification that uses the `names_from` columns (and special
#'   `.value`) to create custom column names.
#' @param names_sort Should the column names be sorted? If `FALSE`, the default,
#'   column names are ordered by first appearance.
#' @param names_vary When `names_from` identifies a column (or columns) with
#'   multiple unique values, and multiple `values_from` columns are provided,
#'   in what order should the resulting column names be combined?
#'
#'   - `"fastest"` varies `names_from` values fastest, resulting in a column
#'     naming scheme of the form: `value1_name1, value1_name2, value2_name1,
#'     value2_name2`. This is the default.
#'
#'   - `"slowest"` varies `names_from` values slowest, resulting in a column
#'     naming scheme of the form: `value1_name1, value2_name1, value1_name2,
#'     value2_name2`.
#' @param names_repair What happens if the output has invalid column names?
#' @param names_expand Should the values in the `names_from` columns be expanded
#'   by [expand()] before pivoting? This results in more columns, the output
#'   will contain column names corresponding to a complete expansion of all
#'   possible values in `names_from`. Additionally, the column names will be
#'   sorted, identical to what `names_sort` would produce.
#' @param values_fill Optionally, a (scalar) value that specifies what each
#'   `value` should be filled in with when missing.
#' @param values_fn A function, the default is `max()`, applied to the `value`
#' in each cell in the output. In contrast to local data frames it must not be
#' `NULL`.
#' @param unused_fn Optionally, a function applied to summarize the values from
#'   the unused columns (i.e. columns not identified by `id_cols`,
#'   `names_from`, or `values_from`).
#'
#'   The default drops all unused columns from the result.
#'
#'   This can be a named list if you want to apply different aggregations
#'   to different unused columns.
#'
#'   `id_cols` must be supplied for `unused_fn` to be useful, since otherwise
#'   all unspecified columns will be considered `id_cols`.
#'
#'   This is similar to grouping by the `id_cols` then summarizing the
#'   unused columns using `unused_fn`.
#' @param ... Unused; included for compatibility with generic.
#'
#' @examplesIf rlang::is_installed("tidyr", version = "1.0.0")
#' memdb_frame(
#'   id = 1,
#'   key = c("x", "y"),
#'   value = 1:2
#' ) %>%
#'   tidyr::pivot_wider(
#'     id_cols = id,
#'     names_from = key,
#'     values_from = value
#'   )
pivot_wider.tbl_lazy <- function(data,
                                 ...,
                                 id_cols = NULL,
                                 id_expand = FALSE,
                                 names_from = name,
                                 names_prefix = "",
                                 names_sep = "_",
                                 names_glue = NULL,
                                 names_sort = FALSE,
                                 names_vary = "fastest",
                                 names_expand = FALSE,
                                 names_repair = "check_unique",
                                 values_from = value,
                                 values_fill = NULL,
                                 values_fn = ~ max(.x, na.rm = TRUE),
                                 unused_fn = NULL) {
  rlang::check_dots_empty()
  if (!is_false(id_expand)) {
    cli_abort("{.code id_expand = TRUE} isn't supported by dbplyr.")
  }

  names_from <- enquo(names_from)
  values_from <- enquo(values_from)

  spec <- dbplyr_build_wider_spec(data,
    names_from = !!names_from,
    values_from = !!values_from,
    names_prefix = names_prefix,
    names_sep = names_sep,
    names_glue = names_glue,
    names_sort = names_sort,
    names_vary = names_vary,
    names_expand = names_expand,
    error_call = current_env()
  )

  id_cols <- build_wider_id_cols_expr(
    data = data,
    id_cols = {{id_cols}},
    names_from = !!names_from,
    values_from = !!values_from
  )

  dbplyr_pivot_wider_spec(
    data = data,
    spec = spec,
    id_cols = !!id_cols,
    names_repair = names_repair,
    values_fill = values_fill,
    values_fn = values_fn,
    unused_fn = unused_fn
  )
}

dbplyr_build_wider_spec <- function(data,
                                    names_from = name,
                                    values_from = value,
                                    names_prefix = "",
                                    names_sep = "_",
                                    names_glue = NULL,
                                    names_sort = FALSE,
                                    names_vary = "fastest",
                                    names_expand = FALSE,
                                    error_call = current_env()) {
  if (!inherits(data, "tbl_sql")) {
    cli_abort(c(
      "{.fun dbplyr_build_wider_spec} doesn't work with local lazy tibbles.",
      i = "Use {.fun memdb_frame} together with {.fun show_query} to see the SQL code."
    ))
  }

  # prepare a minimal local tibble we can pass to `tidyr::build_wider_spec`
  # 1. create a tibble with unique values in the `names_from` column
  # row_ids <- vec_unique(data[names_from])
  names_from <- tidyselect::eval_select(enquo(names_from), data) %>% names()
  if (is_empty(names_from)) {
    cli_abort("{.arg names_from} must select at least one column.")
  }
  distinct_data <- collect(distinct(data, !!!syms(names_from)))

  # 2. add `values_from` column
  values_from <- tidyselect::eval_select(enquo(values_from), data) %>% names()
  if (is_empty(values_from)) {
    cli_abort("{.arg values_from} must select at least one column.")
  }
  dummy_data <- vctrs::vec_cbind(
    distinct_data,
    !!!rlang::rep_named(values_from, list(TRUE)),
    .name_repair = "check_unique"
  )

  tidyr::build_wider_spec(dummy_data,
    names_from = !!enquo(names_from),
    values_from = !!enquo(values_from),
    names_prefix = names_prefix,
    names_sep = names_sep,
    names_glue = names_glue,
    names_sort = names_sort,
    names_vary = names_vary,
    names_expand = names_expand,
    error_call = error_call
  )
}

dbplyr_pivot_wider_spec <- function(data,
                                    spec,
                                    names_repair = "check_unique",
                                    id_cols = NULL,
                                    values_fill = NULL,
                                    values_fn = ~ max(.x, na.rm = TRUE),
                                    unused_fn = NULL) {
  input <- data

  spec <- tidyr::check_pivot_spec(spec)

  names_from_cols <- names(spec)[-(1:2)]
  values_from_cols <- vctrs::vec_unique(spec$.value)
  non_id_cols <- c(names_from_cols, values_from_cols)

  id_cols <- select_wider_id_cols(
    data = data,
    id_cols = {{id_cols}},
    non_id_cols = non_id_cols
  )

  if (is.null(values_fill)) {
    values_fill <- list()
  }
  if (is_scalar(values_fill)) {
    values_fill <- rep_named(values_from_cols, list(values_fill))
  }
  if (!vctrs::vec_is_list(values_fill)) {
    cli_abort("{.arg values_fill} must be NULL, a scalar, or a named list")
  }
  values_fill <- values_fill[intersect(names(values_fill), values_from_cols)]

  call <- current_env()
  values_fn <- check_list_of_functions(values_fn, values_from_cols, "values_fn", call = call)
  missing_values <- setdiff(values_from_cols, names(values_fn))
  if (!is_empty(missing_values)) {
    cli_abort("{.arg values_fn} must specify a function for each col in {.arg values_from}")
  }

  pivot_exprs <- purrr::map(
    set_names(vctrs::vec_seq_along(spec), spec$.name),
    ~ build_pivot_wider_exprs(.x, spec, values_fill, values_fn, data, call = call)
  )

  key_vars <- setdiff(id_cols, non_id_cols)
  data_grouped <- group_by(data, !!!syms(key_vars), .add = TRUE)

  group_names <- group_vars(data_grouped)
  out_nms <- c(group_names, names(pivot_exprs))
  out_nms_repaired <- vctrs::vec_as_names(out_nms, repair = names_repair)

  if (!is_empty(group_names)) {
    out_nms_repaired <- out_nms_repaired[-(1:length(group_names))]
  }
  pivot_exprs <- set_names(pivot_exprs, out_nms_repaired)

  unused_cols <- setdiff(colnames(data), c(id_cols, non_id_cols))
  unused_fn <- check_list_of_functions(unused_fn, unused_cols, "unused_fn")
  unused_col_expr <- purrr::imap(unused_fn, ~ resolve_fun(.x, sym(.y), data, call = call))

  data_grouped %>%
    summarise(
      !!!pivot_exprs,
      !!!unused_col_expr,
      .groups = "drop"
    ) %>%
    group_by(!!!syms(group_vars(data)))
}

globalVariables(c("name", "value"))

build_wider_id_cols_expr <- function(data,
                                     id_cols = NULL,
                                     names_from = name,
                                     values_from = value,
                                     call = caller_env()) {
  # COPIED FROM tidyr
  # TODO: Use `allow_rename = FALSE`.
  # Requires https://github.com/r-lib/tidyselect/issues/225.
  names_from <- names(tidyselect::eval_select(enquo(names_from), data, error_call = call))
  values_from <- names(tidyselect::eval_select(enquo(values_from), data, error_call = call))
  non_id_cols <- c(names_from, values_from)

  out <- select_wider_id_cols(
    data = data,
    id_cols = {{id_cols}},
    non_id_cols = non_id_cols,
    call = call
  )

  expr(c(!!!out))
}

build_pivot_wider_exprs <- function(row_id, spec, values_fill, values_fn, data, call) {
  values_col <- spec[[".value"]][row_id]
  fill_value <- values_fill[[values_col]]

  keys <- vctrs::vec_slice(spec[, -(1:2)], row_id)
  keys_cond <- purrr::imap(
    keys,
    function(value, name) {
      expr(!!sym(name) == !!value)
    }
  ) %>%
    purrr::reduce(~ expr(!!.x & !!.y))

  case_expr <- expr(ifelse(!!keys_cond, !!sym(values_col), !!fill_value))

  agg_fn <- values_fn[[values_col]]
  resolve_fun(agg_fn, case_expr, data = data, call = call)
}

select_wider_id_cols <- function(data,
                                 id_cols = NULL,
                                 non_id_cols = character(),
                                 call = caller_env()) {
  # COPIED FROM tidyr
  id_cols <- enquo(id_cols)
  sim_data <- tidyselect_data_proxy(data)

  # Remove known non-id-cols so they are never selected
  sim_data <- sim_data[setdiff(names(sim_data), non_id_cols)]

  if (quo_is_null(id_cols)) {
    names(sim_data)
  } else {
    # TODO: Use `allow_rename = FALSE`.
    # Requires https://github.com/r-lib/tidyselect/issues/225.
    names(tidyselect::eval_select(enquo(id_cols), sim_data, error_call = call))
  }
}

is_scalar <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  }

  if (is.list(x)) {
    (length(x) == 1) && !have_name(x)
  } else {
    length(x) == 1
  }
}


resolve_fun <- function(x, var, data, call = caller_env()) {
  if (is_formula(x)) {
    .fn_expr <- across_fun(x, env = empty_env(), data = data, dots = NULL, fn = "across")
    exec(.fn_expr, var, NULL)
  } else {
    fn_name <- find_fun(x)
    if (is_null(fn_name)) {
      cli_abort("Can't convert to a function.", call = call)
    }
    call2(fn_name, var)
  }
}
