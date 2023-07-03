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
  check_unsupported_arg(id_expand, FALSE)

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
                                    unused_fn = NULL,
                                    error_call = current_env()) {
  spec <- tidyr::check_pivot_spec(spec)

  names_from_cols <- names(spec)[-(1:2)]
  values_from_cols <- vctrs::vec_unique(spec$.value)

  id_cols <- select_wider_id_cols(
    data = data,
    id_cols = {{ id_cols }},
    names_from_cols = names_from_cols,
    values_from_cols = values_from_cols,
    error_call = error_call
  )

  values_fn <- check_list_of_functions(values_fn, values_from_cols, call = error_call)

  unused_cols <- setdiff(colnames(data), c(id_cols, names_from_cols, values_from_cols))
  unused_fn <- check_list_of_functions(unused_fn, unused_cols, call = error_call)
  unused_cols <- names(unused_fn)

  if (is.null(values_fill)) {
    values_fill <- list()
  } else if (is_scalar(values_fill)) {
    values_fill <- rep_named(values_from_cols, list(values_fill))
  } else if (!vctrs::vec_is_list(values_fill)) {
    cli::cli_abort(
      "{.arg values_fill} must be {.code NULL}, a scalar, or a named list, not {.obj_type_friendly {values_fill}}.",
      call = error_call
    )
  }
  values_fill <- values_fill[intersect(names(values_fill), values_from_cols)]

  missing_values <- setdiff(values_from_cols, names(values_fn))
  if (!is_empty(missing_values)) {
    cli_abort("{.arg values_fn} must specify a function for each col in {.arg values_from}")
  }

  n_unused_fn <- length(unused_fn)

  unused_col_expr <- vector("list", length = n_unused_fn)
  names(unused_col_expr) <- unused_cols

  for (i in seq_len(n_unused_fn)) {
    unused_col <- unused_cols[[i]]
    unused_fn_i <- unused_fn[[i]]

    unused_col_expr[[i]] <- resolve_fun(unused_fn_i, sym(unused_col), call = error_call)
  }

  spec_idx <- set_names(vctrs::vec_seq_along(spec), spec$.name)
  pivot_exprs <- with_indexed_errors(
    purrr::map(
      spec_idx,
      ~ build_pivot_wider_exprs(.x, spec, values_fill, values_fn, call = NULL)
    ),
    message = function(cnd) {
      col <- spec[[".value"]][cnd$location]
      cli::format_inline("Can't pivot column {.field {col}}:")
    },
    .error_call = error_call
  )

  non_id_cols <- c(names_from_cols, values_from_cols)
  key_vars <- setdiff(id_cols, non_id_cols)
  data_grouped <- group_by(data, !!!syms(key_vars), .add = TRUE)

  group_names <- group_vars(data_grouped)
  out_nms <- c(group_names, names(pivot_exprs))
  out_nms_repaired <- vctrs::vec_as_names(out_nms, repair = names_repair)

  if (!is_empty(group_names)) {
    out_nms_repaired <- out_nms_repaired[-(1:length(group_names))]
  }
  pivot_exprs <- set_names(pivot_exprs, out_nms_repaired)

  data_grouped %>%
    summarise(
      !!!pivot_exprs,
      !!!unused_col_expr,
      .groups = "drop"
    ) %>%
    group_by(!!!syms(group_vars(data)))
}

utils::globalVariables(c("name", "value"))

build_wider_id_cols_expr <- function(data,
                                     id_cols = NULL,
                                     names_from = name,
                                     values_from = value,
                                     call = caller_env()) {
  # COPIED FROM tidyr
  names_from <- tidyselect::eval_select(
    enquo(names_from),
    data,
    allow_rename = FALSE,
    error_call = error_call
  )

  values_from <- tidyselect::eval_select(
    enquo(values_from),
    data,
    allow_rename = FALSE,
    error_call = error_call
  )

  out <- select_wider_id_cols(
    data = data,
    id_cols = {{ id_cols }},
    names_from_cols = names(names_from),
    values_from_cols = names(values_from),
    error_call = error_call
  )

  expr(c(!!!out))
}

select_wider_id_cols <- function(data,
                                 id_cols = NULL,
                                 names_from_cols = character(),
                                 values_from_cols = character(),
                                 error_call = caller_env()) {
  # COPIED FROM tidyr
  id_cols <- enquo(id_cols)
  sim_data <- tidyselect_data_proxy(data)

  # Remove known non-id-cols so they are never selected
  sim_data <- sim_data[setdiff(names(sim_data), c(names_from_cols, values_from_cols))]

  if (quo_is_null(id_cols)) {
    # Default selects everything in `sim_data` after non-id-cols have been removed
    return(names(sim_data))
  }

  try_fetch(
    id_cols <- tidyselect::eval_select(
      enquo(id_cols),
      sim_data,
      allow_rename = FALSE,
      error_call = error_call
    ),
    vctrs_error_subscript_oob = function(cnd) {
      rethrow_id_cols_oob(cnd, names_from_cols, values_from_cols, error_call)
    }
  )

  names(id_cols)
}

rethrow_id_cols_oob <- function(cnd, names_from_cols, values_from_cols, call) {
  i <- cnd[["i"]]

  check_string(i, .internal = TRUE)

  if (i %in% names_from_cols) {
    stop_id_cols_oob(i, "names_from", call = call)
  } else if (i %in% values_from_cols) {
    stop_id_cols_oob(i, "values_from", call = call)
  } else {
    # Zap this special handler, throw the normal condition
    zap()
  }
}

stop_id_cols_oob <- function(i, arg, call) {
  cli::cli_abort(
    c(
      "`id_cols` can't select a column already selected by `{arg}`.",
      i = "Column `{i}` has already been selected."
    ),
    parent = NA,
    call = call
  )
}

build_pivot_wider_exprs <- function(row_id, spec, values_fill, values_fn, call) {
  values_col <- spec[[".value"]][row_id]
  fill_value <- values_fill[[values_col]]

  keys <- vctrs::vec_slice(spec[, -(1:2)], row_id)
  keys_cond <- purrr::imap(
    keys,
    function(value, name) {
      if (is.na(value)) {
        expr(is.na(!!sym(name)))
      } else {
        expr(!!sym(name) == !!value)
      }
    }
  )
  keys_cond <- purrr::reduce(keys_cond, ~ expr(!!.x & !!.y))

  case_expr <- expr(ifelse(!!keys_cond, !!sym(values_col), !!fill_value))

  agg_fn <- values_fn[[values_col]]
  resolve_fun(agg_fn, case_expr, arg = paste0("values_fn$", values_col), call = call)
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


resolve_fun <- function(x, var, arg = caller_arg(x), call = caller_env()) {
  if (is_formula(x)) {
    .fn_expr <- across_fun(x, env = empty_env(), dots = NULL, fn = "across")
    exec(.fn_expr, var, NULL)
  } else {
    fn_name <- find_fun(x)
    if (is_null(fn_name)) {
      cli_abort("Can't convert {.arg {arg}}, {.code {as_label(x)}}, to a function.", call = call)
    }
    call2(fn_name, var)
  }
}
