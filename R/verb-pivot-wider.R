#' Pivot data from long to wide
#'
#' `pivot_wider()` "widens" data, increasing the number of columns and
#' decreasing the number of rows. The inverse transformation is
#' `pivot_longer()`.
#' Learn more in `vignette("pivot", "tidyr")`.
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
#' @param names_repair What happens if the output has invalid column names?
#' @param values_fill Optionally, a (scalar) value that specifies what each
#'   `value` should be filled in with when missing.
#' @param values_fn A function, the default is `max()`, applied to the `value`
#' in each cell in the output. In contrast to local data frames it must not be
#' `NULL`.
#' @param ... Unused; included for compatibility with generic.
#'
#' @examples
#' if (require("tidyr", quietly = TRUE)) {
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
#' }
pivot_wider.tbl_lazy <- function(data,
                                 id_cols = NULL,
                                 names_from = name,
                                 names_prefix = "",
                                 names_sep = "_",
                                 names_glue = NULL,
                                 names_sort = FALSE,
                                 names_repair = "check_unique",
                                 values_from = value,
                                 values_fill = NULL,
                                 values_fn = max,
                                 ...
                                 ) {
  ellipsis::check_dots_empty()
  names_from <- enquo(names_from)
  values_from <- enquo(values_from)

  spec <- dbplyr_build_wider_spec(data,
    names_from = !!names_from,
    values_from = !!values_from,
    names_prefix = names_prefix,
    names_sep = names_sep,
    names_glue = names_glue,
    names_sort = names_sort
  )

  id_cols <- enquo(id_cols)
  dbplyr_pivot_wider_spec(data, spec, !!id_cols,
    names_repair = names_repair,
    values_fill = values_fill,
    values_fn = values_fn
  )
}

dbplyr_build_wider_spec <- function(data,
                                    names_from = name,
                                    values_from = value,
                                    names_prefix = "",
                                    names_sep = "_",
                                    names_glue = NULL,
                                    names_sort = FALSE) {
  if (!inherits(data, "tbl_sql")) {
    error_message <- c(
      "`dbplyr_build_wider_spec()` doesn't work with local lazy tibbles.",
      i = "Use `memdb_frame()` together with `show_query()` to see the SQL code."
    )

    abort(error_message)
  }

  # prepare a minimal local tibble we can pass to `tidyr::build_wider_spec`
  # 1. create a tibble with unique values in the `names_from` column
  # row_ids <- vec_unique(data[names_from])
  sim_data <- simulate_vars(data)
  names_from <- tidyselect::eval_select(enquo(names_from), sim_data) %>% names()
  distinct_data <- collect(distinct(data, !!!syms(names_from)))

  # 2. add `values_from` column
  values_from <- tidyselect::eval_select(enquo(values_from), sim_data) %>% names()
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
    names_sort = names_sort
  )
}

dbplyr_pivot_wider_spec <- function(data,
                                    spec,
                                    names_repair = "check_unique",
                                    id_cols = NULL,
                                    values_fill = NULL,
                                    values_fn = max) {
  spec <- check_spec(spec)

  if (is.null(values_fn)) {
    abort(c(
      "`values_fn` must not be NULL",
      i = "`values_fn` must be a function or a named list of functions"
    ))
  }
  if (is.function(values_fn)) {
    values_fn <- rep_named(unique(spec$.value), list(values_fn))
    values_fn <- purrr::map_chr(values_fn, find_fun)
  }

  if (is_scalar(values_fill)) {
    values_fill <- rep_named(unique(spec$.value), list(values_fill))
  }
  if (!is.null(values_fill) && !is.list(values_fill)) {
    abort("`values_fill` must be NULL, a scalar, or a named list")
  }

  values <- vctrs::vec_unique(spec$.value)
  spec_cols <- c(names(spec)[-(1:2)], values)

  id_cols <- enquo(id_cols)
  if (!quo_is_null(id_cols)) {
    cn <- set_names(colnames(data))
    key_vars <- names(tidyselect::eval_select(enquo(id_cols), cn))
  } else {
    key_vars <- tbl_vars(data)
  }
  key_vars <- setdiff(key_vars, spec_cols)

  key_col <- sym(names(spec)[3])
  pivot_exprs <- purrr::map(
    vctrs::vec_seq_along(spec),
    function(row) {
      key <- spec[[3]][row]
      values_col <- spec[[".value"]][row]

      fill_value <- values_fill[[values_col]]
      case_expr <- expr(ifelse(!!key_col == !!key, !!sym(values_col), !!fill_value))

      agg_fn <- values_fn[[values_col]]
      expr((!!agg_fn)(!!case_expr, na.rm = TRUE))
    }
  ) %>%
    set_names(spec$.name)

  data_grouped <- group_by(data, !!!syms(key_vars), .add = TRUE)

  group_names <- group_vars(data_grouped)
  out_nms <- c(group_names, names(pivot_exprs))
  out_nms_repaired <- vctrs::vec_as_names(out_nms, repair = names_repair)

  if (!is_empty(group_names)) {
    out_nms_repaired <- out_nms_repaired[-(1:length(group_names))]
  }
  pivot_exprs <- set_names(pivot_exprs, out_nms_repaired)

  data_grouped %>%
    summarise(!!!pivot_exprs, .groups = "drop") %>%
    group_by(!!!syms(group_vars(data)))
}

globalVariables(c("name", "value"))

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
