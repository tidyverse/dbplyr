#' Pivot data from long to wide
#'
#' @details
#' * always needs to use a `values_fn` -> no warning/error on duplicate keys
#'
#' @param data
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
#' @param names_repair TODO
#' @param values_fill Optionally, a (scalar) value that specifies what each
#'   `value` should be filled in with when missing.
#' @param values_fn A function applied to the `value` in each cell
#'   in the output.
#' @param ... Not supported.
#'
#' @examples
#' if (require("tidyr", quietly = TRUE)) {
#'   tbl_memdb(us_rent_income) %>%
#'     pivot_wider(names_from = variable, values_from = c(estimate, moe))
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
                                 values_fn = NULL,
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
  cn_data <- colnames(data)
  data_tmp <- set_names(character(length(cn_data)), cn_data)
  names_from <- names(tidyselect::eval_select(enquo(names_from), data_tmp))
  values_from <- tidyselect::eval_select(enquo(values_from), data_tmp)

  row_ids <- collect(distinct(data, !!!syms(names_from)))
  if (names_sort) {
    row_ids <- vctrs::vec_sort(row_ids)
  }

  row_names <- exec(paste, !!!row_ids, sep = names_sep)

  out <- tibble(
    .name = paste0(names_prefix, row_names)
  )

  if (length(values_from) == 1) {
    out$.value <- names(values_from)
  } else {
    out <- vctrs::vec_repeat(out, times = vctrs::vec_size(values_from))
    out$.value <- vctrs::vec_repeat(names(values_from), each = vctrs::vec_size(row_ids))
    out$.name <- paste0(out$.value, names_sep, out$.name)

    row_ids <- vctrs::vec_repeat(row_ids, times = vctrs::vec_size(values_from))
  }

  out <- vctrs::vec_cbind(out, as_tibble(row_ids), .name_repair = "minimal")
  if (!is.null(names_glue)) {
    out$.name <- as.character(glue::glue_data(out, names_glue))
  }

  out
}

dbplyr_pivot_wider_spec <- function(data,
                                    spec,
                                    names_repair = "check_unique",
                                    id_cols = NULL,
                                    values_fill = NULL,
                                    values_fn = NULL) {
  # spec <- check_spec(spec)

  if (is.function(values_fn)) {
    values_fn <- rep_named(unique(spec$.value), list(values_fn))
  }
  if (!is.null(values_fn) && !is.list(values_fn)) {
    abort("`values_fn` must be a NULL, a function, or a named list")
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

  value_cols <- unique(spec[[".value"]])
  values_fn <- values_fn %||% max
  if (is.function(values_fn)) {
    values_fn <- find_fun(values_fn)
    values_fn <- set_names(rep_along(value_cols, values_fn), value_cols)
  } else if (is.list(values_fn)) {
    values_fn <- purrr::map_chr(values_fn, find_fun)
    # TODO error if aggregate function is missing for some cols
  } else if (is.character(values_fn)) {
    # as is
    values_fn <- set_names(rep_along(value_cols, values_fn), value_cols)
  } else {
    abort("Unsupported `.fns` for dbplyr::across()")
  }

  pivot_exprs <- purrr::map(
    vctrs::vec_seq_along(spec),
    function(row) {
      key <- spec[[3]][row]
      key_col <- sym(names(spec)[3])
      values_col <- spec[[".value"]][row]

      fill_value <- values_fill[[values_col]]

      agg_fn <- values_fn[[values_col]]
      case_expr <- expr(ifelse(!!key_col == !!key, !!sym(values_col), !!fill_value))
      expr((!!agg_fn)(!!case_expr, na.rm = TRUE))
    }
  ) %>%
    set_names(spec$.name)

  data %>%
    group_by(!!!syms(key_vars)) %>%
    summarise(!!!pivot_exprs) %>%
    ungroup() %>%
    group_by(!!!syms(group_vars(data)))
}

globalVariables(c("name", "value"))


# move to other file ------------------------------------------------------

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
