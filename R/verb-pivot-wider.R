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

check_null_pivot_args <- function(..., msg = "The `{arg}` argument is not supported for remote back-ends") {
  vars <- enquos(...)
  purrr::imap(
    vars,
    ~ assert_that(
      is.null(quo_get_expr(.x)),
      msg = sub("\\{arg\\}", .y, msg)
    )
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

  value_specs <- unname(split(spec, spec$.value))
  # value_specs <- vctrs::vec_split(spec, spec$.value)

  pivot_exprs <- purrr::map(
    vctrs::vec_seq_along(spec),
    function(row) {
      key <- spec[[3]][row]
      key_col <- sym(names(spec)[3])
      values_col <- sym(spec[[".value"]][row])

      fill_value <- values_fill[[values_col]]

      # TODO
      # values_fn -> ???
      expr(max(ifelse(!!key_col == !!key, !!values_col, !!fill_value), na.rm = TRUE))
    }
  ) %>%
    set_names(spec$.name)

  data %>%
    group_by(!!!syms(key_vars)) %>%
    summarise(!!!pivot_exprs) %>%
    ungroup() %>%
    group_by(!!!syms(group_vars(data)))
  #
  # # recreate desired column order
  # # https://github.com/r-lib/vctrs/issues/227
  # if (all(spec$.name %in% names(out))) {
  #   out <- out[c(names(rows), spec$.name)]
  # }
  #
  # reconstruct_tibble(data, out)
}

globalVariables(c("name", "value"))


# move to other file ------------------------------------------------------

# Not a great name as it now also casts
vals_dedup <- function(key, val, value, summarize = NULL) {

  if (is.null(summarize)) {
    if (!vec_duplicate_any(key)) {
      return(list(key = key, val = val))
    }

    warn(glue::glue(
      "Values are not uniquely identified; output will contain list-cols.\n",
      "* Use `values_fn = list` to suppress this warning.\n",
      "* Use `values_fn = length` to identify where the duplicates arise\n",
      "* Use `values_fn = {{summary_fun}}` to summarise duplicates"
    ))
  }

  out <- vec_split(val, key)
  if (!is.null(summarize) && !identical(summarize, list)) {
    summarize <- as_function(summarize)
    # This is only correct if `values_fn` always returns a single value
    # Needs https://github.com/r-lib/vctrs/issues/183
    out$val <- vec_c(!!!map(out$val, summarize))
  }

  out
}

# Wrap a "rectangular" vector into a data frame
wrap_vec <- function(vec, names) {
  ncol <- length(names)
  nrow <- vec_size(vec) / ncol
  out <- set_names(vec_init(list(), ncol), names)
  for (i in 1:ncol) {
    out[[i]] <- vec_slice(vec, ((i - 1) * nrow + 1):(i * nrow))
  }

  as_tibble(out)
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
