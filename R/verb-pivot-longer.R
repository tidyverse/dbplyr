#' Pivot data from wide to long
#'
#' @description
#' `pivot_longer()` "lengthens" data, increasing the number of rows and
#' decreasing the number of columns. The inverse transformation is
#' `tidyr::pivot_wider()]
#'
#' Learn more in `vignette("pivot", "tidyr")`.
#'
#' While most functionality is identical there are some differences to
#' `pivot_longer()` on local data frames:
#' * the output is sorted differently/not explicitly,
#' * the coercion of mixed column types is left to the database,
#' * `values_ptypes` NOT supported.
#'
#' Note that `build_longer_spec()` and `pivot_longer_spec()` do not work with
#' remote tables.
#'
#' @details
#' The SQL translation basically works as follows:
#'
#' 1. split the specification by its key columns i.e. by variables crammed
#' into the column names.
#' 2. for each part in the splitted specification `transmute()` `data` into the
#' following columns
#'   * id columns i.e. columns that are not pivotted
#'   * key columns
#'   * value columns i.e. columns that are pivotted
#' 3. combine all the parts with `union_all()`
#'
#' @param data A data frame to pivot.
#' @param cols Columns to pivot into longer format.
#' @param ... Additional arguments passed on to methods.
#' @param cols_vary Unsupported; included for compatibility with the generic.
#' @param names_to A string specifying the name of the column to create
#'   from the data stored in the column names of `data`.
#' @param names_prefix A regular expression used to remove matching text
#'   from the start of each variable name.
#' @param names_sep,names_pattern If `names_to` contains multiple values,
#'   these arguments control how the column name is broken up.
#' @param names_repair What happens if the output has invalid column names?
#' @param values_to A string specifying the name of the column to create
#'   from the data stored in cell values. If `names_to` is a character
#'   containing the special `.value` sentinel, this value will be ignored,
#'   and the name of the value column will be derived from part of the
#'   existing column names.
#' @param values_drop_na If `TRUE`, will drop rows that contain only `NA`s
#'   in the `value_to` column.
#' @param names_transform,values_transform A list of column name-function pairs.
#' @param names_ptypes A list of column name-prototype pairs.
#' @param values_ptypes Not supported.
#' @examplesIf rlang::is_installed("tidyr", version = "1.0.0")
#' # See vignette("pivot") for examples and explanation
#'
#' # Simplest case where column names are character data
#' memdb_frame(
#'   id = c("a", "b"),
#'   x = 1:2,
#'   y = 3:4
#' ) %>%
#'   tidyr::pivot_longer(-id)
pivot_longer.tbl_lazy <- function(data,
                                  cols,
                                  ...,
                                  cols_vary,
                                  names_to = "name",
                                  names_prefix = NULL,
                                  names_sep = NULL,
                                  names_pattern = NULL,
                                  names_ptypes = NULL,
                                  names_transform = NULL,
                                  names_repair = "check_unique",
                                  values_to = "value",
                                  values_drop_na = FALSE,
                                  values_ptypes,
                                  values_transform = NULL) {
  if (!is_missing(values_ptypes)) {
    cli_abort("The {.arg values_ptypes} argument is not supported for remote back-ends")
  }
  if (!is_missing(cols_vary)) {
    cli_abort("The {.arg cols_vary} argument is not supported for remote back-ends.")
  }

  rlang::check_dots_empty()

  cols <- enquo(cols)
  spec <- tidyr::build_longer_spec(tidyselect_data_proxy(data), !!cols,
    names_to = names_to,
    values_to = values_to,
    names_prefix = names_prefix,
    names_sep = names_sep,
    names_pattern = names_pattern,
    names_ptypes = names_ptypes,
    names_transform = names_transform,
    error_call = current_env()
  )

  dbplyr_pivot_longer_spec(data, spec,
    names_repair = names_repair,
    values_drop_na = values_drop_na,
    values_transform = values_transform
  )
}

dbplyr_pivot_longer_spec <- function(data,
                                     spec,
                                     names_repair = "check_unique",
                                     values_drop_na = FALSE,
                                     values_transform = NULL) {
  spec <- tidyr::check_pivot_spec(spec)
  # .seq col needed if different input columns are mapped to the same output
  # column
  spec <- deduplicate_spec(spec, data)

  id_cols <- syms(setdiff(colnames(data), spec$.name))
  repair_info <- apply_name_repair_pivot_longer(id_cols, spec, names_repair)
  id_cols <- repair_info$id_cols
  spec <- repair_info$spec

  spec_split <- vctrs::vec_split(spec, spec[, -(1:2)])

  call <- current_env()
  value_names <- unique(spec$.value)
  values_transform <- check_list_of_functions(values_transform, value_names, "values_transform", call)

  nms_map <- tibble(
    name = colnames(spec_split$key),
    name_mapped = ifelse(
      name %in% unlist(spec_split$key),
      paste0("..", name),
      name
    )
  )
  spec_split$key <- set_names(spec_split$key, nms_map$name_mapped)

  data_long_list <- purrr::map(
    vctrs::vec_seq_along(spec_split),
    function(idx) {
      row <- spec_split$val[[idx]][, 1:2]
      keys <- spec_split$key[idx, ]
      keys$.seq <- NULL

      measure_cols_exprs <- get_measure_column_exprs(
        row[[".name"]],
        row[[".value"]],
        values_transform,
        data = data,
        call = call
      )

      transmute(
        data,
        !!!id_cols,
        !!!keys,
        !!!measure_cols_exprs
      )
    }
  )

  data_long <- purrr::reduce(data_long_list, union_all)


  if (values_drop_na) {
    value_cols <- unique(spec$.value)

    data_long <- dplyr::filter_at(
      data_long,
      value_cols,
      dplyr::all_vars(!is.na(.))
    )
  }

  data_long %>%
    rename(!!!tibble::deframe(nms_map))
}

get_measure_column_exprs <- function(name, value, values_transform, data, call) {
  measure_cols <- set_names(syms(name), value)
  purrr::imap(
    measure_cols,
    ~ {
      f_trans <- values_transform[[.y]]

      if (is_null(f_trans)) {
        .x
      } else {
        resolve_fun(f_trans, .x, data, call)
      }
    }
  )
}

apply_name_repair_pivot_longer <- function(id_cols, spec, names_repair) {
  # Calculates how the column names in `pivot_longer()` need to be repaired
  # and applies this to the `id_cols` and the `spec` argument:
  # * The names of `id_cols` are the repaired id column names
  # * The `spec` columns after the third column are renamed to the repaired name
  # * The entries in the `value` column of `spec` are changed to the repaired name

  nms_map_df <- vctrs::vec_rbind(
    tibble(from = "id_cols", name = as.character(id_cols)),
    tibble(from = "measure_cols", name = colnames(spec)[-(1:2)]),
    tibble(from = "value_cols", name = unique(spec[[".value"]]))
  ) %>%
    mutate(name_rep = vctrs::vec_as_names(name, repair = names_repair))
  nms_map <- split(nms_map_df, nms_map_df$from)

  id_cols <- purrr::set_names(id_cols, nms_map$id_cols$name_rep)

  colnames(spec)[-(1:2)] <- nms_map$measure_cols$name_rep

  value_nms_map <- purrr::set_names(
    nms_map$value_cols$name_rep,
    nms_map$value_cols$name
  )
  spec$.value <- dplyr::recode(spec$.value, !!!value_nms_map)

  list(id_cols = id_cols, spec = spec)
}

# The following is copy-pasted from `tidyr`

# nocov start
# Ensure that there's a one-to-one match from spec to data by adding
# a special .seq variable which is automatically removed after pivotting.
deduplicate_spec <- function(spec, df) {
  # COPIED FROM tidyr

  # Ensure each .name has a unique output identifier
  key <- spec[setdiff(names(spec), ".name")]
  if (vctrs::vec_duplicate_any(key)) {
    pos <- vctrs::vec_group_loc(key)$loc
    seq <- vector("integer", length = nrow(spec))
    for (i in seq_along(pos)) {
      seq[pos[[i]]] <- seq_along(pos[[i]])
    }
    spec$.seq <- seq
  }

  # Match spec to data, handling duplicated column names
  col_id <- vctrs::vec_match(names(df), spec$.name)
  has_match <- !is.na(col_id)

  if (!vctrs::vec_duplicate_any(col_id[has_match])) {
    return(spec)
  }

  spec <- vctrs::vec_slice(spec, col_id[has_match])
  # Need to use numeric indices because names only match first
  spec$.name <- seq_along(df)[has_match]

  pieces <- vctrs::vec_split(seq_len(nrow(spec)), col_id[has_match])
  copy <- integer(nrow(spec))
  for (i in seq_along(pieces$val)) {
    idx <- pieces$val[[i]]
    copy[idx] <- seq_along(idx)
  }

  spec$.seq <- copy
  spec
}

check_list_of_functions <- function(x, names, arg, call = caller_env()) {
  # mostly COPIED FROM tidyr
  if (is.null(x)) {
    x <- set_names(list(), character())
  }

  if (!vctrs::vec_is_list(x)) {
    x <- rep_named(names, list(x))
  }

  if (length(x) > 0L && !is_named(x)) {
    cli_abort("All elements of {.arg {arg}} must be named.", call = call)
  }

  if (vctrs::vec_duplicate_any(names(x))) {
    cli_abort("The names of {.arg {arg}} must be unique.", call = call)
  }

  # Silently drop user supplied names not found in the data
  x <- x[intersect(names(x), names)]

  x
}
# nocov end

globalVariables(".")
