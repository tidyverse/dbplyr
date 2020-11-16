#' Pivot data from wide to long
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
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
#' @param ... Additional arguments passed on to methods.
#' @examples
#' # See vignette("pivot") for examples and explanation
#'
#' # Simplest case where column names are character data
#' if (require("tidyr", quietly = TRUE)) {
#'   memdb_frame(
#'     id = c("a", "b"),
#'     x = 1:2,
#'     y = 3:4
#'   ) %>%
#'     pivot_longer(-id)
#' }
pivot_longer.tbl_lazy <- function(data,
                                  cols,
                                  names_to = "name",
                                  names_prefix = NULL,
                                  names_sep = NULL,
                                  names_pattern = NULL,
                                  names_ptypes = list(),
                                  names_transform = list(),
                                  names_repair = "check_unique",
                                  values_to = "value",
                                  values_drop_na = FALSE,
                                  values_ptypes,
                                  values_transform = list(),
                                  ...) {
  if (!is_missing(values_ptypes)) {
    abort("The `values_ptypes` argument is not supported for remote back-ends")
  }

  ellipsis::check_dots_empty()

  cols <- enquo(cols)
  spec <- dbplyr_build_longer_spec(data, !!cols,
    names_to = names_to,
    values_to = values_to,
    names_prefix = names_prefix,
    names_sep = names_sep,
    names_pattern = names_pattern,
    names_ptypes = names_ptypes,
    names_transform = names_transform
  )

  dbplyr_pivot_longer_spec(data, spec,
    names_repair = names_repair,
    values_drop_na = values_drop_na,
    values_transform = values_transform
  )
}

# this is a nearly identical copy of `tidyr::build_longer_spec` adapted to
# work with `tbl_lazy` objects.
dbplyr_build_longer_spec <- function(data, cols,
                                     names_to = "name",
                                     values_to = "value",
                                     names_prefix = NULL,
                                     names_sep = NULL,
                                     names_pattern = NULL,
                                     names_ptypes = NULL,
                                     names_transform = NULL) {
  cn_data <- colnames(data)
  data_tmp <- set_names(character(length(cn_data)), cn_data)
  cols <- tidyselect::eval_select(enquo(cols), data_tmp)

  if (length(cols) == 0) {
    abort(glue::glue("`cols` must select at least one column."))
  }

  if (is.null(names_prefix)) {
    names <- names(cols)
  } else {
    names <- gsub(paste0("^", names_prefix), "", names(cols))
  }

  if (length(names_to) > 1) {
    if (!xor(is.null(names_sep), is.null(names_pattern))) {
      abort(glue::glue(
        "If you supply multiple names in `names_to` you must also supply one",
        " of `names_sep` or `names_pattern`."
      ))
    }

    if (!is.null(names_sep)) {
      names <- str_separate(names, names_to, sep = names_sep)
    } else {
      names <- str_extract(names, names_to, regex = names_pattern)
    }
  } else if (length(names_to) == 0) {
    names <- tibble::new_tibble(x = list(), nrow = length(names))
  } else {
    if (!is.null(names_sep)) {
      abort("`names_sep` can not be used with length-1 `names_to`")
    }
    if (!is.null(names_pattern)) {
      names <- str_extract(names, names_to, regex = names_pattern)[[1]]
    }

    names <- tibble(!!names_to := names)
  }

  if (".value" %in% names_to) {
    values_to <- NULL
  } else {
    vctrs::vec_assert(values_to, ptype = character(), size = 1)
  }

  # optionally, cast variables generated from columns
  cast_cols <- intersect(names(names), names(names_ptypes))
  for (col in cast_cols) {
    names[[col]] <- vctrs::vec_cast(names[[col]], names_ptypes[[col]])
  }

  # transform cols
  coerce_cols <- intersect(names(names), names(names_transform))
  for (col in coerce_cols) {
    f <- as_function(names_transform[[col]])
    names[[col]] <- f(names[[col]])
  }

  out <- tibble(.name = names(cols))
  out[[".value"]] <- values_to
  out <- vctrs::vec_cbind(out, names)
  out
}

dbplyr_pivot_longer_spec <- function(data,
                                     spec,
                                     names_repair = "check_unique",
                                     values_drop_na = FALSE,
                                     values_transform = list()) {
  spec <- check_spec(spec)
  # .seq col needed if different input columns are mapped to the same output
  # column
  spec <- deduplicate_spec(spec, data)

  id_cols <- syms(setdiff(colnames(data), spec$.name))

  spec_split <- vctrs::vec_split(spec, spec[, -(1:2)])
  data_long_list <- purrr::map(
    vctrs::vec_seq_along(spec_split),
    function(idx) {
      row <- spec_split$val[[idx]][, 1:2]
      keys <- spec_split$key[idx, ]
      keys$.seq <- NULL

      # idea copied from `partial_eval_across`
      measure_funs <- syms(purrr::map_chr(values_transform, find_fun))

      measure_cols <- set_names(syms(row$.name), row[[".value"]])
      measure_cols_exprs <- purrr::imap(
        measure_cols,
        ~ {
          f_trans <- measure_funs[[.y]]

          if (is_null(f_trans)) {
            .x
          } else {
            expr((!!f_trans)(!!.x))
          }
        }
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
    # add "." variable locally so that cran doesn't complain about
    # missing visible binding
    . <- NULL

    data_long <- dplyr::filter_at(
      data_long,
      value_cols,
      dplyr::all_vars(!is.na(.))
    )
  }

  return(data_long)
}

# The following is copy-pasted from tidyr
# with the exception of `simplifyPieces()` which I have quickly hacked in R

check_spec <- function(spec) {
  # COPIED FROM tidyr

  # Eventually should just be vec_assert() on partial_frame()
  # Waiting for https://github.com/r-lib/vctrs/issues/198

  if (!is.data.frame(spec)) {
    stop("`spec` must be a data frame", call. = FALSE)
  }

  if (!has_name(spec, ".name") || !has_name(spec, ".value")) {
    stop("`spec` must have `.name` and `.value` columns", call. = FALSE)
  }

  # Ensure .name and .value come first
  vars <- union(c(".name", ".value"), names(spec))
  spec[vars]
}

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
