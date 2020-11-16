#' Pivot data from wide to long
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#' `pivot_longer()` "lengthens" data, increasing the number of rows and
#' decreasing the number of columns. The inverse transformation is
#' `pivot_wider()`
#'
#' Learn more in `vignette("pivot")`.
#'
#' @details
#' `pivot_longer()` is an updated approach to [tidyr::gather()], designed to be both
#' simpler to use and to handle more use cases. We recommend you use
#' `pivot_longer()` for new code; `gather()` isn't going away but is no longer
#' under active development.
#'
#' @param data A data frame to pivot.
#' @param cols Columns to pivot into
#'   longer format.
#' @param names_to A string specifying the name of the column to create
#'   from the data stored in the column names of `data`.
#'
#'   Can be a character vector, creating multiple columns, if `names_sep`
#'   or `names_pattern` is provided. In this case, there are two special
#'   values you can take advantage of:
#'
#'   * `NA` will discard that component of the name.
#'   * `.value` indicates that component of the name defines the name of the
#'     column containing the cell values, overriding `values_to`.
#' @param names_prefix A regular expression used to remove matching text
#'   from the start of each variable name.
#' @param names_sep,names_pattern If `names_to` contains multiple values,
#'   these arguments control how the column name is broken up.
#'
#'   `names_sep` takes the same specification as [tidyr::separate()], and can either
#'   be a numeric vector (specifying positions to break on), or a single string
#'   (specifying a regular expression to split on).
#'
#'   `names_pattern` takes the same specification as [tidyr::extract()], a regular
#'   expression containing matching groups (`()`).
#'
#'   If these arguments do not give you enough control, use
#'   `pivot_longer_spec()` to create a spec object and process manually as
#'   needed.
#' @param names_repair What happens if the output has invalid column names?
#'   The default, `"check_unique"` is to error if the columns are duplicated.
#'   Use `"minimal"` to allow duplicates in the output, or `"unique"` to
#'   de-duplicated by adding numeric suffixes. See [vctrs::vec_as_names()]
#'   for more options.
#' @param values_to A string specifying the name of the column to create
#'   from the data stored in cell values. If `names_to` is a character
#'   containing the special `.value` sentinel, this value will be ignored,
#'   and the name of the value column will be derived from part of the
#'   existing column names.
#' @param values_drop_na If `TRUE`, will drop rows that contain only `NA`s
#'   in the `value_to` column. This effectively converts explicit missing values
#'   to implicit missing values, and should generally be used only when missing
#'   values in `data` were created by its structure.
#' @param names_transform,values_transform A list of column name-function pairs.
#'   Use these arguments if you need to change the type of specific columns.
#'   For example, `names_transform = list(week = as.integer)` would convert
#'   a character week variable to an integer.
#' @param names_ptypes,values_ptypes A list of column name-prototype pairs.
#'   A prototype (or ptype for short) is a zero-length vector (like `integer()`
#'   or `numeric()`) that defines the type, class, and attributes of a vector.
#'   Use these arguments to confirm that the created columns are the types that
#'   you expect.
#'
#'   If not specified, the type of the columns generated from `names_to` will
#'   be character, and the type of the variables generated from `values_to`
#'   will be the common type of the input columns used to generate them.
#' @param ... Additional arguments passed on to methods.
#' @export
pivot_longer <- function(data, cols, names_to = "name", names_prefix = NULL,
                         names_sep = NULL, names_pattern = NULL, names_ptypes = list(),
                         names_transform = list(), names_repair = "check_unique",
                         values_to = "value", values_drop_na = FALSE, values_ptypes = list(),
                         values_transform = list(), ...) {
  ellipsis::check_dots_used()
  UseMethod("pivot_longer")
}

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

  # the values of `.name` (first column) go to the column described in `.value`
  # (second column). The "keys", i.e. the other columns - if any - describe the
  # value further.
  # Each name in `.value` stands for one column in the long format.
  # Two rows with the same values in the "keys" imply that the should end up
  # in the same row in the long format.

  # 1. split spec according to "keys"
  # 2. iterate over splitted spec and pivot_long for each part
  # 3. row bind the results
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
