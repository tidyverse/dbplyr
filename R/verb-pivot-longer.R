#' @export
pivot_longer <- function(data, cols, names_to = "name", names_prefix = NULL,
                         names_sep = NULL, names_pattern = NULL, names_ptypes = list(),
                         names_transform = list(), names_repair = "check_unique",
                         values_to = "value", values_drop_na = FALSE, values_ptypes = list(),
                         values_transform = list(), ...) {
  ellipsis::check_dots_used()
  UseMethod("pivot_longer")
}

#' @export
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
                                  values_ptypes = list(),
                                  values_transform = list(),
                                  ...) {
  cols <- enquo(cols)
  spec <- build_longer_spec(data, !!cols,
    names_to = names_to,
    values_to = values_to,
    names_prefix = names_prefix,
    names_sep = names_sep,
    names_pattern = names_pattern,
    names_ptypes = names_ptypes,
    names_transform = names_transform
  )

  pivot_longer_spec(data, spec,
    names_repair = names_repair,
    values_drop_na = values_drop_na,
    values_ptypes = values_ptypes,
    values_transform = values_transform
  )
}


build_longer_spec <- function(data, cols,
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


pivot_longer_spec <- function(data,
                              spec,
                              names_repair = "check_unique",
                              values_drop_na = FALSE,
                              values_ptypes = list(),
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
    ~ {
      row <- spec_split$val[[.x]][, 1:2]
      keys <- spec_split$key[.x, ]
      keys$.seq <- NULL

      transmute(
        data,
        !!!id_cols,
        !!!keys,
        !!!set_names(syms(row$.name), row$.value)
      )
    }
  )

  data_long <- purrr::reduce(data_long_list, union_all)

  if (values_drop_na) {
    value_cols <- unique(spec$.value)

    data_long <- data_long %>%
      dplyr::filter_at(value_cols, dplyr::all_vars(!is.na(.)))
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

str_separate <- function(x, into, sep, convert = FALSE, extra = "warn", fill = "warn") {
  # COPIED FROM tidyr

  if (!is.character(into)) {
    abort("`into` must be a character vector")
  }

  if (is.numeric(sep)) {
    out <- strsep(x, sep)
  } else if (is_character(sep)) {
    out <- str_split_fixed(x, sep, length(into), extra = extra, fill = fill)
  } else {
    abort("`sep` must be either numeric or character")
  }

  names(out) <- as_utf8_character(into)
  out <- out[!is.na(names(out))]
  if (convert) {
    out[] <- map(out, type.convert, as.is = TRUE)
  }
  as_tibble(out)
}

strsep <- function(x, sep) {
  # COPIED FROM tidyr

  nchar <- nchar(x)
  pos <- map(sep, function(i) {
    if (i >= 0) return(i)
    pmax(0, nchar + i)
  })
  pos <- c(list(0), pos, list(nchar))

  map(1:(length(pos) - 1), function(i) {
    substr(x, pos[[i]] + 1, pos[[i + 1]])
  })
}

str_split_fixed <- function(value, sep, n, extra = "warn", fill = "warn") {
  # COPIED FROM tidyr

  if (extra == "error") {
    warn(glue(
      "`extra = \"error\"` is deprecated. \\
       Please use `extra = \"warn\"` instead"
    ))
    extra <- "warn"
  }

  extra <- arg_match(extra, c("warn", "merge", "drop"))
  fill <- arg_match(fill, c("warn", "left", "right"))

  n_max <- if (extra == "merge") n else -1L
  pieces <- str_split_n(value, sep, n_max = n_max)

  simp <- simplifyPieces(pieces, n, fill == "left")

  n_big <- length(simp$too_big)
  if (extra == "warn" && n_big > 0) {
    idx <- list_indices(simp$too_big)
    warn(glue("Expected {n} pieces. Additional pieces discarded in {n_big} rows [{idx}]."))
  }

  n_sml <- length(simp$too_sml)
  if (fill == "warn" && n_sml > 0) {
    idx <- list_indices(simp$too_sml)
    warn(glue("Expected {n} pieces. Missing pieces filled with `NA` in {n_sml} rows [{idx}]."))
  }

  simp$strings
}

str_split_n <- function(x, pattern, n_max = -1) {
  # COPIED FROM tidyr

  m <- gregexpr(pattern, x, perl = TRUE)
  if (n_max > 0) {
    m <- lapply(m, function(x) slice_match(x, seq_along(x) < n_max))
  }
  regmatches(x, m, invert = TRUE)
}

str_extract <- function(x, into, regex, convert = FALSE) {
  # COPIED FROM tidyr

  stopifnot(
    is_string(regex),
    is_character(into)
  )

  out <- str_match_first(x, regex)
  if (length(out) != length(into)) {
    stop(
      "`regex` should define ", length(into), " groups; ", ncol(matches), " found.",
      call. = FALSE
    )
  }

  # Handle duplicated names
  if (anyDuplicated(into)) {
    pieces <- split(out, into)
    into <- names(pieces)
    out <- map(pieces, pmap_chr, paste0, sep = "")
  }

  into <- as_utf8_character(into)

  non_na_into <- !is.na(into)
  out <- out[non_na_into]
  names(out) <- into[non_na_into]

  out <- as_tibble(out)

  if (convert) {
    out[] <- map(out, type.convert, as.is = TRUE)
  }

  out
}

simplifyPieces <- function(pieces, p, fillLeft = TRUE) {
  # HACKED VERSION OF CPP CODE IN tidyr

  n_pieces <- lengths(pieces)
  n <- max(n_pieces)

  strings <- lapply(
    1:p,
    function(i) vapply(
      pieces,
      purrr::pluck,
      i,
      .default = NA_character_,
      FUN.VALUE = character(1)
    )
  )

  list(
    strings = strings,
    too_big = which(p < n_pieces),
    too_sml = which((p > n_pieces) & !is.na(pieces))
  )
}

str_match_first <- function(string, regex) {
  # COPIED FROM tidyr

  loc <- regexpr(regex, string, perl = TRUE)
  loc <- group_loc(loc)

  out <- lapply(
    seq_len(loc$matches),
    function(i) substr(string, loc$start[, i], loc$end[, i])
  )
  out[-1]
}

group_loc <- function(x) {
  # COPIED FROM tidyr

  start <- cbind(as.vector(x), attr(x, "capture.start"))
  end <- start + cbind(attr(x, "match.length"), attr(x, "capture.length")) - 1L

  no_match <- start == -1L
  start[no_match] <- NA
  end[no_match] <- NA

  list(matches = ncol(start), start = start, end = end)
}
