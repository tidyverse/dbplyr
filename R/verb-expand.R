expand.tbl_lazy <- function(data, ..., .name_repair = "check_unique") {
  # TODO wait for bugfix: distinct() ignores groups
  # https://github.com/tidyverse/dbplyr/issues/535
  dots <- purrr::discard(quos(...), quo_is_null)

  if (is_empty(dots)) {
    abort("Must supply variables in `...`")
  }

  distinct_tbl_vars <- purrr::imap(
    dots,
    ~ {
      # ugly hack to deal with `nesting()`
      if (quo_is_call(.x, name = "nesting")) {
        x_expr <- quo_get_expr(.x)
        call_args(x_expr)
      } else {
        list(quo_get_expr(.x))
      }
    }
  )

  # now that `nesting()` has been unpacked resolve name conflicts
  out_names <- names(exprs_auto_name(purrr::flatten(distinct_tbl_vars)))
  out_names_repaired <- vctrs::vec_as_names(out_names, repair = .name_repair)

  ns <- lengths(distinct_tbl_vars)
  indices <- vec_rep_each(seq_along(distinct_tbl_vars), ns)
  out_names_list <- vctrs::vec_split(out_names_repaired, indices)$val

  distinct_tables <- purrr::map2(
    distinct_tbl_vars, out_names_list,
    ~ {
      args <- set_names(.x, .y)
      distinct(data, !!!args)
    }
  )

  purrr::reduce(distinct_tables, left_join, by = character())
}

complete.tbl_lazy <- function(data, ..., fill = list()) {
  full <- expand(data, ...)

  if (is_empty(full)) {
    return(data)
  }

  full <- full_join(full, data, by = colnames(full))
  replace_na(full, replace = fill)
}
