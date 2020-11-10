#' @importFrom tidyr expand
#' @export
expand.tbl_lazy <- function(data, ..., .name_repair = "check_unique") {
  # TODO sort output?
  # TODO wait for bugfix: distinct() ignores groups
  # https://github.com/tidyverse/dbplyr/issues/535
  dots <- quos_auto_name(quos(...))
  dots <- purrr::discard(dots, rlang::quo_is_null)
  names(dots) <- vctrs::vec_as_names(names(dots), repair = .name_repair)

  if (is_empty(dots)) {
    # TODO what should be returned here?
    abort("No columns selected")
  }

  distinct_tables <- purrr::imap(
    dots,
    ~ {
      # ugly hack to deal with `nesting()`
      if (rlang::quo_is_call(.x) && rlang::call_name(.x) == "nesting") {
        x_expr <- rlang::quo_get_expr(.x)
        args <- rlang::call_args(x_expr)
        distinct(data, !!!args)
      } else {
        distinct(data, !!.y := !!.x)
      }
    }
  )

  purrr::reduce(distinct_tables, left_join, by = character())
}

#' @importFrom tidyr complete
#' @export
complete.tbl_lazy <- function(data, ..., fill = list()) {
  full <- expand(data, ...)

  if (is_empty(full)) {
    return(data)
  }

  full <- dplyr::full_join(full, data, by = colnames(full))
  replace_na(full, replace = fill)
}
