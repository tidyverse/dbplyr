#' @export
#' @rdname db_copy_to
db_pivot_wider <- function(data,
                           id_cols = NULL,
                           names_from = name,
                           names_prefix = "",
                           names_sep = NULL,
                           names_repair = NULL,
                           values_from = value,
                           values_fill = NULL,
                           values_fn = NULL,
                           spec = NULL) {
  check_null_pivot_args(
    id_cols = !!id_cols, names_sep = !!names_sep,
    names_repair = !!names_repair, values_fill = !!values_fill,
    values_fn = !!values_fn, spec = !!spec
  )
  cn <- colnames(data)
  names_from <- tidyselect::vars_select(cn, !!enquo(names_from))
  values_from <- tidyselect::vars_select(cn, !!enquo(values_from))
  pl <- c(values_from, names_from)
  kp <- cn[!(cn %in% pl)]
  headers <- pull(summarise(group_by(data, !!sym(names_from))))
  mt <- purrr::map(
    headers,
    ~ {
      header <- .x
      purrr::map(
        values_from,
        ~ expr(max(ifelse(!!sym(names_from) == !!header, !!sym(.x), NA), na.rm = TRUE))
      )
    }
  )
  fmt <- flatten(mt)
  if (length(values_from) > 1) {
    vp <- paste0(values_from, "_")
  } else {
    vp <- ""
  }
  hn <- purrr::map(headers, ~ paste0(vp, names_prefix, .x))
  rhn <- purrr::reduce(hn, c)
  nmt <- set_names(fmt, rhn)
  grps <- group_by(data, !!!syms(kp))
  summarise(grps, !!!nmt)
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

globalVariables(c("name", "value"))
