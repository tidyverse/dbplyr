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
  cn <- colnames(data)
  names_from <- tidyselect::vars_select(cn, !!enquo(names_from))
  values_from <- tidyselect::vars_select(cn, !!enquo(values_from))

  np <- "The `reparg`` argument is not supported for remote back-ends"
  assert_that(is.null(id_cols), msg = sub("reparg", "id_cols", np))
  assert_that(is.null(names_sep), msg = sub("reparg", "names_sep", np))
  assert_that(is.null(names_repair), msg = sub("reparg", "names_repair", np))
  assert_that(is.null(values_fill), msg = sub("reparg", "values_fill", np))
  assert_that(is.null(values_fn), msg = sub("reparg", "values_fn", np))
  assert_that(is.null(spec), msg = sub("reparg", "spec", np))


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

globalVariables(c("name", "value"))
