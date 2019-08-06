#' @export
#' @rdname db_copy_to
db_pivot_longer <- function(data, cols,
                            names_to = "name",
                            values_to = "value",
                            names_prefix = NULL,
                            names_sep = NULL,
                            names_pattern = NULL,
                            names_ptypes = NULL) {
  np <- "The `reparg`` argument is not supported for remote back-ends"
  assert_that(is.null(names_pattern), msg = sub("reparg", "names_pattern", np))
  assert_that(is.null(names_sep), msg = sub("reparg", "names_sep", np))
  assert_that(is.null(names_ptypes), msg = sub("reparg", "names_ptypes", np))
  cols <- enquo(cols)
  names_to <- parse_expr(names_to)
  values_to <- parse_expr(values_to)
  cn <- colnames(data)
  # Create local df to operate the `cols` `select()` op offline
  lcn <- purrr::map_dfc(cn, ~ tibble(!!.x := ""))
  pl <- colnames(select(lcn, !!cols))
  kp <- cn[!(cn %in% pl)]
  grps <- summarise(group_by(data, !!!parse_exprs(kp)))
  ugrps <- ungroup(grps)
  jdata <- inner_join(ugrps, data, by = kp)
  jtb <- purrr::map(
    pl,
    ~ {
      xs <- select(jdata, c(kp, .x))
      xr <- rename(xs, !!values_to := .x)
      if (is.null(names_prefix)) {
        cl <- .x
      } else {
        cl <- stringi::stri_replace_all_regex(.x, paste0("^", names_prefix), "")
      }
      xm <- mutate(xr, !!names_to := cl)
      select(xm, kp, !!names_to, !!values_to)
    }
  )
  purrr::reduce(jtb, union_all)
}
