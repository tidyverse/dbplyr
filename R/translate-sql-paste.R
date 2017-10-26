#' @export
#' @rdname sql_variant
sql_paste <- function(default_sep, f = "CONCAT_WS") {
  f <- toupper(f)

  function(..., sep = default_sep, collapse = NULL){
    check_collapse(collapse)

    sql_expr(UQ(f)(!!sep, !!!list(...)))
  }
}

check_collapse <- function(collapse) {
  if (is.null(collapse))
    return()

  stop(
    "`collapse` not supported in DB translation of `paste()`.\n",
    "Please use str_collapse() instead",
    call. = FALSE
  )
}
