#' @export
#' @rdname sql_variant
sql_paste <- function(default_sep, f = "CONCAT_WS") {
  function(..., sep = default_sep, collapse = NULL) {
    check_collapse(collapse)
    sql_call2(f, sep, ...)
  }
}

#' @export
#' @rdname sql_variant
sql_paste_infix <- function(default_sep, op, cast) {
  force(default_sep)
  op <- as.symbol(paste0("%", op, "%"))
  force(cast)

  function(..., sep = default_sep, collapse = NULL) {
    check_collapse(collapse)

    args <- list(...)
    if (length(args) == 1) {
      return(cast(args[[1]]))
    }

    if (sep == "") {
      infix <- function(x, y) sql_call2(op, x, y)
    } else {
      infix <- function(x, y) sql_call2(op, sql_call2(op, x, sep), y)
    }

    purrr::reduce(args, infix)
  }
}

check_collapse <- function(collapse) {
  if (is.null(collapse)) {
    return()
  }

  cli_abort(c(
    "{.arg collapse} not supported in DB translation of {.fun paste}.",
    i = "Please use {.fun str_flatten} instead."
  ))
}
