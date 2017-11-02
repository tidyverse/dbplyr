#' @export
#' @rdname sql_variant
sql_paste <- function(default_sep, f = "CONCAT_WS") {
  f <- toupper(f)

  function(..., sep = default_sep, collapse = NULL){
    check_collapse(collapse)

    sql_expr(UQ(f)(!!sep, !!!list(...)))
  }
}

#' @export
#' @rdname sql_variant
sql_paste_infix <- function(default_sep, op, cast) {
  force(default_sep)
  op <- as.symbol(paste0("%", op, "%"))
  force(cast)

  function(..., sep = default_sep, collapse = NULL){
    check_collapse(collapse)

    args <- list(...)
    if (length(args) == 1) {
      return(cast(args[[1]]))
    }

    if (sep == "") {
      infix <- function(x, y) sql_expr(UQ(op)(!!x, !!y))
    } else {
      infix <- function(x, y) sql_expr(UQ(op)(UQ(op)(!!x, !!sep), !!y))
    }

    reduce(args, infix)
  }
}

check_collapse <- function(collapse) {
  if (is.null(collapse))
    return()

  stop(
    "`collapse` not supported in DB translation of `paste()`.\n",
    "Please use str_flatten() instead",
    call. = FALSE
  )
}
