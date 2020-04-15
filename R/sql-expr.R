#' Generate SQL from R expressions
#'
#' Low-level building block for generating SQL from R expressions.
#' Strings are escaped; names become bare SQL identifiers. User infix
#' functions have `%` stripped.
#'
#' Using `sql_expr()` in package will require use of [globalVariables()]
#' to avoid `R CMD check` NOTES. This is a small amount of additional pain,
#' which I think is worthwhile because it leads to more readable translation
#' code.
#'
#' @param x A quasiquoted expression
#' @param con Connection to use for escaping. Will be set automatically when
#'   called from a function translation.
#' @param .fn Function name (as string, call, or symbol)
#' @param ... Arguments to function
#' @keywords internal
#' @export
#' @examples
#' con <- simulate_dbi() # not necessary when writing translations
#'
#' sql_expr(f(x + 1), con = con)
#' sql_expr(f("x", "y"), con = con)
#' sql_expr(f(x, y), con = con)
#'
#' x <- ident("x")
#' sql_expr(f(!!x, y), con = con)
#'
#' sql_expr(cast("x" %as% DECIMAL), con = con)
#' sql_expr(round(x) %::% numeric, con = con)
#'
#' sql_call2("+", quote(x), 1, con = con)
#' sql_call2("+", "x", 1, con = con)
sql_expr <- function(x, con = sql_current_con()) {
  x <- enexpr(x)
  x <- replace_expr(x, con = con)
  sql(x)
}

#' @export
#' @rdname sql_expr
sql_call2 <- function(.fn, ..., con = sql_current_con()) {
  fn <- call2(.fn, ...)
  fn <- replace_expr(fn, con = con)
  sql(fn)
}


replace_expr <- function(x, con) {
  if (is.atomic(x)) {
    as.character(escape(unname(x), con = con))
  } else if (is.name(x)) {
    as.character(x)
  # } else if (is.call(x) && identical(x[[1]], quote(I))) {
  #   escape(ident(as.character(x[[2]])))
  } else if (is.call(x)) {
    fun <- toupper(as.character(x[[1]]))
    args <- lapply(x[-1], replace_expr, con = con)

    if (is_infix_base(fun)) {
      if (length(args) == 1) {
        paste0(fun, args[[1]])
      } else {
        paste0(args[[1]], " ", fun, " ", args[[2]])
      }
    } else if (is_infix_user(fun)) {
      fun <- substr(fun, 2, nchar(fun) - 1)
      paste0(args[[1]], " ", fun, " ", args[[2]])
    } else if (fun == "(") {
      paste0("(", paste0(args, collapse = ", "), ")")
    } else {
      paste0(fun, "(", paste0(args, collapse = ", "), ")")
    }

  } else {
    x
  }

}
