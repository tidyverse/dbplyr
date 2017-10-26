#' Generate SQL from R expressions
#'
#' Low-level building block for generating SQL from R expressions.
#' Strings are escaped; names become bare SQL identifiers. User infix
#' functions have `%` stripped.
#'
#' @param x A quasiquoted expression
#' @inheritParams translate_sql
#' @export
#' @examples
#' sql_expr(f(x + 1))
#'
#' sql_expr(f("x", "y"))
#' sql_expr(f(x, y))
#'
#' sql_expr(cast("x" %as% DECIMAL))
#' sql_expr(round(x) %::% numeric)
sql_expr <- function(x, con = sql_current_con()) {
  x <- enexpr(x)
  x <- replace_expr(x, con = con)
  sql(x)
}

replace_expr <- function(x, con) {
  if (is.atomic(x)) {
    escape(x, con = con)
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
