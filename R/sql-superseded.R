#' Build a SQL string.
#'
#' @description
#' `r lifecycle::badge("superseded")`
#' `build_sql()` is superseded in favor of [sql_glue2()].
#'
#' This is a convenience function that should prevent sql injection attacks
#' (which in the context of dplyr are most likely to be accidental not
#' deliberate) by automatically escaping all expressions in the input, while
#' treating bare strings as sql. This is unlikely to prevent any serious
#' attack, but should make it unlikely that you produce invalid sql.
#'
#' This function should be used only when generating `SELECT` clauses,
#' other high level queries, or for other syntax that has no R equivalent.
#' For individual function translations, prefer [sql_expr()].
#'
#' @param ... input to convert to SQL. Use [sql()] to preserve
#'   user input as is (dangerous), and [ident()] to label user
#'   input as sql identifiers (safe)
#' @param .env the environment in which to evaluate the arguments. Should not
#'   be needed in typical use.
#' @param con database connection; used to select correct quoting characters.
#' @keywords internal
#' @export
#' @examples
#' con <- dialect_ansi()
#'
#' # Old:
#' build_sql("SELECT * FROM ", ident("table"), con = con)
#' # New:
#' sql_glue2(con, "SELECT * FROM {.tbl 'table'}")
#'
#' # Old:
#' name <- "Robert"
#' build_sql("INSERT INTO students (name) VALUES (", name, ")", con = con)
#' # New:
#' sql_glue2(con, "INSERT INTO students (name) VALUES ({name})")
build_sql <- function(..., .env = parent.frame(), con = sql_current_con()) {
  check_con(con)

  escape_expr <- function(x, con) {
    # If it's a string, leave it as is
    if (is.character(x)) {
      return(x)
    }

    val <- eval_bare(x, .env)
    # Skip nulls, so you can use if statements like in paste
    if (is.null(val)) {
      return("")
    }

    escape(val, con = con)
  }

  pieces <- purrr::map_chr(enexprs(...), escape_expr, con = con)
  sql(paste0(pieces, collapse = ""))
}

#' Generate SQL from R expressions
#'
#' @description
#' `r lifecycle::badge("superseded")`
#' `sql_expr()` and `sql_call2()` are superseded in favour of [sql_glue()].
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
#' con <- dialect_ansi() # not necessary when writing translations
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
  if (is.null(x)) {
    "NULL"
  } else if (is.atomic(x) || blob::is_blob(x)) {
    as.character(escape(unname(x), con = con))
  } else if (is.name(x)) {
    as.character(x)
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
    x # nocov
  }
}

#' Convert to sql (deprecated)
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @keywords internal
#'
#' @param x Object to coerce
#' @param con Needed when `x` is directly supplied from the user so that
#'   schema specifications can be quoted using the correct identifiers.
#' @export
as.sql <- function(x, con) {
  lifecycle::deprecate_warn("2.6.0", "as.sql()", "as_table_path()")
  if (is_bare_character(x)) {
    ident(x)
  } else {
    x
  }
}

#' Declare a identifier as being pre-quoted.
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' No longer needed; please use [sql()] instead.
#'
#' @keywords internal
#' @export
ident_q <- function(...) {
  x <- c_character(...)
  structure(x, class = c("ident_q", "ident", "character"))
}

#' @export
escape.ident_q <- function(x, parens = FALSE, collapse = ", ", con = NULL) {
  sql_vector(x, parens, collapse, con = con)
}
