#' Build a SQL string.
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
#' con <- simulate_dbi()
#' build_sql("SELECT * FROM TABLE", con = con)
#' x <- "TABLE"
#' build_sql("SELECT * FROM ", x, con = con)
#' build_sql("SELECT * FROM ", ident(x), con = con)
#' build_sql("SELECT * FROM ", sql(x), con = con)
#'
#' # http://xkcd.com/327/
#' name <- "Robert'); DROP TABLE Students;--"
#' build_sql("INSERT INTO Students (Name) VALUES (", name, ")", con = con)
build_sql <- function(..., .env = parent.frame(), con = sql_current_con()) {
  check_con(con)

  escape_expr <- function(x, con) {
    # If it's a string, leave it as is
    if (is.character(x)) return(x)

    val <- eval_bare(x, .env)
    # Skip nulls, so you can use if statements like in paste
    if (is.null(val)) return("")

    escape(val, con = con)
  }

  pieces <- purrr::map_chr(enexprs(...), escape_expr, con = con)
  sql(paste0(pieces, collapse = ""))
}

#' A dbplyr specific version of glue
#'
#' Similar to the inline markup of cli this function makes SQL generation easier
#' and safer by providing a couple of types. For example
#'
#' ```
#'   glue_sql2(
#'      con,
#'      "CREATE ", if (unique) "UNIQUE ", "INDEX {.name name}",
#'      " ON {.tbl table} ({.col columns*})"
#'    )
#' ```
#'
#' The following types are supported:
#'
#' * .tbl A table identifier, e.g. `DBI::Id()`. Converted via `as_table_ident()`.
#' * .from A subquery or a table identifier. Converted via `as_from()`.
#' * .name A name, e.g. for an index or a subquery. Can be a string or a scalar (quoted) ident.
#' * .col A column or multiple columns if expression ends with `*`.
#' * .kw An SQL keyword - e.g. `SELECT` or `WHERE` - that should be highlighted.
#' * .val Any value - e.g. an integer vector, a Date, SQL - which is escaped as
#'   usual via `escape()`.
#'
#' If no type is specified the value must be a string or scalar SQL and it isn't
#' escaped or collapsed.
#'
#' @noRd
#'
#' @examples
#' glue_sql2(con, "COLLECT STATISTICS {.tbl table}")
#' glue_sql2(con, "{f}({.val x}, {.val y})")
glue_sql2 <- function(.con,
                      ...,
                      .sep = "",
                      .envir = parent.frame(),
                      .open = "{",
                      .close = "}",
                      .na = DBI::SQL("NULL"),
                      .null = "",
                      .comment = "#",
                      .literal = FALSE,
                      .trim = TRUE) {
  sql(glue(
    ...,
    .sep = .sep,
    .envir = .envir,
    .open = .open,
    .close = .close,
    .na = .na,
    .null = .null,
    .comment = .comment,
    .literal = .literal,
    .transformer = sql_quote_transformer(.con),
    .trim = .trim
  ))
}

sql_quote_transformer <- function(connection) {
  function(text, envir) {
    collapse_regex <- "[*][[:space:]]*$"
    should_collapse <- grepl(collapse_regex, text)
    if (should_collapse) {
      text <- sub(collapse_regex, "", text)
    }

    type_regex <- "^\\.(tbl|col|name|from|kw|val) (.*)"
    m <- regexec(type_regex, text)
    is_quoted <- any(m[[1]] != -1)
    if (is_quoted) {
      matches <- regmatches(text, regexec(type_regex, text))[[1]]

      type <- matches[[2]]
      value <- matches[[3]]
    } else {
      value <- text
      type <- "raw"
    }
    value <- eval(parse(text = value, keep.source = FALSE), envir)
    glue_check_collapse(type, should_collapse)

    if (type == "tbl") {
      value <- as_table_name(value, connection)
    } else if (type == "from") {
      value <- as_table_source(value, connection)
    } else if (type == "col") {
      if (is_bare_character(value)) {
        value <- ident(value)
      }
    } else if (type == "name") {
      # allowed should be `ident`, `ident_q` (maybe), string
      if (is_bare_character(value)) {
        value <- ident(value)
      }
    } else if (type == "kw") {
      value <- sql(style_kw(value))
    } else if (type == "val") {
      # keep as is
    } else if (type == "raw") {
      if (!is.sql(value) && !is_string(value)) {
        stop_input_type(
          value,
          what = c("a string", "scalar SQL")
        )
      }
    }

    if (type == "val") {
      if (should_collapse) {
        value <- escape(value, collapse = ", ", parens = FALSE, con = connection)
      } else {
        value <- escape(value, con = connection)
      }
    } else if (type %in% c("tbl", "from", "col", "name")) {
      value <- escape(value, collapse = NULL, parens = FALSE, con = connection)
      if (should_collapse) {
        value <- paste0(unclass(value), collapse = ", ")
      }
    }

    # TODO use `vctrs::vec_check_size(value, size = 1L)`
    if (length(value) != 1) {
      cli_abort("{.arg value} must have size 1, not {length(value)}.")
    }

    unclass(value)
  }
}

glue_check_collapse <- function(type, collapse) {
  if (type %in% c("col", "val")) {
    return()
  }

  if (collapse) {
    cli_abort("Collapsing is only allowed for {.val col} and {.val val}, not for {.val {type}}.")
  }
}
