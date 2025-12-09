#' Build a SQL string.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' `build_sql()` is deprecated in favor of [glue_sql2()].
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
#'
#' # Old:
#' build_sql("SELECT * FROM ", ident("table"), con = con)
#' # New:
#' glue_sql2(con, "SELECT * FROM {.tbl 'table'}")
#'
#' # Old:
#' name <- "Robert"
#' build_sql("INSERT INTO students (name) VALUES (", name, ")", con = con)
#' # New:
#' glue_sql2(con, "INSERT INTO students (name) VALUES ({.val name})")
#'
#' # Old:
#' cols <- c("x", "y")
#' build_sql("SELECT ", ident(cols), " FROM table", con = con)
#' # New:
#' glue_sql2(con, "SELECT {.col cols*} FROM {.tbl 'table'}")
build_sql <- function(..., .env = parent.frame(), con = sql_current_con()) {
  lifecycle::deprecate_warn("2.6.0", "build_sql()", "glue_sql2()")
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

#' Build SQL strings with glue syntax
#'
#' @description
#' `glue_sql2()` is a SQL string builder that uses [glue::glue()] syntax with
#' special type markers for safe SQL generation. It replaces the deprecated
#' [build_sql()] function with a more explicit and type-safe approach.
#'
#' Within the glue template, use type markers to specify how values should be
#' escaped and formatted:
#'
#' * `.tbl` - A table identifier (e.g., `DBI::Id()`, string, or `in_schema()`).
#' * `.from` - A subquery or table identifier.
#' * `.name` - A name for an index or subquery (string or `ident()`).
#' * `.col` - A column name or multiple columns (use `*` suffix for multiple).
#' * `.kw` - An SQL keyword that will be syntax-highlighted.
#' * `.val` - Any value to be escaped (vectors, dates, SQL, etc.).
#'
#' The `*` suffix after `.col` or `.val` collapses the vector into a
#' comma-separated list.
#'
#' @param .con A database connection.
#' @param ... SQL fragments to interpolate. These are evaluated in `.envir` and
#'   then combined according to their type markers.
#' @param .sep Separator to use between elements of `...`.
#' @param .envir Environment to evaluate `...` in.
#' @param .open,.close Opening and closing delimiters for interpolation.
#' @param .na,.null Values to use for `NA`/`NULL` values.
#' @param .comment Comment character to use.
#' @param .literal Whether to treat strings literally.
#' @param .trim Whether to trim whitespace.
#' @return An SQL string.
#' @export
#' @examples
#' con <- simulate_dbi()
#'
#' tbl <- "my_table"
#' glue_sql2(con, "SELECT * FROM {.tbl tbl}")
#'
#' # Values are properly escaped
#' name <- "Robert'); DROP TABLE students;--"
#' glue_sql2(con, "INSERT INTO students (name) VALUES ({.val name})")
#'
#' # Multiple columns with collapse
#' cols <- c("name", "age", "grade")
#' glue_sql2(con, "SELECT {.col cols*} FROM students")
glue_sql2 <- function(
  .con,
  ...,
  .sep = "",
  .envir = parent.frame(),
  .open = "{",
  .close = "}",
  .na = DBI::SQL("NULL"),
  .null = "",
  .comment = "#",
  .literal = FALSE,
  .trim = TRUE
) {
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

sql_glue <- function(x, con = sql_current_con(), envir = parent.frame()) {
  glue_sql2(con, x, .envir = envir)
}

sql_quote_transformer <- function(connection) {
  function(text, envir) {
    collapse_regex <- "[*][[:space:]]*$"
    should_collapse <- grepl(collapse_regex, text)
    if (should_collapse) {
      text <- sub(collapse_regex, "", text)
    }

    type_regex <- "^\\.(tbl|col|name|from|kw|val|sql) (.*)"
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

    if (type == "sql") {
      # leave as is
    } else if (type == "tbl") {
      value <- as_table_path(value, connection)
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
      # if (!is.sql(value) && !is_string(value)) {
      #   stop_input_type(value, what = c("a string", "scalar SQL"))
      # }
    }

    if (type == "val") {
      if (should_collapse) {
        value <- escape(
          value,
          collapse = ", ",
          parens = FALSE,
          con = connection
        )
      } else {
        value <- escape(value, con = connection)
      }
    } else if (type %in% c("tbl", "from", "col", "name", "raw")) {
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
    cli_abort(
      "Collapsing is only allowed for {.val col} and {.val val}, not for {.val {type}}."
    )
  }
}
