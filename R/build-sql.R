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
#' con <- simulate_dbi()
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
  lifecycle::deprecate_warn("2.6.0", "build_sql()", "sql_glue2()")
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
#' `sql_glue()` and `sql_glue2()` are designed to help dbplyr extenders
#' generate custom SQL. They differ only in whether or not they require
#' a connection. `sql_glue()` retrieves the ambient connection, making it
#' suitable for use inside [sql_translation()] methods; `sql_glue2()` requires
#' a connection, making it suitable for use inside all other `sql_` methods.
#'
#' As the name suggests, these functions use \pkg{glue} syntax to make it
#' easy to mix fixed SQL with varying user inputs. The default glue syntax, `{x}`,
#' will escape `x` using the database connection. If there are multiple values
#' in `x`, they'll be collapsed into a single string with `,`. If you want them
#' to be wrapped in `()`, use a `*` suffix, e.g. `{x*}`.
#'
#' You can also use type markers to control how the value is treated:
#'
#' * `{.sql x}`: `x` is literal SQL that should be interpolated as
#'   is, without additional escaping. `x` must be a string.
#' * `{.tbl x}`: `x` is a table identifier like a string, `I()`, or one of
#'   the older forms like `DBI::Id()` or `in_schema()`.
#' * `{.id x}`: `x` is a generic identifier, e.g. for a column or index.
#'    `x` must be a character vector
#'
#' @param con A database connection.
#' @param sql A string to interpolate.
#' @param envir Environment to evaluate `sql` in.
#' @return An SQL string.
#' @export
#' @examples
#' con <- simulate_dbi()
#'
#' tbl <- "my_table"
#' sql_glue2(con, "SELECT * FROM {.tbl tbl}")
#'
#' # Values are properly escaped
#' name <- "Robert'); DROP TABLE students;--"
#' sql_glue2(con, "INSERT INTO students (name) VALUES ({name})")
#'
#' # Control wrapping with *
#' x <- c("name", "age", "grade")
#' sql_glue2(con, "SELECT {.id x} FROM students")
#' sql_glue2(con, "SELECT * WHERE variable IN {x*}")
#' @export
sql_glue <- function(sql, envir = parent.frame()) {
  dbplyr_glue(sql_current_con(), sql, envir = envir)
}

#' @export
#' @rdname sql_glue
sql_glue2 <- function(con, sql, envir = parent.frame()) {
  check_con(con)
  dbplyr_glue(con, sql, envir = envir)
}


dbplyr_glue <- function(con, sql, envir = caller_env(), call = caller_env()) {
  sql(glue(
    sql,
    .envir = envir,
    .na = "NULL",
    .null = "",
    .transformer = function(text, envir) {
      glue_transformer(con, text, envir, call = call)
    }
  ))
}


glue_transformer <- function(con, text, envir, call = caller_env()) {
  parsed <- parse_glue_spec(text)
  if (!parsed$type %in% c("sql", "tbl", "id", "")) {
    cli::cli_abort(
      "Unknown marker {.val {parsed$type}} in {{{text}}}.",
      call = call
    )
  }

  if (parsed$value == "...") {
    value <- eval(quote(list(...)), envir)
  } else {
    value <- wrap_glue_error(
      eval(parse(text = parsed$value, keep.source = FALSE), envir),
      text,
      call
    )
  }

  # Coerce types that need coercion
  if (parsed$type == "sql") {
    if (!is_string(value)) {
      cli::cli_abort("{{{text}}} must be passed a string.", call = call)
    }
    value <- sql(value)
  } else if (parsed$type == "tbl") {
    value <- wrap_glue_error(as_table_source(value, con), text, call)
    value <- escape(value, collapse = ", ", parens = parsed$collapse, con = con)
  } else if (parsed$type == "id") {
    if (!is_bare_character(value)) {
      cli::cli_abort(
        "{{{text}}} must be passed a character vector.",
        call = call
      )
    }
    value <- wrap_glue_error(as_ident(value), text, call)
    value <- sql_escape_ident(con, value)
    value <- sql_collapse(value, collapse = ", ", parens = parsed$collapse)
  } else {
    value <- escape(value, collapse = ", ", parens = parsed$collapse, con = con)
  }

  unclass(value)
}

wrap_glue_error <- function(code, text, call = caller_env) {
  withCallingHandlers(
    code,
    error = function(e) {
      cli::cli_abort(
        "Failed to interpolate {{{text}}.",
        call = call,
        parent = e
      )
    }
  )
}

parse_glue_spec <- function(text) {
  # optional format string, like .id, .table
  # followed by R expression
  # followed by optional *, and whitespace
  re <- "^(\\.([^ ]+) )?\\s*([^*]+)([*]?)\\s*$"
  rc <- regexec(re, text)
  rm <- regmatches(text, rc)[[1]]

  list(
    type = rm[[3]],
    value = rm[[4]],
    collapse = rm[[5]] == "*"
  )
}
