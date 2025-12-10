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
#' glue_sql2(con, "INSERT INTO students (name) VALUES ({name})")
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
#' The default glue syntax, `{x}`, will escape the `x` using the
#' database connection. If there are multiple values in `x`, they'll be
#' collapsed into a single string with `,`. If you want them to be wrapped in
#' `()`, use a `*` suffix, e.g. `{x*}`.
#'
#' You can also use a type markers to control how the value is treated:
#'
#' * `{.sql x}`: `x` is literal SQL that should be interpolated as
#'   is, without additional escaping.
#' * `{.tbl x}`: `x` is a table identifier like a string, `I()`, or one of
#'   the older forms like  `DBI::Id()` or `in_schema()`.
#' * `{.id x}`: `x` is an generic identifiers like for a column or index.
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
#' glue_sql2(con, "SELECT * FROM {.tbl tbl}")
#'
#' # Values are properly escaped
#' name <- "Robert'); DROP TABLE students;--"
#' glue_sql2(con, "INSERT INTO students (name) VALUES ({name})")
#'
#' # Control wrapping with *
#' x <- c("name", "age", "grade")
#' glue_sql2(con, "SELECT {.id x} FROM students")
#' glue_sql2(con, "SELECT * WHERE variable IN {x*}")
glue_sql2 <- function(con, sql, envir = parent.frame()) {
  check_con(con)

  env <- current_env()
  sql(glue(
    sql,
    .envir = envir,
    .na = "NULL",
    .null = "",
    .transformer = function(text, envir) {
      glue_transformer(con, text, envir, call = env)
    }
  ))
}

sql_glue <- function(sql, envir = parent.frame()) {
  glue_sql2(sql_current_con(), sql, envir = envir)
}

glue_transformer <- function(con, text, envir, call = caller_env()) {
  parsed <- parse_glue_spec(text)
  if (!parsed$type %in% c("sql", "tbl", "from", "id", "")) {
    cli::cli_abort(
      "Unknown marker {.val {parsed$type}} in {{{text}}}.",
      call = call
    )
  }
  withCallingHandlers(
    value <- eval(parse(text = parsed$value, keep.source = FALSE), envir),
    error = function(e) {
      cli::cli_abort(
        "Failed to interpolate {{{text}}.",
        call = call,
        parent = e
      )
    }
  )

  # Coerce types that need coercion
  if (parsed$type == "sql") {
    value <- sql(value)
  } else if (parsed$type == "tbl") {
    value <- as_table_path(value, con)
  } else if (parsed$type == "id") {
    value <- as_ident(value)
  }

  if (parsed$collapse) {
    value <- escape(value, collapse = ", ", parens = TRUE, con = con)
  } else {
    value <- escape(value, collapse = ", ", parens = FALSE, con = con)
  }

  unclass(value)
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
