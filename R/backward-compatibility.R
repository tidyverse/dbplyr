# Backward compatibility ---------------------------------------------------------
# These functions are provided for backward compatibility with packages that
# used internal dbplyr functions. They should not be used in new code.

# Used by duckdb
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
      if (!is.sql(value) && !is_string(value)) {
        stop_input_type(value, what = c("a string", "scalar SQL"))
      }
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
    cli_abort(
      "Collapsing is only allowed for {.val col} and {.val val}, not for {.val {type}}."
    )
  }
}


# Used by RPresto

#' Optimise a SQL query
#'
#' No longer used.
#'
#' @export
#' @keywords internal
sql_optimise <- function(x, con = NULL, ..., subquery = FALSE) {
  UseMethod("sql_optimise")
}

# Used by ckanr, implyr, sergeant
#' Create a "sql src" object
#'
#' Deprecated: please use directly use a `DBIConnection` object instead.
#'
#' @param subclass Name of subclass.
#' @param con The connection object.
#' @param ... Other arguments passed on to individual methods.
#' @export
#' @keywords internal
src_sql <- function(subclass, con, ...) {
  lifecycle::deprecate_stop(when = "1.4.0", what = "src_sql()")
}

# Used by dm, etl, starwarsdb
#' @importFrom dplyr src_tbls
#' @export
src_tbls.src_sql <- function(x, ...) {
  lifecycle::deprecate_soft("2.6.0", "src_tbls()")
  DBI::dbListTables(x$con)
}

# Used by healthdb
#' Escape ANSI characters
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [escape()] instead.
#'
#' @param x Object to escape.
#' @param parens,collapse Controls parens and collapsing. Passed on to
#'   [escape()].
#' @export
#' @keywords internal
escape_ansi <- function(x, parens = NA, collapse = "") {
  lifecycle::deprecate_soft("2.6.0", "escape_ansi()", "escape()")
  escape(x, parens = parens, collapse = collapse, con = simulate_dbi())
}
