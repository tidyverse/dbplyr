
# Returns either SQL (representing a custom query) or a table name
as_table_source <- function(x, con, ..., error_arg = caller_arg(x), error_call = caller_env()) {
  if (is.sql(x)) {
    x
  } else if (is_table_id(x)) {
    as_table_name(x, con = con, error_arg = error_arg, error_call = error_call)
  } else {
    check_table_source(x, arg = error_arg, call = error_call)
  }
}

check_table_source <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!is.sql(x) && !is_table_id(x)) {
    stop_input_type(x, "a table source (SQL or a table identifier)")
  }
}

check_table_id <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!is_table_id(x)) {
    stop_input_type(x, "a table identifier")
  }
}


is_table_id <- function(x) {
  is_table_name(x) ||
    is.ident(x) ||
    is(x, "Id") ||
    is_catalog(x) ||
    is_schema(x) ||
    is.character(x)
}

as_table_name <- function(x,
                          con,
                          error_arg = caller_arg(x),
                          error_call = caller_env()) {
  if (is_table_name(x)) {
    x
  } else if (is.sql(x)) {
    cli::cli_warn(
      c(
        "{.arg {error_arg}} uses SQL where a table identifier is expected",
        i = "If you want to use a literal string without escaping using {.fn I} instead"
      )
    )
    table_name(unclass(x))
  } else if (inherits(x, "ident_q")) {
    table_name(paste0(x, collapse = "."))
  } else if (is.ident(x)) {
    make_table_name(unclass(x), con)
  } else if (is(x, "Id")) {
    table_name(DBI::dbQuoteLiteral(con, x))
  } else if (inherits(x, "dbplyr_catalog")) {
    make_table_name(c(unclass(x$catalog), unclass(x$schema), unclass(x$table)), con)
  } else if (inherits(x, "dbplyr_schema")) {
    make_table_name(c(unclass(x$schema), unclass(x$table)), con)
  } else if (inherits(x, "AsIs")) {
    check_string(unclass(x), allow_empty = FALSE, arg = error_arg, call = error_call)
    table_name(unclass(x))
  } else if (is.character(x)) {
    make_table_name(x, con)
  } else {
    cli::cli_abort(
      "{.arg {error_arg}} uses specification for table name",
      error_call = error_call
    )
  }
}

make_table_name <- function(x, con) {
  needs_quote <- !vapply(x, function(x) inherits(x, "AsIs"), logical(1))
  x[needs_quote] <- sql_escape_ident(con, x[needs_quote])

  table_name(paste0(x, collapse = "."))
}

table_name <- function(x) {
  structure(x, class = "dbplyr_table_name")
}

#' @export
print.dbplyr_table_name <- function(x) {
  cat("<table_name> ", style_kw(x), "\n", sep = "")
}

is_table_name <- function(x) {
  inherits(x, "dbplyr_table_name")
}

#' @export
escape.dbplyr_table_name <- function(x, parens = FALSE, collapse = ", ", con = NULL) {

  alias <- names2(x)
  x <- unname(x)

  if (db_supports_table_alias_with_as(con)) {
    as_sql <- style_kw(" AS ")
  } else {
    as_sql <- " "
  }

  # TODO: Why is this getting double escaped?
  # alias_esc <- sql_escape_ident(con, alias)

  out <- ifelse(alias == "" | alias == x, x, paste0(x, as_sql, alias))
  sql_vector(out, parens, collapse, con = con)
}
