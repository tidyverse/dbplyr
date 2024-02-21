# table source = table id or sql

# table id =
# * interface to outside world; many ways to specify
# * always refers to exactly one table
# * but all converted to table name ASAP

# table path =
# * qualified table identifier (e.g. foo.bar.baz, bar.baz, baz)
# * always quoted
# * internal (and backend) use only; not user facing
# * can be vector containing multiple names
# * object names are always assumed to be table paths

# table_path --------------------------------------------------------------

table_path <- function(x) {
  check_character(x)
  x <- unname(x)
  structure(x, class = c("dbplyr_table_path", "character"))
}

# So you can do SQL(table_path("foo"))
setOldClass(c("dbplyr_table_path", "character"))

is_table_path <- function(x) {
  inherits(x, "dbplyr_table_path")
}

#' @export
print.dbplyr_table_path <- function(x, ...) {
  cat("<table_path> ", paste0(style_kw(x), collapse = ", "), "\n", sep = "")
}

#' @export
`[.dbplyr_table_path` <- function(x, ...) {
  table_path(NextMethod())
}
#' @export
`[[.dbplyr_table_path` <- function(x, ...) {
  table_path(NextMethod())
}

#' @export
`c.dbplyr_table_path` <- function(x, ...) {
  table_path(NextMethod())
}

make_table_path <- function(x, con, collapse = TRUE) {
  needs_quote <- !vapply(x, component_is_escaped, logical(1))

  x <- vapply(x, unclass, character(1))
  x[needs_quote] <- sql_escape_ident(con, x[needs_quote])
  if (collapse) {
    x <- paste0(x, collapse = ".")
  }

  table_path(x)
}

component_is_escaped <- function(x) {
  inherits(x, "AsIs") || is.sql(x) || inherits(x, "ident_q")
}

as_table_paths <- function(x, con) {
  make_table_path(x, con, collapse = FALSE)
}

# Extract just the table name from a full identifier
table_name <- function(x, con) {
  x <- as_table_path(x, con)

  vapply(x, FUN.VALUE = character(1), function(x) {
    if (x == "") return("")

    out <- table_path_components(x, con)
    out[[length(out)]]
  })
}
table_path_components <- function(x, con) {
  quote_char <- substr(sql_escape_ident(con, ""), 1, 1)

  scan(
    text = x,
    what = character(),
    quote = quote_char,
    quiet = TRUE,
    na.strings = character(),
    sep = "."
  )
}

#' @export
escape.dbplyr_table_path <- function(x, parens = FALSE, collapse = ", ", con = NULL) {
  # names are always already escaped
  alias <- names2(x)
  table_path <- as_table_path(table_name(x, con), con)
  has_alias <- alias == "" | alias == table_path

  if (db_supports_table_alias_with_as(con)) {
    as_sql <- style_kw(" AS ")
  } else {
    as_sql <- " "
  }

  out <- ifelse(has_alias, unname(x), paste0(x, as_sql, alias))
  sql_vector(out, parens, collapse, con = con)
}

# table id ----------------------------------------------------------------

check_table_id <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!is_table_id(x)) {
    stop_input_type(x, "a table identifier")
  }
}

is_table_id <- function(x) {
  is_table_path(x) ||
    is.ident(x) ||
    methods::is(x, "Id") ||
    is_catalog(x) ||
    is_schema(x) ||
    is.character(x)
}

check_table_path <- function(x,
                             error_arg = caller_arg(x),
                             error_call = caller_env()) {
  if (!is_table_path(x)) {
    cli::cli_abort(
      "{.arg {error_arg}} must be a <table_path>, not {.obj_type_friendly x}.",
      call = error_call,
      .internal = TRUE
    )
  }
}

as_table_path <- function(x,
                          con,
                          error_arg = caller_arg(x),
                          error_call = caller_env()) {
  check_required(con)

  if (is_table_path(x)) {
    x
  } else if (is.sql(x)) {
    cli::cli_warn(
      c(
        "{.arg {error_arg}} uses SQL where a table identifier is expected.",
        i = "If you want to use a literal (unquoted) identifier use {.fn I} instead."
      )
    )
    table_path(unclass(x))
  } else if (inherits(x, "ident_q")) {
    table_path(paste0(x, collapse = "."))
  } else if (is.ident(x)) {
    make_table_path(unclass(x), con)
  } else if (methods::is(x, "Id")) {
    make_table_path(x@name, con)
  } else if (inherits(x, "dbplyr_catalog")) {
    make_table_path(list(x$catalog, x$schema, x$table), con)
  } else if (inherits(x, "dbplyr_schema")) {
    make_table_path(list(x$schema, x$table), con)
  } else if (inherits(x, "AsIs")) {
    check_string(unclass(x), allow_empty = FALSE, arg = error_arg, call = error_call)
    table_path(unclass(x))
  } else if (is.character(x)) {
    check_string(x, allow_empty = FALSE, arg = error_arg, call = error_call)
    make_table_path(x, con, collapse = FALSE)
  } else {
    cli::cli_abort(
      "{.arg {error_arg}} uses unknown specification for table name",
      error_call = error_call
    )
  }
}

# table source ------------------------------------------------------------

# Returns either SQL (representing a custom query) or a table name
as_table_source <- function(x, con, ..., error_arg = caller_arg(x), error_call = caller_env()) {
  if (is.sql(x)) {
    x
  } else if (is_table_id(x)) {
    as_table_path(x, con = con, error_arg = error_arg, error_call = error_call)
  } else {
    check_table_source(x, arg = error_arg, call = error_call)
  }
}

check_table_source <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!is.sql(x) && !is_table_id(x)) {
    stop_input_type(x, "a table source (SQL or a table identifier)")
  }
}
