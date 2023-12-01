# table source = table id or sql

# table id =
# * interface to outside world; many ways to specify
# * always refers to exactly one table
# * but all converted to table name ASAP

# table name
# * escaped string
# * internal (and backend) use only; not user facing
# * can be vector of multiple names
# * object names are always assumed to be table names

# table_name --------------------------------------------------------------

table_name <- function(x) {
  check_character(x)
  structure(x, class = "dbplyr_table_name")
}

is_table_name <- function(x) {
  inherits(x, "dbplyr_table_name")
}

#' @export
print.dbplyr_table_name <- function(x, ...) {
  cat("<table_name> ", paste0(style_kw(x), collapse = ", "), "\n", sep = "")
}

#' @export
`[.dbplyr_table_name` <- function(x, ...) {
  table_name(NextMethod())
}
#' @export
`[[.dbplyr_table_name` <- function(x, ...) {
  table_name(NextMethod())
}

#' @export
`c.dbplyr_table_name` <- function(x, ...) {
  table_name(NextMethod())
}

make_table_name <- function(x, con, collapse = TRUE) {
  needs_quote <- !vapply(x, function(x) inherits(x, "AsIs"), logical(1))
  x[needs_quote] <- sql_escape_ident(con, x[needs_quote])
  if (collapse) {
    x <- paste0(x, collapse = ".")
  }

  table_name(x)
}

as_table_names <- function(x, con) {
  make_table_name(x, con, collapse = FALSE)
}

# TODO: make this generic
db_parse_table_name <- function(con, x) {
  quote_char <- substr( sql_escape_ident(con, ""), 1, 1)
  scan(
    text = x,
    what = character(),
    quote = quote_char,
    quiet = TRUE,
    na.strings = character(),
    sep = "."
  )
}
db_table_name_extract <- function(con, x) {
  vapply(x, FUN.VALUE = character(1), function(x) {
    if (x == "") return("")

    out <- db_parse_table_name(con, x)
    out[[length(out)]]
  })
}

#' @export
escape.dbplyr_table_name <- function(x, parens = FALSE, collapse = ", ", con = NULL) {
  alias <- names2(x) # assume alias is already escaped
  x <- unname(x)

  table_name <- as_table_name(db_table_name_extract(con, x), con)

  if (db_supports_table_alias_with_as(con)) {
    as_sql <- style_kw(" AS ")
  } else {
    as_sql <- " "
  }

  out <- ifelse(alias == "" | alias == table_name, x, paste0(x, as_sql, alias))
  sql_vector(out, parens, collapse, con = con)
}

# table id ----------------------------------------------------------------

check_table_id <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!is_table_id(x)) {
    stop_input_type(x, "a table identifier")
  }
}

is_table_id <- function(x) {
  is_table_name(x) ||
    is.ident(x) ||
    methods::is(x, "Id") ||
    is_catalog(x) ||
    is_schema(x) ||
    is.character(x)
}

check_table_name <- function(x,
                             error_arg = caller_arg(x),
                             error_call = caller_env()) {
  if (!is_table_name(x)) {
    cli::cli_abort(
      "{.arg {error_arg}} must be a <table_name>, not {.obj_type_friendly x}.",
      call = error_call,
      .internal = TRUE
    )
  }
}

as_table_name <- function(x,
                          con,
                          error_arg = caller_arg(x),
                          error_call = caller_env()) {
  check_required(con)

  if (is_table_name(x)) {
    x
  } else if (is.sql(x)) {
    cli::cli_warn(
      c(
        "{.arg {error_arg}} uses SQL where a table identifier is expected.",
        i = "If you want to use a literal (unquoted) identifier use {.fn I} instead."
      )
    )
    table_name(unclass(x))
  } else if (inherits(x, "ident_q")) {
    table_name(paste0(x, collapse = "."))
  } else if (is.ident(x)) {
    make_table_name(unclass(x), con)
  } else if (methods::is(x, "Id")) {
    make_table_name(x@name, con)
  } else if (inherits(x, "dbplyr_catalog")) {
    make_table_name(c(unclass(x$catalog), unclass(x$schema), unclass(x$table)), con)
  } else if (inherits(x, "dbplyr_schema")) {
    make_table_name(c(unclass(x$schema), unclass(x$table)), con)
  } else if (inherits(x, "AsIs")) {
    check_string(unclass(x), allow_empty = FALSE, arg = error_arg, call = error_call)
    table_name(unclass(x))
  } else if (is.character(x)) {
    check_string(x, allow_empty = FALSE, arg = error_arg, call = error_call)
    make_table_name(x, con, collapse = FALSE)
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

