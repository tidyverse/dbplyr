#' Escape/quote a value
#'
#' @description
#' `escape()` turns R values into SQL literals. It implements double dispatch
#' via two sets of generics: first `escape()` dispatches on the class of `x`,
#' then that method calls `sql_escape_ident()`, `sql_escape_logical()`, etc,
#' which dispatch on `con`.
#'
#' These generics translate individual values into SQL. The core
#' generics are [DBI::dbQuoteIdentifier()] and [DBI::dbQuoteString()]
#' for quoting identifiers and strings, but dbplyr needs additional
#' tools for inserting logical, date, date-time, and raw values into
#' queries.
#'
#' @param x An object to escape. Existing sql vectors will be left as is,
#'   character vectors are escaped with single quotes, numeric vectors have
#'   trailing `.0` added if they're whole numbers, identifiers are
#'   escaped with double quotes.
#' @param parens,collapse Controls behaviour when multiple values are supplied.
#'   `parens` should be a logical flag, or if `NA`, will wrap in
#'   parens if length > 1.
#'
#'   Default behaviour: lists are always wrapped in parens and separated by
#'   commas, identifiers are separated by commas and never wrapped,
#'   atomic vectors are separated by spaces and wrapped in parens if needed.
#' @param con A [sql_dialect] object or database connection. Connections are
#'   supported for backward compatibility.
#' @family generic
#' @returns A [sql] vector.
#' @export
#' @examples
#' con <- simulate_dbi()
#'
#' # Doubles vs. integers
#' escape(1:5, con = con)
#' escape(c(1, 5.4), con = con)
#'
#' # String vs known sql vs. sql identifier
#' escape("X", con = con)
#' escape(sql("X"), con = con)
#' escape(ident("X"), con = con)
#'
#' # Escaping is idempotent
#' escape("X", con = con)
#' escape(escape("X", con = con), con = con)
#'
#' # Database specific generics
#' sql_escape_logical(con, c(TRUE, FALSE, NA))
#' sql_escape_date(con, Sys.Date())
#' sql_escape_date(con, Sys.time())
#' sql_escape_raw(con, charToRaw("hi"))
escape <- function(x, parens = NA, collapse = " ", con = NULL) {
  check_con(con)

  UseMethod("escape")
}

# ident -------------------------------------------------------------------

#' @export
escape.ident <- function(x, parens = FALSE, collapse = ", ", con = NULL) {
  y <- set_names(sql_escape_ident(con, x), names(x))
  sql_vector(y, parens, collapse, con = con)
}

#' @export
#' @rdname escape
sql_escape_ident <- function(con, x) {
  UseMethod("sql_escape_ident", sql_dialect(con))
}
#' @export
sql_escape_ident.DBIConnection <- function(con, x) {
  sql(DBI::dbQuoteIdentifier(con, x))
}

#' @export
sql_escape_ident.sql_dialect <- function(con, x) {
  # Needed because UseMethod hack only affects dispatch, not value
  sql_dialect(con)$quote_identifier(x)
}

# logical -----------------------------------------------------------------

#' @export
escape.logical <- function(x, parens = NA, collapse = ", ", con = NULL) {
  sql_vector(sql_escape_logical(con, x), parens, collapse, con = con)
}

#' @rdname escape
#' @export
sql_escape_logical <- function(con, x) {
  UseMethod("sql_escape_logical", sql_dialect(con))
}
#' @export
sql_escape_logical.DBIConnection <- function(con, x) {
  y <- as.character(x)
  y[is.na(x)] <- "NULL"
  sql(y)
}

#' @export
sql_escape_logical.sql_dialect <- sql_escape_logical.DBIConnection

# factor ------------------------------------------------------------------

#' @export
escape.factor <- function(x, parens = NA, collapse = ", ", con = NULL) {
  x <- as.character(x)
  escape.character(x, parens = parens, collapse = collapse, con = con)
}

# Date --------------------------------------------------------------------

#' @export
escape.Date <- function(x, parens = NA, collapse = ", ", con = NULL) {
  sql_vector(sql_escape_date(con, x), parens, collapse, con = con)
}

#' @export
#' @rdname escape
sql_escape_date <- function(con, x) {
  UseMethod("sql_escape_date", sql_dialect(con))
}
#' @export
sql_escape_date.DBIConnection <- function(con, x) {
  sql_escape_string(con, as.character(x))
}

#' @export
sql_escape_date.sql_dialect <- sql_escape_date.DBIConnection

# POSIXt ------------------------------------------------------------------

#' @export
escape.POSIXt <- function(x, parens = NA, collapse = ", ", con = NULL) {
  sql_vector(sql_escape_datetime(con, x), parens, collapse, con = con)
}

#' @export
#' @rdname escape
sql_escape_datetime <- function(con, x) {
  UseMethod("sql_escape_datetime", sql_dialect(con))
}
#' @export
sql_escape_datetime.DBIConnection <- function(con, x) {
  x <- strftime(x, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  sql_escape_string(con, x)
}

#' @export
sql_escape_datetime.sql_dialect <- sql_escape_datetime.DBIConnection

# character ---------------------------------------------------------------

#' @export
escape.character <- function(x, parens = NA, collapse = ", ", con = NULL) {
  sql_vector(sql_escape_string(con, x), parens, collapse, con = con)
}

#' @export
#' @rdname escape
sql_escape_string <- function(con, x) {
  UseMethod("sql_escape_string")
}
#' @export
sql_escape_string.default <- function(con, x) {
  sql(sql_quote(x, "'"))
}

# double ------------------------------------------------------------------

#' @export
escape.double <- function(x, parens = NA, collapse = ", ", con = NULL) {
  out <- ifelse(is_whole_number(x), sprintf("%.1f", x), as.character(x))

  # Special values
  out[is.na(x)] <- "NULL"
  inf <- is.infinite(x)
  out[inf & x > 0] <- "'Infinity'"
  out[inf & x < 0] <- "'-Infinity'"

  sql_vector(out, parens, collapse, con = con)
}

is_whole_number <- function(x) {
  trunc(x) == x
}

# integer -----------------------------------------------------------------

#' @export
escape.integer <- function(x, parens = NA, collapse = ", ", con = NULL) {
  x[is.na(x)] <- "NULL"
  sql_vector(x, parens, collapse, con = con)
}

#' @export
escape.integer64 <- function(x, parens = NA, collapse = ", ", con = NULL) {
  x <- as.character(x)
  x[is.na(x)] <- "NULL"
  sql_vector(x, parens, collapse, con = con)
}

# blob --------------------------------------------------------------------

#' @export
escape.blob <- function(x, parens = NA, collapse = ", ", con = NULL) {
  pieces <- vapply(x, sql_escape_raw, character(1), con = con)
  sql_vector(pieces, isTRUE(parens) || length(pieces) > 1, collapse, con = con)
}

#' @export
#' @rdname escape
sql_escape_raw <- function(con, x) {
  UseMethod("sql_escape_raw", sql_dialect(con))
}
#' @export
sql_escape_raw.DBIConnection <- function(con, x) {
  # Unlike the other escape functions, this is not vectorised because
  # raw "vectors" are scalars in this content

  if (is.null(x)) {
    sql("NULL")
  } else {
    # SQL-99 standard for BLOB literals
    # https://crate.io/docs/sql-99/en/latest/chapters/05.html#blob-literal-s
    sql(paste0(c("X'", format(x), "'"), collapse = ""))
  }
}

#' @export
sql_escape_raw.sql_dialect <- sql_escape_raw.DBIConnection

# NULL --------------------------------------------------------------------

#' @export
escape.NULL <- function(x, parens = NA, collapse = " ", con = NULL) {
  sql("NULL")
}

# sql ---------------------------------------------------------------------

#' @export
escape.sql <- function(x, parens = NULL, collapse = NULL, con = NULL) {
  sql_vector(x, isTRUE(parens), collapse, con = con)
}

# list --------------------------------------------------------------------

#' @export
escape.list <- function(x, parens = TRUE, collapse = ", ", con = NULL) {
  pieces <- vapply(x, escape, character(1), con = con)
  sql_vector(pieces, parens, collapse, con = con)
}

# errors ------------------------------------------------------------------

#' @export
escape.data.frame <- function(x, parens = TRUE, collapse = ", ", con = NULL) {
  error_embed("a data.frame", "df$x")
}

#' @export
escape.reactivevalues <- function(
  x,
  parens = TRUE,
  collapse = ", ",
  con = NULL
) {
  error_embed("shiny inputs", "input$x")
}

#' @export
escape.default <- function(x, parens = TRUE, collapse = ", ", con = NULL) {
  error_embed(obj_type_friendly(x), "x")
}

# Also used in default_ops() for reactives
error_embed <- function(type, expr) {
  cli_abort(
    c(
      "Cannot translate {type} to SQL.",
      `i` = "Do you want to force evaluation in R with (e.g.) `!!{expr}` or `local({expr})`?"
    ),
    call = NULL
  )
}

# helpers -----------------------------------------------------------------

#' @export
#' @rdname escape
sql_vector <- function(x, parens = NA, collapse = " ", con = NULL) {
  check_bool(parens, allow_na = TRUE)
  check_string(collapse, allow_null = TRUE)
  check_con(con)

  if (is.na(parens)) {
    parens <- length(x) > 1L
  }

  x <- names_to_as(con, x)
  sql_collapse(x, collapse, parens)
}

sql_collapse <- function(x, collapse = " ", parens = FALSE) {
  x <- paste(x, collapse = collapse)
  if (parens) {
    x <- paste0("(", x, ")", recycle0 = TRUE)
  }
  sql(x)
}

# Strips names from named sql() vector by converting them to AS identifiers
names_to_as <- function(con, x, names = names2(x)) {
  if (length(x) == 0) {
    return(sql())
  }

  names_esc <- sql_escape_ident(con, names)
  as_sql <- style_kw(" AS ")
  as <- ifelse(names == "" | names_esc == x, "", paste0(as_sql, names_esc))

  sql(paste0(x, as))
}
