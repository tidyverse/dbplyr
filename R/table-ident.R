new_table_ident <- function(...,
                            table = NA_character_,
                            schema = NA_character_,
                            catalog = NA_character_,
                            quoted = FALSE,
                            error_call = caller_env()) {
  check_dots_empty(call = error_call)

  check_character(table, call = error_call)
  check_character(schema, call = error_call)
  check_character(catalog, call = error_call)
  check_logical(quoted, call = error_call)

  data <- vctrs::vec_recycle_common(
    table = table,
    schema = schema,
    catalog = catalog,
    quoted = quoted,
    alias = NA_character_,
    .call = error_call
  )

  purrr::pmap(
    data,
    function(table, schema, catalog, quoted, alias) {
      check_db_table_args(
        table = table,
        schema = schema,
        catalog = catalog,
        quoted = quoted,
        call = error_call
      )
    }
  )

  vctrs::new_rcrd(data, class = "dbplyr_table_ident")
}

check_db_table_args <- function(table,
                                schema,
                                catalog,
                                quoted,
                                call = caller_env()) {
  if (!is.na(catalog) && is.na(schema)) {
    cli_abort("Must supply {.arg schema} when {.arg catalog} is supplied.", call = call)
  }
  if (!is.na(schema) && is.na(table)) {
    cli_abort("Must supply {.arg table} when {.arg schema} is supplied.", call = call)
  }

  if (quoted && !is.na(schema)) {
    cli_abort("Can't supply a schema when {.arg table} is quoted.", call = call)
  }
}

as_table_ident <- function(x, ..., error_call = caller_env()) {
  check_dots_empty()
  UseMethod("as_table_ident")
}

#' @export
as_table_ident.dbplyr_table_ident <- function(x, ..., error_call = caller_env()) {
  x
}

#' @export
as_table_ident.character <- function(x, ..., error_call = caller_env()) {
  new_table_ident(table = unclass(x))
}

#' @export
as_table_ident.ident <- function(x, ..., error_call = caller_env()) {
  new_table_ident(table = unclass(x))
}

#' @export
as_table_ident.ident_q <- function(x, ..., error_call = caller_env()) {
  inform_unwanted_table_ident("ident_q")
  new_table_ident(table = unclass(x), quoted = TRUE)
}

#' @export
as_table_ident.sql <- function(x, ..., error_call = caller_env()) {
  inform_unwanted_table_ident("sql")
  new_table_ident(table = unclass(x), quoted = TRUE)
}

inform_unwanted_table_ident <- function(f) {
  cli::cli_inform(
    c(
      i = "Using {.fn {f}} for a table identifier is intended as fallback in case of bugs.",
      i = "If you need it to work around a bug please open an issue {.url https://github.com/tidyverse/dbplyr/issues}."
    ),
    .frequency = "regularly",
    .frequency_id = paste0("dbplyr_", f)
  )
}

#' @export
as_table_ident.dbplyr_schema <- function(x, ..., error_call = caller_env()) {
  new_table_ident(
    table = unclass(x$table),
    schema = unclass(x$schema)
  )
}

#' @export
as_table_ident.dbplyr_catalog <- function(x, ..., error_call = caller_env()) {
  new_table_ident(
    table = unclass(x$table),
    schema = unclass(x$schema),
    catalog = unclass(x$catalog)
  )
}

#' @export
as_table_ident.Id <- function(x, ..., error_call = caller_env()) {
  id <- x@name
  nms <- names(id)
  known_names <- c("catalog", "schema", "table")
  unknown_names <- setdiff(nms, known_names)
  if (!is_empty(unknown_names)) {
    cli_abort(c(
      "{.arg table} is an <Id> with unknown names {.val {unknown_names}}.",
      i = "Only {.val {known_names}} are supported."
    ), call = error_call)
  }

  new_table_ident(
    table = if ("table" %in% nms) id[["table"]] else NA_character_,
    schema = if ("schema" %in% nms) id[["schema"]] else NA_character_,
    catalog = if ("catalog" %in% nms) id[["catalog"]] else NA_character_
  )
}

as_table_ident_or_sql <- function(x, ..., error_call = caller_env()) {
  if (is.sql(x)) {
    return(x)
  }

  as_table_ident(x, error_call = error_call)
}

#' @export
format.dbplyr_table_ident <- function(x, ..., sep = ".", con = NULL) {
  quoted <- vctrs::field(x, "quoted")
  con <- con %||% simulate_dbi()
  x <- quote_table_ident(x, con)

  collapse_table_ident(x, sep = sep)
}

is_table_ident <- function(x) {
  inherits(x, "dbplyr_table_ident")
}

#' @export
escape.dbplyr_table_ident <- function(x, parens = FALSE, collapse = ", ", con = NULL) {
  # this ignores `parens` and `collapse`; at least for now
  x_quoted <- format(x, con = con)

  canonical_alias <- purrr::map_chr(x, ~ table_ident_name(.x) %||% "")
  alias <- table_ident_alias(x) %||% vctrs::vec_rep("", vctrs::vec_size(x))

  if (db_supports_table_alias_with_as(con)) {
    as_sql <- style_kw(" AS ")
  } else {
    as_sql <- " "
  }

  alias_esc <- sql_escape_ident(con, alias)
  out <- ifelse(
    alias == "" | alias == canonical_alias,
    x_quoted,
    paste0(x_quoted, as_sql, alias_esc)
  )

  sql_vector(out, parens, collapse, con = con)
}

quote_table_ident <- function(x, con) {
  quoted <- vctrs::field(x, "quoted")

  for (field in c("table", "schema", "catalog")) {
    xf <- vctrs::field(x, field)
    idx <- !is.na(xf) & !quoted
    xf[idx] <- sql_escape_ident(con, xf[idx])
    x <- vctrs::`field<-`(x, field, xf)
  }

  x
}

collapse_table_ident <- function(x, sep = ".") {
  table <- vctrs::field(x, "table")
  schema <- vctrs::field(x, "schema")
  catalog <- vctrs::field(x, "catalog")

  out <- table
  out[!is.na(schema)] <- paste0(schema, sep, table)[!is.na(schema)]
  out[!is.na(catalog)] <- paste0(catalog, sep, schema, sep, table)[!is.na(catalog)]

  out
}

table_ident_name <- function(x) {
  table <- vctrs::field(x, "table")
  quoted <- vctrs::field(x, "quoted")
  if (quoted) {
    NULL
  } else {
    table
  }
}

table_ident_alias <- function(x) {
  alias <- vctrs::field(x, "alias")
  if (all(is.na(alias))) {
    return(NULL)
  }

  alias
}

set_table_ident_alias <- function(x, alias) {
  alias <- alias %||% vctrs::vec_rep(NA_character_, times = vctrs::vec_size(x))
  vctrs::`field<-`(x, "alias", alias)
}

as_from <- function(x, ..., arg = caller_arg(x), error_call = caller_env()) {
  check_table_ident(x, sql = TRUE)
  if (!inherits(x, "sql")) {
    x <- as_table_ident(x)
  }

  x
}

table_ident_to_id <- function(x) {
  vctrs::vec_check_size(x, 1)

  quoted <- vctrs::field(x, "quoted")
  if (quoted) {
    out <- DBI::SQL(sql)
    return(out)
  }

  catalog <- vctrs::field(x, "catalog")
  schema <- vctrs::field(x, "schema")
  table <- vctrs::field(x, "table")

  if (!is.na(catalog)) {
    DBI::Id(catalog = catalog, schema = schema, table = table)
  } else if (!is.na(schema)) {
    DBI::Id(schema = schema, table = table)
  } else {
    DBI::Id(table = table)
  }
}
