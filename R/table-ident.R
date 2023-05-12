new_table_ident <- function(...,
                            table = NA_character_,
                            schema = NA_character_,
                            catalog = NA_character_,
                            sql = NA_character_,
                            error_call = caller_env()) {
  check_dots_empty(call = error_call)

  check_character(table, character(), call = error_call)
  check_character(schema, character(), call = error_call)
  check_character(catalog, character(), call = error_call)
  check_character(sql, character(), call = error_call)

  n <- vctrs::vec_size_common(
    table = table,
    schema = schema,
    catalog = catalog,
    sql = sql,
    .call = error_call
  )
  data <- list(
    table = vctrs::vec_recycle(table, n, call = error_call),
    schema = vctrs::vec_recycle(schema, n, call = error_call),
    catalog = vctrs::vec_recycle(catalog, n, call = error_call),
    sql = vctrs::vec_recycle(sql, n, call = error_call),
    name = vctrs::vec_rep(NA_character_, n)
  )

  purrr::pmap(
    data,
    function(table, schema, catalog, sql, name) {
      check_db_table_args(
        table = table,
        schema = schema,
        catalog = catalog,
        sql = sql,
        call = error_call
      )
    }
  )

  vctrs::new_rcrd(data, class = "dbplyr_table_ident")
}

check_db_table_args <- function(table,
                                schema,
                                catalog,
                                sql,
                                call = caller_env()) {
  if (!is.na(catalog) && is.na(schema)) {
    cli_abort("Must supply {.arg schema} when {.arg catalog} is supplied.", call = call)
  }
  if (!is.na(schema) && is.na(table)) {
    cli_abort("Must supply {.arg table} when {.arg schema} is supplied.", call = call)
  }

  if (!is.na(sql) && !is.na(table)) {
    cli_abort("Can only supply either {.arg sql} or {.arg table}.", call = call)
  }
}

as_table_ident <- function(x, ..., error_call = current_env()) {
  check_dots_empty()
  UseMethod("as_table_ident")
}

#' @export
as_table_ident.dbplyr_table_ident <- function(x, ..., error_call = current_env()) {
  x
}

#' @export
as_table_ident.character <- function(x, ..., error_call = current_env()) {
  new_table_ident(table = vctrs::vec_data(x))
}

#' @export
as_table_ident.ident <- function(x, ..., error_call = current_env()) {
  new_table_ident(table = vctrs::vec_data(x))
}

#' @export
as_table_ident.ident_q <- function(x, ..., error_call = current_env()) {
  new_table_ident(sql = x)
}

#' @export
as_table_ident.sql <- function(x, ..., error_call = current_env()) {
  cli_abort("Can't convert {.cls sql} to a table ident object.", call = error_call)
}

#' @export
as_table_ident.dbplyr_schema <- function(x, ..., error_call = current_env()) {
  new_table_ident(
    table = unclass(x$table),
    schema = unclass(x$schema)
  )
}

#' @export
as_table_ident.dbplyr_catalog <- function(x, ..., error_call = current_env()) {
  new_table_ident(
    table = unclass(x$table),
    schema = unclass(x$schema),
    catalog = unclass(x$catalog)
  )
}

#' @export
as_table_ident.Id <- function(x, ..., error_call = current_env()) {
  id <- x@name
  nms <- names(id)
  unknown_names <- setdiff(nms, c("catalog", "schema", "table"))
  if (!is_empty(unknown_names)) {
    # TODO
    cli_abort("{.arg table} is an <Id> with unknown names {.val {unknown_names}}.", call = error_call)
  }

  new_table_ident(
    table = if ("table" %in% nms) id[["table"]] else NA_character_,
    schema = if ("schema" %in% nms) id[["schema"]] else NA_character_,
    catalog = if ("catalog" %in% nms) id[["catalog"]] else NA_character_
  )
}

as_table_ident_or_sql <- function(x, ..., error_call = current_env()) {
  if (is.sql(x)) {
    return(x)
  }

  as_table_ident(x, error_call = error_call)
}

#' @export
format.dbplyr_table_ident <- function(x, ..., sep = ".") {
  out <- vctrs::field(x, "table")
  schema <- vctrs::field(x, "schema")
  out <- ifelse(is.na(schema), out, paste0(schema, sep, out))
  catalog <- vctrs::field(x, "catalog")
  out <- ifelse(is.na(catalog), out, paste0(catalog, sep, out))

  sql <- vctrs::field(x, "sql")
  ifelse(is.na(sql), out, sql)
}

is_table_ident <- function(x) {
  inherits(x, "dbplyr_table_ident")
}

#' @export
escape.dbplyr_table_ident <- function(x, parens = FALSE, collapse = ", ", con = NULL) {
  # this ignores `parens` and `collapse`; at least for now
  x_quoted <- quote_table_ident(x, con)
  sql_vector(x_quoted, parens, collapse, con = con)
}

quote_table_ident <- function(x, con) {
  for (field in c("table", "schema", "catalog")) {
    xf <- vctrs::field(x, field)
    idx <- !is.na(xf)
    xf[idx] <- sql_escape_ident(con, xf[idx])
    x <- vctrs::`field<-`(x, field, xf)
  }

  out <- format(x)
  nms <- names(x) %||% vctrs::vec_rep("", vctrs::vec_size(x))
  names_to_as(out, nms, con = con)
}

table_ident_name <- function(x) {
  # TODO should error if `x$table` is NA?
  # TODO or should this support `x$sql`?
  table <- vctrs::field(x, "table")
  if (is.na(table)) {
    NULL
  } else {
    table
  }
}

#' @export
names.dbplyr_table_ident <- function(x) {
  names <- vctrs::field(x, "name")
  # TODO remove hack later
  if (all(is.na(names))) {
    return(NULL)
  }

  names
}

#' @export
`names<-.dbplyr_table_ident` <- function(x, value) {
  value <- value %||% vctrs::vec_rep(NA_character_, times = vctrs::vec_size(x))
  vctrs::`field<-`(x, "name", value)
}

as_from <- function(x, ..., arg = caller_arg(x), error_call = caller_env()) {
  # TODO should only allow size = 1
  if (inherits(x, "dbplyr_table_ident")) {
    return(x)
  }
  if (inherits(x, "sql")) {
    return(x)
  }

  check_table_ident(x, sql = TRUE)
  as_table_ident(x)
}
