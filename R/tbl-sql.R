#' Create an SQL tbl (abstract)
#'
#' Generally, you should no longer need to provide a custom `tbl()`
#' method.
#' The default `tbl.DBIConnect` method should work in most cases.
#'
#' @keywords internal
#' @export
#' @param subclass name of subclass
#' @param ... needed for agreement with generic. Not otherwise used.
#' @param vars Provide column names as a character vector
#'   to avoid retrieving them from the database.
#'   Mainly useful for better performance when creating
#'   multiple `tbl` objects.
#' @param check_from Check if `from` is likely misspecified SQL or a table in a schema.
tbl_sql <- function(subclass, src, from, ..., vars = NULL, check_from = TRUE) {
  check_dots_used()
  check_character(vars, allow_null = TRUE)

  from <- as_from(from)
  if (check_from) {
    check_from_for_query_or_schema(from)
  }

  vars <- vars %||% dbplyr_query_fields(src$con, from)

  dplyr::make_tbl(
    c(subclass, "sql", "lazy"),
    src = src,
    lazy_query = lazy_query_remote(from, vars)
  )
}

check_from_for_query_or_schema <- function(from) {
  if (!is_table_ident(from)) {
    return()
  }

  table <- vctrs::field(from, "table")
  schema <- vctrs::field(from, "schema")

  if (grepl(" from ", tolower(table), fixed = TRUE)) {
    cli::cli_inform(c(
      "It looks like you tried to incorrectly use an SQL query as source.",
      i = "If you want to select from a query wrap it in {.fn sql}.",
      i = "If your table actually contains {.val FROM} in the name use {.arg check_from = FALSE} to silence this message."
    ))
    return()
  }

  if (grepl(".", table, fixed = TRUE) && is.na(schema)) {
    cli::cli_inform(c(
      "It looks like you tried to incorrectly use a table in a schema as source.",
      i = "If you want to specify a schema use {.fn in_schema} or {.fn in_catalog}.",
      i = "If your table actually contains {.val .} in the name use {.arg check_from = FALSE} to silence this message."
    ))
    return()
  }
}

#' @importFrom dplyr same_src
#' @export
same_src.tbl_sql <- function(x, y) {
  inherits(y, "tbl_sql") && same_src(x$src, y$src)
}

# Grouping methods -------------------------------------------------------------

#' @importFrom dplyr group_size
#' @export
group_size.tbl_sql <- function(x) {
  df <- x %>%
    summarise(n = n()) %>%
    collect()
  df$n
}

#' @importFrom dplyr n_groups
#' @export
n_groups.tbl_sql <- function(x) {
  if (length(groups(x)) == 0) return(1L)

  df <- x %>%
    summarise() %>%
    ungroup() %>%
    summarise(n = n()) %>%
    collect()
  df$n
}

# Standard data frame methods --------------------------------------------------

#' @export
print.tbl_sql <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  cat_line(format(x, ..., n = n, width = width, n_extra = n_extra))
  invisible(x)
}

#' @export
as.data.frame.tbl_sql <- function(x,
                                  row.names = NULL,
                                  optional = NULL,
                                  ...,
                                  n = Inf) {
  as.data.frame(collect(x, n = n))
}

#' @export
#' @importFrom tibble tbl_sum
tbl_sum.tbl_sql <- function(x) {
  tbl_sum_tbl_sql(x)
}

tbl_sum_tbl_sql <- function(x, desc = tbl_desc(x)) {
  grps <- op_grps(x$lazy_query)
  sort <- op_sort(x$lazy_query)
  c(
    "Source" = desc,
    "Database" = dbplyr_connection_describe(x$src$con),
    "Groups" = if (length(grps) > 0) commas(grps),
    "Ordered by" = if (length(sort) > 0) commas(deparse_all(sort))
  )
}

tbl_desc <- function(x, rows_total = NA_integer_) {
  paste0(
    op_desc(x$lazy_query),
    " [",
    op_rows(x$lazy_query, rows_total),
    " x ",
    big_mark(op_cols(x$lazy_query)),
    "]"
  )
}
