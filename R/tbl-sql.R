#' Create an SQL tbl (abstract)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is no longer needed, since backends don't need to create
#' their own custom classes. Instead, rely on the default `tbl.DBIConnection()`
#' method.
#'
#' @keywords internal
#' @export
#' @inheritParams tbl.src_dbi
#' @param subclass name of subclass
#' @param ... needed for agreement with generic. Not otherwise used.
#' @param check_from `r lifecycle::badge("deprecated")`
tbl_sql <- function(
  subclass,
  src,
  from,
  ...,
  vars = NULL,
  check_from = deprecated()
) {
  lifecycle::deprecate_soft("2.6.0", "tbl_sql()")

  check_dots_empty()

  if (lifecycle::is_present(check_from)) {
    lifecycle::deprecate_warn("2.5.0", "tbl_sql(check_from)")
  }

  db_table(src$con, from, vars = vars, subclass = subclass)
}

find_variables <- function(con, from, call = caller_env()) {
  source <- as_table_source(from, con = con, error_call = call)

  withCallingHandlers(
    dbplyr_query_fields(con, source),
    error = function(err) {
      is_suspicious <- is_bare_string(from) && grepl(".", from, fixed = TRUE)
      if (!is_suspicious) {
        return()
      }

      cli::cli_abort(
        c(
          "Failed to find table {source}.",
          i = "Did you mean {.code from = I({.str {from}})}?"
        ),
        parent = err,
        call = call
      )
    }
  )
}

new_tbl_sql <- function(con, source, vars, subclass = NULL) {
  check_con(con)
  check_table_source(source)
  check_character(vars)

  lazy_query <- lazy_query_remote(source, vars)
  new_tbl_lazy(con, lazy_query, subclass = subclass)
}

# The goal of this function is to paper over all of the historical differences
# and provide a consistent forward looking interface, recognising that the
# primary different between the three cases (tbl(), lazy_frame(), and
# copy_inline()) is the base query
new_tbl_lazy <- function(con, query, subclass = NULL) {
  check_con(con)
  if (!inherits(query, "lazy_base_query")) {
    stop_input_type(query, "a <lazy_base_query>")
  }

  is_sql <- !inherits(query, "lazy_base_local_query")
  subclass <- c(
    subclass %||% class(con)[[1]],
    if (is_sql) c("dbi", "sql"),
    "lazy"
  )

  dplyr::make_tbl(
    subclass,
    con = con,
    src = src_dbi(con), # for backward compatibility
    lazy_query = query
  )
}

#' @importFrom dplyr same_src
#' @export
same_src.tbl_sql <- function(x, y) {
  inherits(y, "tbl_sql") && identical(x$con, y$con)
}

# Grouping methods -------------------------------------------------------------

#' @importFrom dplyr group_size
#' @export
group_size.tbl_sql <- function(x) {
  df <- x |>
    summarise(n = n()) |>
    collect()
  df$n
}

#' @importFrom dplyr n_groups
#' @export
n_groups.tbl_sql <- function(x) {
  if (length(groups(x)) == 0) {
    return(1L)
  }

  df <- x |>
    summarise() |>
    ungroup() |>
    summarise(n = n()) |>
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
as.data.frame.tbl_sql <- function(
  x,
  row.names = NULL,
  optional = NULL,
  ...,
  n = Inf
) {
  as.data.frame(collect(x, n = n))
}

#' @importFrom pillar tbl_format_header
#' @export
tbl_format_header.tbl_sql <- function(x, setup, ...) {
  grps <- op_grps(x$lazy_query)
  sort <- op_sort(x$lazy_query)
  named_header <- c(
    # Can be overwritten by tbl_format_header.tbl_lazy:
    "A query" = paste0("?? x ", length(op_vars(x))),
    "Database" = db_connection_describe(x$con),
    "Groups" = if (length(grps) > 0) commas(grps),
    "Ordered by" = if (length(sort) > 0) commas(deparse_all(sort))
  )
  header <- paste0(
    pillar::align(paste0(names(named_header), ":")),
    " ",
    named_header
  )
  pillar::style_subtle(paste0("# ", header))
}
