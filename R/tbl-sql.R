#' Create an SQL tbl (abstract)
#'
#' Generally, you should no longer need to provide a custom `tbl()`
#' method you you can default `tbl.DBIConnect` method.
#'
#' @keywords internal
#' @export
#' @param subclass name of subclass
#' @param ... needed for agreement with generic. Not otherwise used.
#' @param vars DEPRECATED
tbl_sql <- function(subclass, src, from, ..., vars = NULL) {
  # If not literal sql, must be a table identifier
  from <- as.sql(from)

  if (!missing(vars)) {
    warning("`vars` argument is deprecated as it is no longer needed", call. = FALSE)
  }

  vars <- vars %||% db_query_fields(src$con, from)
  ops <- op_base_remote(from, vars)

  dplyr::make_tbl(c(subclass, "sql", "lazy"), src = src, ops = ops)
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
as.data.frame.tbl_sql <- function(x, row.names = NULL, optional = NULL,
                                  ..., n = Inf) {
  as.data.frame(collect(x, n = n))
}

#' @export
tbl_sum.tbl_sql <- function(x) {
  grps <- op_grps(x$ops)
  sort <- op_sort(x$ops)
  c(
    "Source" = tbl_desc(x),
    "Database" = db_desc(x$src$con),
    "Groups" = if (length(grps) > 0) commas(grps),
    "Ordered by" = if (length(sort) > 0) commas(deparse_all(sort))
  )
}

tbl_desc <- function(x) {
  paste0(
    op_desc(x$ops),
    " [",
    op_rows(x$ops),
    " x ",
    big_mark(op_cols(x$ops)),
    "]"
  )
}
