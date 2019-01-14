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

  make_tbl(c(subclass, "sql", "lazy"), src = src, ops = ops)
}

#' @export
same_src.tbl_sql <- function(x, y) {
  if (!inherits(y, "tbl_sql")) return(FALSE)
  same_src(x$src, y$src)
}

# Grouping methods -------------------------------------------------------------

#' @export
group_size.tbl_sql <- function(x) {
  df <- x %>%
    summarise(n = n()) %>%
    collect()
  df$n
}

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
    if (length(grps) > 0) c("Groups" = commas(grps)),
    if (length(sort) > 0) c("Ordered by" = commas(deparse_all(sort)))
  )
}

#' @export
pull.tbl_sql <- function(.data, var = -1) {
  expr <- enquo(var)
  var <- dplyr:::find_var(expr, tbl_vars(.data))

  .data <- ungroup(.data)
  .data <- select(.data, !! sym(var))
  .data <- collect(.data)
  .data[[1]]
}

#' @export
dimnames.tbl_sql <- function(x) {
  list(NULL, op_vars(x$ops))
}

#' @export
dim.tbl_sql <- function(x) {
  c(NA, length(op_vars(x$ops)))
}

#' @export
tail.tbl_sql <- function(x, n = 6L, ...) {
  stop("tail() is not supported by sql sources", call. = FALSE)
}

# Copying ----------------------------------------------------------------------

#' Force computation of query
#'
#' `collapse()` creates a subquery; `compute()` stores the results in a
#' remote table; `collect()` donwloads the results into the current
#' R session.
#'
#' @export
#' @param x A `tbl_sql`
collapse.tbl_sql <- function(x, ...) {
  sql <- db_sql_render(x$src$con, x)

  tbl(x$src, sql) %>%
    group_by(!!! syms(op_grps(x))) %>%
    add_op_order(op_sort(x))
}

#' @rdname collapse.tbl_sql
#' @param name Table name in remote database.
#' @param temporary Should the table be temporary (`TRUE`, the default`) or
#'   persistent (`FALSE`)?
#' @inheritParams copy_to.src_sql
#' @export
compute.tbl_sql <- function(x, name = random_table_name(), temporary = TRUE,
                            unique_indexes = list(), indexes = list(),
                            analyze = TRUE, ...) {

  vars <- op_vars(x)
  assert_that(all(unlist(indexes) %in% vars))
  assert_that(all(unlist(unique_indexes) %in% vars))

  x_aliased <- select(x, !!! syms(vars)) # avoids problems with SQLite quoting (#1754)
  sql <- db_sql_render(x$src$con, x_aliased$ops)

  name <- db_compute(x$src$con, name, sql,
    temporary = temporary,
    unique_indexes = unique_indexes,
    indexes = indexes,
    analyze = analyze,
    ...
  )

  tbl(x$src, name) %>%
    group_by(!!! syms(op_grps(x))) %>%
    add_op_order(op_sort(x))
}

#' @rdname collapse.tbl_sql
#' @param n Number of rows to fetch. Defaults to `Inf`, meaning all rows.
#' @param warn_incomplete Warn if `n` is less than the number of result rows?
#' @export
collect.tbl_sql <- function(x, ..., n = Inf, warn_incomplete = TRUE) {
  assert_that(length(n) == 1, n > 0L)
  if (n == Inf) {
    n <- -1
  } else {
    # Gives the query planner information that it might be able to take
    # advantage of
    x <- head(x, n)
  }

  sql <- db_sql_render(x$src$con, x)
  out <- db_collect(x$src$con, sql, n = n, warn_incomplete = warn_incomplete)
  grouped_df(out, intersect(op_grps(x), names(out)))
}
