#' Force computation of query
#'
#' `collapse()` creates a subquery; `compute()` stores the results in a
#' remote table; `collect()` downloads the results into the current
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

# compute -----------------------------------------------------------------

#' @rdname collapse.tbl_sql
#' @param name Table name in remote database.
#' @param temporary Should the table be temporary (`TRUE`, the default`) or
#'   persistent (`FALSE`)?
#' @inheritParams copy_to.src_sql
#' @export
compute.tbl_sql <- function(x,
                            name = unique_table_name(),
                            temporary = TRUE,
                            unique_indexes = list(),
                            indexes = list(),
                            analyze = TRUE,
                            ...) {

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

#' @export
#' @rdname db_copy_to
db_compute <- function(con,
                      table,
                      sql,
                      temporary = TRUE,
                      unique_indexes = list(),
                      indexes = list(),
                      analyze = TRUE,
                      ...) {
  UseMethod("db_compute")
}

#' @export
db_compute.DBIConnection <- function(con,
                                     table,
                                     sql,
                                     temporary = TRUE,
                                     unique_indexes = list(),
                                     indexes = list(),
                                     analyze = TRUE,
                                     ...) {
  if (!is.list(indexes)) {
    indexes <- as.list(indexes)
  }
  if (!is.list(unique_indexes)) {
    unique_indexes <- as.list(unique_indexes)
  }

  table <- db_save_query(con, sql, table, temporary = temporary)
  db_create_indexes(con, table, unique_indexes, unique = TRUE)
  db_create_indexes(con, table, indexes, unique = FALSE)
  if (analyze) db_analyze(con, table)

  table
}

# collect -----------------------------------------------------------------

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

#' @export
#' @rdname db_copy_to
db_collect <- function(con, sql, n = -1, warn_incomplete = TRUE, ...) {
  UseMethod("db_collect")
}

#' @export
db_collect.DBIConnection <- function(con, sql, n = -1, warn_incomplete = TRUE, ...) {
  res <- dbSendQuery(con, sql)
  tryCatch({
    out <- dbFetch(res, n = n)
    if (warn_incomplete) {
      res_warn_incomplete(res, "n = Inf")
    }
  }, finally = {
    dbClearResult(res)
  })

  out
}

# sql_render --------------------------------------------------------------

# Used by implyr
#' @rdname db_copy_to
#' @export
db_sql_render <- function(con, sql, ...) {
  UseMethod("db_sql_render")
}

#' @export
db_sql_render.DBIConnection <- function(con, sql, ...) {
  sql_render(sql, con = con, ...)
}
