#' Copy a local data frame to a DBI backend.
#'
#' This [copy_to()] method works for all DBI sources. It is useful for
#' copying small amounts of data to a database for examples, experiments,
#' and joins. By default, it creates temporary tables which are typically
#' only visible to the current connection to the database.
#'
#' @export
#' @param df A local data frame, a `tbl_sql` from same source, or a `tbl_sql`
#'   from another source. If from another source, all data must transition
#'   through R in one pass, so it is only suitable for transferring small
#'   amounts of data.
#' @param types a character vector giving variable types to use for the columns.
#'    See \url{http://www.sqlite.org/datatype3.html} for available types.
#' @param temporary if `TRUE`, will create a temporary table that is
#'   local to this connection and will be automatically deleted when the
#'   connection expires
#' @param unique_indexes a list of character vectors. Each element of the list
#'   will create a new unique index over the specified column(s). Duplicate rows
#'   will result in failure.
#' @param indexes a list of character vectors. Each element of the list
#'   will create a new index.
#' @param analyze if `TRUE` (the default), will automatically ANALYZE the
#'   new table so that the query optimiser has useful information.
#' @inheritParams dplyr::copy_to
#' @return A [tbl()] object (invisibly).
#' @examples
#' library(dplyr)
#' set.seed(1014)
#'
#' mtcars$model <- rownames(mtcars)
#' mtcars2 <- src_memdb() %>%
#'   copy_to(mtcars, indexes = list("model"), overwrite = TRUE)
#' mtcars2 %>% filter(model == "Hornet 4 Drive")
#'
#' cyl8 <- mtcars2 %>% filter(cyl == 8)
#' cyl8_cached <- copy_to(src_memdb(), cyl8)
#'
#' # copy_to is called automatically if you set copy = TRUE
#' # in the join functions
#' df <- tibble(cyl = c(6, 8))
#' mtcars2 %>% semi_join(df, copy = TRUE)
#' @importFrom dplyr copy_to
copy_to.src_sql <- function(dest, df, name = deparse(substitute(df)),
                            overwrite = FALSE, types = NULL, temporary = TRUE,
                            unique_indexes = NULL, indexes = NULL,
                            analyze = TRUE, ...) {
  assert_that(is_string(name), is.flag(temporary))

  if (!is.data.frame(df) && !inherits(df, "tbl_sql")) {
    stop("`df` must be a local dataframe or a remote tbl_sql", call. = FALSE)
  }

  if (inherits(df, "tbl_sql") && same_src(df$src, dest)) {
    out <- compute(df,
      name = name,
      temporary = temporary,
      unique_indexes = unique_indexes,
      indexes = indexes,
      analyze = analyze,
      ...
    )
  } else {
    # avoid S4 dispatch problem in dbSendPreparedQuery
    df <- as.data.frame(collect(df))

    name <- db_copy_to(dest$con, name, df,
      overwrite = overwrite,
      types = types,
      temporary = temporary,
      unique_indexes = unique_indexes,
      indexes = indexes,
      analyze = analyze,
      ...
    )

    out <- tbl(dest, name)
  }

  invisible(out)
}

#' @importFrom dplyr auto_copy
#' @export
auto_copy.tbl_sql <- function(x, y, copy = FALSE, ...) {
  copy_to(x$src, as.data.frame(y), unique_table_name(), ...)
}

#' More db generics
#'
#' These are new, so not included in dplyr for backward compatibility
#' purposes.
#'
#' @keywords internal
#' @export
db_copy_to <-  function(con, table, values,
                        overwrite = FALSE, types = NULL, temporary = TRUE,
                        unique_indexes = NULL, indexes = NULL,
                        analyze = TRUE, ...) {
  UseMethod("db_copy_to")
}

#' @export
db_copy_to.DBIConnection <- function(con, table, values,
                            overwrite = FALSE, types = NULL, temporary = TRUE,
                            unique_indexes = NULL, indexes = NULL,
                            analyze = TRUE, ...) {

  types <- types %||% db_data_type(con, values)
  names(types) <- names(values)

  with_transaction(con, {
    # Only remove if it exists; returns NA for MySQL
    if (overwrite && !is_false(db_has_table(con, table))) {
      db_drop_table(con, table, force = TRUE)
    }

    table <- db_write_table(con, table, types = types, values = values, temporary = temporary)
    db_create_indexes(con, table, unique_indexes, unique = TRUE)
    db_create_indexes(con, table, indexes, unique = FALSE)
    if (analyze) db_analyze(con, table)
  })

  table
}

# Don't use `tryCatch()` because it messes with the callstack
with_transaction <- function(con, code) {
  db_begin(con)
  on.exit(db_rollback(con))

  code

  on.exit()
  db_commit(con)
}
