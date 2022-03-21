#' Copy a local data frame to a remote database
#'
#' @description
#' This is an implementation of the dplyr [copy_to()] generic and it mostly
#' a wrapper around [DBI::dbWriteTable()].
#'
#' It is useful for copying small amounts of data to a database for examples,
#' experiments, and joins. By default, it creates temporary tables which are
#' only visible within the current connection to the database.
#'
#' @export
#' @param df A local data frame, a `tbl_sql` from same source, or a `tbl_sql`
#'   from another source. If from another source, all data must transition
#'   through R in one pass, so it is only suitable for transferring small
#'   amounts of data.
#' @param types a character vector giving variable types to use for the columns.
#'    See <https://www.sqlite.org/datatype3.html> for available types.
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
#' @param in_transaction Should the table creation be wrapped in a transaction?
#'   This typically makes things faster, but you may want to suppress if the
#'   database doesn't support transactions, or you're wrapping in a transaction
#'   higher up (and your database doesn't support nested transactions.)
#' @inheritParams dplyr::copy_to
#' @inherit arrange.tbl_lazy return
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' df <- data.frame(x = 1:5, y = letters[5:1])
#' db <- copy_to(src_memdb(), df)
#' db
#'
#' df2 <- data.frame(y = c("a", "d"), fruit = c("apple", "date"))
#' # copy_to() is called automatically if you set copy = TRUE
#' # in the join functions
#' db %>% left_join(df2, copy = TRUE)
#' @importFrom dplyr copy_to
copy_to.src_sql <- function(dest, df, name = deparse(substitute(df)),
                            overwrite = FALSE, types = NULL, temporary = TRUE,
                            unique_indexes = NULL, indexes = NULL,
                            analyze = TRUE, ...,
                            in_transaction = TRUE
                            ) {
  assert_that(is.flag(temporary))

  if (!is.data.frame(df) && !inherits(df, "tbl_sql")) {
    abort("`df` must be a local dataframe or a remote tbl_sql")
  }

  name <- as.sql(name, con = dest$con)

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
      in_transaction = in_transaction,
      ...
    )

    out <- tbl(dest, name)
  }

  invisible(out)
}

#' @export
db_values <- function(con, df) {
  if (!inherits(df, "data.frame")) {
    abort("`df` needs to be a data.frame.")
  }

  if (ncol(df) == 0) {
    abort("`df` needs at least one column.")
  }

  tbl(
    src = con,
    from = sql_values(con, df),
    vars = colnames(df)
  )
}

sql_values <- function(con, df) {
  UseMethod("sql_values")
}

#' @export
sql_values.DBIConnection <- function(con, df) {
  sql_values_clause(con, df)
}

sql_values_clause <- function(con, df, row = FALSE) {
  # The query consists of two parts:
  # * An outer select which converts the values to the correct types
  # * A subquery which provides:
  #   * named NULLs (needed as e.g. SQLite cannot name `VALUES`)
  #   * the actual values without names

  sim_data <- rep_named(colnames(df), list(NULL))
  cols_clause <- escape(sim_data, con = con, parens = FALSE)
  null_row_clause <- build_sql("SELECT ", cols_clause, " WHERE 0 = 1", con = con)

  empty_df <- nrow(df) == 0L
  if (empty_df) {
    subquery <- null_row_clause
  } else {
    escaped_values <- purrr::map(df, escape, con = con, collapse = NULL, parens = FALSE)
    rows <- rlang::exec(paste, !!!escaped_values, sep = ", ")
    values_clause <- sql(paste0(if (row) "ROW", "(", rows, ")", collapse = ",\n  "))

    union_query <- set_op_query(
      null_row_clause,
      build_sql("VALUES\n  ", values_clause, con = con),
      type = "UNION ALL"
    )

    subquery <- sql_render(union_query, con = con)
  }


  typed_cols <- purrr::map2_chr(df, colnames(df), ~ select_typed_col(.x, ident(.y), con = con))
  select_clause <- sql_vector(typed_cols, parens = FALSE, collapse = ", ", con = con)

  build_sql(
    "SELECT ", select_clause, "\n",
    "FROM ", sql_subquery(con, subquery, name = "values_table"),
    con = con
  )
}

select_typed_col <- function(type, col, con) {
  UseMethod("select_typed_col")
}

#' @export
select_typed_col.default <- function(type, col, con) {
  translate_sql(!!col, con = con)
}

#' @export
select_typed_col.character <- function(type, col, con) {
  translate_sql(!!col, con = con)
}

#' @export
select_typed_col.factor <- function(type, col, con) {
  translate_sql(!!col, con = con)
}

#' @export
select_typed_col.integer <- function(type, col, con) {
  translate_sql(as.integer(!!col), con = con)
}

#' @export
select_typed_col.numeric <- function(type, col, con) {
  translate_sql(as.numeric(!!col), con = con)
}

#' @export
select_typed_col.logical <- function(type, col, con) {
  translate_sql(as.logical(!!col), con = con)
}

#' @export
select_typed_col.Date <- function(type, col, con) {
  translate_sql(as.Date(!!col), con = con)
}

#' @export
select_typed_col.POSIXt <- function(type, col, con) {
  translate_sql(as.POSIXct(!!col), con = con)
}

#' @export
select_typed_col.integer64 <- function(type, col, con) {
  translate_sql(as.integer64(!!col), con = con)
}

#' @importFrom dplyr auto_copy
#' @export
auto_copy.tbl_sql <- function(x, y, copy = FALSE, ...) {
  copy_to(x$src, as.data.frame(y), unique_table_name(), ...)
}
