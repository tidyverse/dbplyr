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
#' @param name Name of new remote table. Use a string to create the table
#'   in the current catalog/schema. Use `I()` to create the table in a specific
#'   catalog/schema, e.g. `I("schema.table")`.
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
#' @seealso [copy_inline()] to use small data in an SQL query without actually
#'   writing to a table.
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
#' db |> left_join(df2, copy = TRUE)
#' @importFrom dplyr copy_to
copy_to.src_sql <- function(
  dest,
  df,
  name = deparse(substitute(df)),
  overwrite = FALSE,
  types = NULL,
  temporary = TRUE,
  unique_indexes = NULL,
  indexes = NULL,
  analyze = TRUE,
  ...,
  in_transaction = TRUE
) {
  check_bool(temporary)

  if (!is.data.frame(df) && !inherits(df, "tbl_sql")) {
    cli_abort("{.var df} must be a local dataframe or a remote tbl_sql")
  }

  name <- as_table_path(name, dest$con)

  if (inherits(df, "tbl_sql") && same_src(df, dest)) {
    out <- compute(
      df,
      name = name,
      temporary = temporary,
      overwrite = overwrite,
      unique_indexes = unique_indexes,
      indexes = indexes,
      analyze = analyze,
      ...
    )
  } else {
    # avoid S4 dispatch problem in dbSendPreparedQuery
    df <- as.data.frame(collect(df))

    name <- db_copy_to(
      dest$con,
      name,
      df,
      overwrite = overwrite,
      types = types,
      temporary = temporary,
      unique_indexes = unique_indexes,
      indexes = indexes,
      analyze = analyze,
      in_transaction = in_transaction,
      ...
    )
    out <- new_tbl_sql(dest$con, name, vars = names(df))
  }

  invisible(out)
}

#' @importFrom dplyr auto_copy
#' @export
auto_copy.tbl_sql <- function(x, y, copy = FALSE, ..., types = NULL) {
  # NOTE: copy must be TRUE here - dplyr's generic errors on copy = FALSE
  # The caller should use dbplyr_auto_copy() to handle all copy values
  dbplyr_auto_copy(x, y, copy = "temp-table", ..., types = types)
}

# Handles all copy argument values including "inline"
# Use this instead of calling auto_copy() directly
dbplyr_auto_copy <- function(
  x,
  y,
  copy,
  ...,
  types = NULL,
  call = caller_env()
) {
  if (same_src(x, y)) {
    return(y)
  }

  copy <- as_copy(copy, error_call = call)
  if (copy == "none") {
    cli_abort(
      c(
        "{.arg x} and {.arg y} must share the same source.",
        i = "Set {.code copy = TRUE} to copy {.arg y} to a temporary table in the same database as {.arg x}.",
        i = "Set {.code copy = \"inline\"} to use {.arg y} inline without creating a temporary table."
      ),
      call = call
    )
  } else if (copy == "inline") {
    copy_inline(x$con, as.data.frame(y), types = types)
  } else if (copy == "temp-table") {
    y <- as.data.frame(y)
    table <- unique_table_name()
    table <- db_copy_to(x$con, table = table, values = y, ..., types = types)
    new_tbl_sql(con = x$con, source = table, vars = colnames(y))
  }
}

as_copy <- function(copy, error_call = caller_env()) {
  if (isTRUE(copy)) {
    "temp-table"
  } else if (isFALSE(copy)) {
    "none"
  } else {
    arg_match(copy, c("none", "temp-table", "inline"), error_call = error_call)
  }
}
