#' @include sql-dialect.R
NULL

methods::setClass(
  "DBIConnectionWithDialect",
  slots = c(
    con = "ANY",
    dialect = "ANY"
  )
)

#' Override the SQL dialect for a connection
#'
#' @description
#' `with_dialect()` wraps a database connection to use a specific SQL dialect
#' for SQL generation, while still using the original connection for database
#' operations.
#'
#' This is useful when you want to use a connection with an ODBC or JDBC driver
#' but want dbplyr to generate SQL for a specific database backend.
#'
#' @param con A database connection (class `DBIConnection`).
#' @param dialect A dialect object created by a `dialect_*()` function
#'   (e.g., [dialect_postgres()], [dialect_sqlite()]).
#' @return A connection object that uses the specified dialect for SQL
#'   generation.
#' @export
#' @examples
#' # Wrap an in-memory SQLite connection to use Postgres dialect
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' pg_con <- with_dialect(con, dialect_postgres())
#'
#' # SQL generation uses Postgres syntax
#' lf <- lazy_frame(x = 1, con = pg_con)
#' lf |> dplyr::mutate(y = sd(x))
#'
#' DBI::dbDisconnect(con)
with_dialect <- function(con, dialect) {
  check_con(con)
  if (!is_sql_dialect(dialect)) {
    cli_abort("{.arg dialect} must be a dialect object.")
  }
  methods::new("DBIConnectionWithDialect", con = con, dialect = dialect)
}

#' @export
sql_dialect.DBIConnectionWithDialect <- function(con) {
  con@dialect
}

#' @export
dbplyr_edition.DBIConnectionWithDialect <- function(con) {
  2L
}

#' @export
db_sql_render.DBIConnectionWithDialect <- function(
  con,
  sql,
  ...,
  cte = FALSE,
  sql_options = NULL
) {
  sql_render(sql, con = con, ..., sql_options = sql_options)
}

#' @export
db_col_types.DBIConnectionWithDialect <- function(con, table, call) {
  db_col_types(con@con, table, call)
}

#' @export
db_connection_describe.DBIConnectionWithDialect <- function(con, ...) {
  paste0(db_connection_describe(con@con, ...), " [custom dialect]")
}

#' @export
#' @importFrom dplyr copy_to
copy_to.DBIConnectionWithDialect <- function(
  dest,
  df,
  name = deparse(substitute(df)),
  ...
) {
  copy_to(src_dbi(dest), df, name = name, ...)
}

#' @export
db_copy_to.DBIConnectionWithDialect <- function(con, table, values, ...) {
  db_copy_to(con@con, table, values, ...)
}

#' @export
db_collect.DBIConnectionWithDialect <- function(con, sql, ...) {
  db_collect(con@con, sql, ...)
}

#' @export
db_compute.DBIConnectionWithDialect <- function(con, table, sql, ...) {
  db_compute(con@con, table, sql, ...)
}

#' @export
dbplyr_write_table.DBIConnectionWithDialect <- function(con, table, ...) {
  dbplyr_write_table(con@con, table, ...)
}

#' @export
db_table_drop_if_exists.DBIConnectionWithDialect <- function(con, table, ...) {
  db_table_drop_if_exists(con@con, table, ...)
}

#' @export
dbplyr_fill0.DBIConnectionWithDialect <- function(.con, ...) {
  dbplyr_fill0(sql_dialect(.con), ...)
}

# DBI method implementations that delegate to the wrapped connection
methods::setMethod(
  DBI::dbSendQuery,
  "DBIConnectionWithDialect",
  function(conn, statement, ...) {
    DBI::dbSendQuery(conn@con, statement, ...)
  }
)

methods::setMethod(
  DBI::dbGetQuery,
  "DBIConnectionWithDialect",
  function(conn, statement, ...) {
    DBI::dbGetQuery(conn@con, statement, ...)
  }
)

methods::setMethod(
  DBI::dbExecute,
  "DBIConnectionWithDialect",
  function(conn, statement, ...) {
    DBI::dbExecute(conn@con, statement, ...)
  }
)

methods::setMethod(
  DBI::dbWriteTable,
  "DBIConnectionWithDialect",
  function(conn, name, value, ...) {
    DBI::dbWriteTable(conn@con, name, value, ...)
  }
)

methods::setMethod(
  DBI::dbAppendTable,
  "DBIConnectionWithDialect",
  function(conn, name, value, ...) {
    DBI::dbAppendTable(conn@con, name, value, ...)
  }
)

methods::setMethod(
  DBI::dbExistsTable,
  "DBIConnectionWithDialect",
  function(conn, name, ...) {
    DBI::dbExistsTable(conn@con, name, ...)
  }
)

methods::setMethod(
  DBI::dbRemoveTable,
  "DBIConnectionWithDialect",
  function(conn, name, ...) {
    DBI::dbRemoveTable(conn@con, name, ...)
  }
)

methods::setMethod(
  DBI::dbListTables,
  "DBIConnectionWithDialect",
  function(conn, ...) {
    DBI::dbListTables(conn@con, ...)
  }
)

methods::setMethod(
  DBI::dbGetInfo,
  "DBIConnectionWithDialect",
  function(dbObj, ...) {
    DBI::dbGetInfo(dbObj@con, ...)
  }
)

methods::setMethod(
  DBI::dbBegin,
  "DBIConnectionWithDialect",
  function(conn, ...) {
    DBI::dbBegin(conn@con, ...)
  }
)

methods::setMethod(
  DBI::dbCommit,
  "DBIConnectionWithDialect",
  function(conn, ...) {
    DBI::dbCommit(conn@con, ...)
  }
)

methods::setMethod(
  DBI::dbRollback,
  "DBIConnectionWithDialect",
  function(conn, ...) {
    DBI::dbRollback(conn@con, ...)
  }
)

methods::setMethod(
  DBI::dbQuoteIdentifier,
  "DBIConnectionWithDialect",
  function(conn, x, ...) {
    DBI::dbQuoteIdentifier(conn@con, x, ...)
  }
)

methods::setMethod(
  DBI::dbQuoteString,
  "DBIConnectionWithDialect",
  function(conn, x, ...) {
    DBI::dbQuoteString(conn@con, x, ...)
  }
)

methods::setMethod(
  DBI::dbDataType,
  "DBIConnectionWithDialect",
  function(dbObj, obj, ...) {
    DBI::dbDataType(dbObj@con, obj, ...)
  }
)

methods::setMethod(
  DBI::dbDisconnect,
  "DBIConnectionWithDialect",
  function(conn, ...) {
    DBI::dbDisconnect(conn@con, ...)
  }
)

methods::setMethod(
  DBI::dbIsValid,
  "DBIConnectionWithDialect",
  function(dbObj, ...) {
    DBI::dbIsValid(dbObj@con, ...)
  }
)
