#' Cache and retrieve an `src_sqlite` of the Lahman baseball database.
#'
#' This creates an interesting database using data from the Lahman baseball
#' data source, provided by Sean Lahman at
#' <http://seanlahman.com/download-baseball-database/>, and
#' made easily available in R through the \pkg{Lahman} package by
#' Michael Friendly, Dennis Murphy and Martin Monkman. See the documentation
#' for that package for documentation of the individual tables.
#'
#' @param ... Other arguments passed to `src` on first
#'   load. For MySQL and PostgreSQL, the defaults assume you have a local
#'   server with `lahman` database already created.
#'   For `lahman_srcs()`, character vector of names giving srcs to generate.
#' @param quiet if `TRUE`, suppress messages about databases failing to
#'   connect.
#' @param type src type.
#' @keywords internal
#' @examples
#' # Connect to a local sqlite database, if already created
#' \donttest{
#' library(dplyr)
#'
#' if (has_lahman("sqlite")) {
#'   lahman_sqlite()
#'   batting <- tbl(lahman_sqlite(), "Batting")
#'   batting
#' }
#'
#' # Connect to a local postgres database with lahman database, if available
#' if (has_lahman("postgres")) {
#'   lahman_postgres()
#'   batting <- tbl(lahman_postgres(), "Batting")
#' }
#' }
#' @name lahman
NULL

# nocov start
#' @export
#' @rdname lahman
lahman_sqlite <- function(path = NULL) {
  path <- db_location(path, "lahman.sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  copy_lahman(con)
}

#' @export
#' @rdname lahman
lahman_postgres <- function(dbname = "lahman", host = "localhost", ...) {
  con <- DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, host = host, ...)
  copy_lahman(con)
}

#' @export
#' @rdname lahman
lahman_mysql <- function(dbname = "lahman", ...) {
  con <- DBI::dbConnect(RMariaDB::MariaDB(), dbname = dbname, ...)
  copy_lahman(con)
}

#' @rdname lahman
#' @export
copy_lahman <- function(con, ...) {
  # Create missing tables
  tables <- setdiff(lahman_tables(), DBI::dbListTables(con))
  for (table in tables) {
    df <- getExportedValue("Lahman", table)
    message("Creating table: ", table)

    ids <- as.list(names(df)[grepl("ID$", names(df))])
    copy_to(con, df, table, indexes = ids, temporary = FALSE)
  }

  invisible(con)
}
# Get list of all non-label data frames in package
lahman_tables <- function() {
  tables <- utils::data(package = "Lahman")$results[, 3]
  tables[!grepl("Labels", tables)]
}

#' @rdname lahman
#' @export
has_lahman <- function(type, ...) {
  if (!requireNamespace("Lahman", quietly = TRUE)) return(FALSE)
  succeeds(lahman(type, ...), quiet = FALSE)
}

#' @rdname lahman
#' @export
lahman_srcs <- function(..., quiet = NULL) {
  load_srcs(lahman, c(...), quiet = quiet)
}

lahman <- function(type, ...) {
  f <- match.fun(paste0("lahman_", type))
  f(...)
}
# nocov end
