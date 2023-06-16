#' Database versions of the nycflights13 data
#'
#' These functions cache the data from the `nycflights13` database in
#' a local database, for use in examples and vignettes. Indexes are created
#' to making joining tables on natural keys efficient.
#'
#' @keywords internal
#' @name nycflights13
NULL

# nocov start
#' @export
#' @rdname nycflights13
#' @param path location of SQLite database file
nycflights13_sqlite <- function(path = NULL) {
  cache_computation("nycflights_sqlite", {
    path <- db_location(path, "nycflights13.sqlite")
    message("Caching nycflights db at ", path)
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path, create = TRUE)
    copy_nycflights13(con)
  })
}

#' @export
#' @rdname nycflights13
#' @param dbname,... Arguments passed on to [src_postgres()]
nycflights13_postgres <- function(dbname = "nycflights13", ...) {
  cache_computation("nycflights_postgres", {
    message("Caching nycflights db in postgresql db ", dbname)
    con <- DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, ...)
    copy_nycflights13(con)
  })
}

#' @rdname nycflights13
#' @export
has_nycflights13 <- function(type = c("sqlite", "postgres"), ...) {
  if (!requireNamespace("nycflights13", quietly = TRUE)) return(FALSE)

  type <- match.arg(type)

  succeeds(switch(
    type,
    sqlite = nycflights13_sqlite(...),
    postgres = nycflights13_postgres(...)
  ), quiet = TRUE)
}


#' @export
#' @rdname nycflights13
copy_nycflights13 <- function(con, ...) {
  all <- utils::data(package = "nycflights13")$results[, 3]
  unique_index <- list(
    airlines = list("carrier"),
    planes =   list("tailnum")
  )
  index <- list(
    airports = list("faa"),
    flights =  list(c("year", "month", "day"), "carrier", "tailnum", "origin", "dest"),
    weather =  list(c("year", "month", "day"), "origin")
  )

  tables <- setdiff(all, DBI::dbListTables(con))

  # Create missing tables
  for (table in tables) {
    df <- getExportedValue("nycflights13", table)
    message("Creating table: ", table)

    copy_to(
      con, df, table,
      unique_indexes = unique_index[[table]],
      indexes = index[[table]],
      temporary = FALSE
    )
  }
  invisible(con)
}
# nocov end
