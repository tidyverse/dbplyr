on_gha <- function() identical(Sys.getenv("GITHUB_ACTIONS"), "true")
on_cran <- function() !identical(Sys.getenv("NOT_CRAN"), "true")

if (test_srcs$length() == 0) {

  test_register_con("sqlite", RSQLite::SQLite(), ":memory:")

  if (identical(Sys.getenv("GITHUB_POSTGRES"), "true")) {
    test_register_con("postgres", RPostgres::Postgres(),
      dbname = "test",
      user = "postgres",
      password = "password",
      host = "127.0.0.1"
    )
  } else if (identical(Sys.getenv("GITHUB_MSSQL"), "true")) {
    test_register_con("mssql", odbc::odbc(),
      driver = "ODBC Driver 17 for SQL Server",
      database = "test",
      uid = "SA",
      pwd = "Password12",
      server = "localhost",
      port = 1433
    )
  } else if (on_gha() || on_cran()) {
    # Only test with sqlite
  } else {
    test_register_con("MariaDB", RMariaDB::MariaDB(),
      dbname = "test",
      host = "localhost",
      username = Sys.getenv("USER")
    )
    test_register_con("postgres", RPostgres::Postgres(),
      dbname = "test",
      host = "localhost",
      user = ""
    )
  }
}

local_sqlite_con_with_aux <- function(envir = parent.frame()) {
  tmp <- tempfile()

  con <- withr::local_db_connection(
    DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
    .local_envir = envir
  )
  DBI::dbExecute(con, paste0("ATTACH '", tmp, "' AS aux"))

  con
}

snap_transform_dbi <- function(x) {
  x <- gsub("dbplyr_[a-zA-Z0-9]+", "dbplyr_{tmp}", x)

  # use the last line matching this in case of multiple chained errors
  caused_by <- which(x == "Caused by error:")
  if (length(caused_by) == 0) {
    return(x)
  }

  dbi_line_id <- max(caused_by)

  n <- length(x)
  x <- x[-seq2(dbi_line_id + 1, n)]
  c(x, "! dummy DBI error")
}
