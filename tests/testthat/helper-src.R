on_gha <- function() identical(Sys.getenv("GITHUB_ACTIONS"), "true")
on_cran <- function() !identical(Sys.getenv("NOT_CRAN"), "true")

if (test_srcs$length() == 0) {

  # test_register_src("df", dplyr::src_df(env = new.env(parent = emptyenv())))
  test_register_con("sqlite", RSQLite::SQLite(), ":memory:")

  if (identical(Sys.getenv("GITHUB_POSTGRES"), "true")) {
    test_register_con("postgres", RPostgres::Postgres(),
      dbname = "test",
      user = "postgres",
      password = "password"
    )
  } else if (identical(Sys.getenv("GITHUB_MSSQL"), "true")) {
    if (packageVersion("odbc") >= "1.2.2.9000") {
      test_register_con("mssql", odbc::odbc(),
        dsn = "mssql-test-ms",
        UID = "kirill",
        PWD = "Password12"
      )
    }
  } else if (on_gha() || on_cran()) {
    # Only test with sqlite
  } else  {
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


skip_if_no_db <- function(db) {
  if (!test_srcs$has(db))
    skip(paste0("No ", db))
}

sqlite_con_with_aux <- function() {
  tmp <- tempfile()

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, paste0("ATTACH '", tmp, "' AS aux"))

  con
}
