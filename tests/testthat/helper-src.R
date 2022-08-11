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

sqlite_con_with_aux <- function() {
  tmp <- tempfile()

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, paste0("ATTACH '", tmp, "' AS aux"))

  con
}

snap_transform_dbi <- function(x) {
  # use the last line matching this in case of multiple chained errors
  dbi_line_id <- max(which(x == "Caused by error:"))
  n <- length(x)
  x <- x[-seq2(dbi_line_id + 1, n)]
  c(x, "! dummy DBI error")
}
