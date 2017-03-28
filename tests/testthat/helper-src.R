if (test_srcs$length() == 0) {

  con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  con_mysql <- DBI::dbConnect(
    RMySQL::MySQL(), "test", host = "localhost", user = Sys.getenv("USER")
  )

  if (identical(Sys.getenv("TRAVIS"), "true")) {
    con_postgres <- DBI::dbConnect(
      RPostgreSQL::PostgreSQL(), "test", user = "travis", password = ""
    )
  } else {
    con_postgres <- DBI::dbConnect(
      RPostgreSQL::PostgreSQL(), "test", host = "localhost", user = ""
    )
  }

  test_register_src("df", dplyr::src_df(env = new.env(parent = emptyenv())))
  test_register_src("sqlite", src_dbi(con_sqlite))
  test_register_src("mysql", src_dbi(con_mysql))
  test_register_src("postgres", src_dbi(con_postgres))
}

skip_if_no_sqlite <- function() {
  if (!test_srcs$has("sqlite"))
    skip("No SQLite")
}
