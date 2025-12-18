on_gha <- function() identical(Sys.getenv("GITHUB_ACTIONS"), "true")
on_cran <- function() !identical(Sys.getenv("NOT_CRAN"), "true")
on_github_postgres <- function() {
  identical(Sys.getenv("GITHUB_POSTGRES"), "true")
}
on_github_mssql <- function() identical(Sys.getenv("GITHUB_MSSQL"), "true")

cache_test_con <- function(name, get_args) {
  cache <- cache()
  cache_name <- paste0("test_con_", name)

  if (env_has(cache, cache_name)) {
    con <- env_get(cache, cache_name)
    if (is.null(con)) {
      testthat::skip(paste0("No ", name))
    }
    return(con)
  }

  args <- get_args()
  if (is.null(args)) {
    env_poke(cache, cache_name, NULL)
    testthat::skip(paste0("No ", name))
  }

  tryCatch(
    {
      con <- exec(DBI::dbConnect, !!!args)
      env_poke(cache, cache_name, con)
      con
    },
    error = function(e) {
      env_poke(cache, cache_name, NULL)
      testthat::skip(paste0(
        "Can't connect to ",
        name,
        ": ",
        conditionMessage(e)
      ))
    }
  )
}

test_sqlite <- function() {
  cache_test_con("sqlite", function() {
    list(drv = RSQLite::SQLite(), dbname = ":memory:")
  })
}

test_postgres <- function() {
  cache_test_con("postgres", function() {
    if (on_github_postgres()) {
      list(
        drv = RPostgres::Postgres(),
        dbname = "test",
        user = "postgres",
        password = "password",
        host = "127.0.0.1"
      )
    } else if (on_gha() || on_cran()) {
      NULL
    } else {
      list(
        drv = RPostgres::Postgres(),
        dbname = "test",
        host = "localhost",
        user = ""
      )
    }
  })
}

test_mariadb <- function() {
  cache_test_con("mariadb", function() {
    if (on_gha() || on_cran()) {
      NULL
    } else {
      list(
        drv = RMariaDB::MariaDB(),
        dbname = "test",
        host = "localhost",
        username = Sys.getenv("USER")
      )
    }
  })
}

test_mssql <- function() {
  cache_test_con("mssql", function() {
    if (on_github_mssql()) {
      list(
        drv = odbc::odbc(),
        driver = "ODBC Driver 17 for SQL Server",
        database = "test",
        uid = "SA",
        pwd = "Password12",
        server = "localhost",
        port = 1433
      )
    } else {
      NULL
    }
  })
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
  x <- gsub("dbplyr_tmp_[a-zA-Z0-9]+", "dbplyr_{tmp}", x)

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
