test_that("adbc_vendor_to_dialect maps known vendors", {
  expect_s3_class(adbc_vendor_to_dialect("SQLite"), "sql_dialect_sqlite")
  expect_s3_class(adbc_vendor_to_dialect("PostgreSQL"), "sql_dialect_postgres")
  expect_s3_class(adbc_vendor_to_dialect("MySQL"), "sql_dialect_mysql")
  expect_s3_class(adbc_vendor_to_dialect("TiDB"), "sql_dialect_mysql")
  expect_s3_class(adbc_vendor_to_dialect("MariaDB"), "sql_dialect_mariadb")
})

test_that("adbc_vendor_to_dialect is case-insensitive", {
  expect_s3_class(adbc_vendor_to_dialect("sqlite"), "sql_dialect_sqlite")
  expect_s3_class(adbc_vendor_to_dialect("POSTGRESQL"), "sql_dialect_postgres")
})

test_that("adbc_vendor_to_dialect falls back to ODBC for unknown vendors", {
  expect_s3_class(adbc_vendor_to_dialect("Unknown"), "sql_dialect_odbc")
})

test_that("sql_dialect.AdbiConnection queries the live driver", {
  skip_if_not_installed("adbi")
  skip_if_not_installed("adbcdrivermanager")

  con <- DBI::dbConnect(adbi::adbi("adbcsqlite"), uri = ":memory:")
  on.exit(DBI::dbDisconnect(con))

  expect_s3_class(sql_dialect(con), "sql_dialect_sqlite")
  expect_equal(dbplyr_edition(con), 2L)

  DBI::dbWriteTable(con, "df", data.frame(x = 1:3, y = c("a", "b", "c")))
  out <- tbl(con, "df") |> dplyr::filter(x > 1) |> dplyr::collect()
  expect_equal(out, tibble::tibble(x = 2:3, y = c("b", "c")))
})
