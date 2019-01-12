context("test-backend-sqlite.R")

test_that("logicals translated to integers", {
  expect_equal(escape(FALSE, con = simulate_sqlite()), sql("0"))
  expect_equal(escape(TRUE, con = simulate_sqlite()), sql("1"))
  expect_equal(escape(NA, con = simulate_sqlite()), sql("NULL"))
})

test_that("vectorised translations", {
  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_sqlite(), window = FALSE)
  }

  expect_equal(trans(paste(x, y)), sql("`x` || ' ' || `y`"))
  expect_equal(trans(paste0(x, y)), sql("`x` || `y`"))
})

test_that("pmin and max become MIN and MAX", {
  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_sqlite(), window = FALSE)
  }

  expect_equal(trans(pmin(x, y)), sql('MIN(`x`, `y`)'))
  expect_equal(trans(pmax(x, y)), sql('MAX(`x`, `y`)'))
})

test_that("as.numeric()/as.double() get custom translation", {
  mf <- dbplyr::memdb_frame(x = 1L)

  out <- mf %>% mutate(x1 = as.numeric(x), x2 = as.double(x)) %>% collect()
  expect_type(out$x1, "double")
  expect_type(out$x2, "double")
})

test_that("sqlite mimics two argument log", {
  translate_sqlite <- function(...) {
    translate_sql(..., con = src_memdb()$con)
  }

  expect_equal(translate_sqlite(log(x)), sql('LOG(`x`)'))
  expect_equal(translate_sqlite(log(x, 10)), sql('LOG(`x`) / LOG(10.0)'))
})

