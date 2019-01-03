context("translate-sqlite")

test_that("vectorised translations", {
  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_sqlite(), window = FALSE)
  }

  expect_equal(trans(nullif(x, 0L)), sql('NULLIF(`x`, 0)'))
  expect_equal(trans(paste(x, y)), sql("`x` || ' ' || `y`"))
  expect_equal(trans(paste0(x, y)), sql("`x` || `y`"))
})

test_that("as.numeric()/as.double() get custom translation", {
  mf <- dbplyr::memdb_frame(x = 1L)

  out <- mf %>% mutate(x1 = as.numeric(x), x2 = as.double(x)) %>% collect()
  expect_type(out$x1, "double")
  expect_type(out$x2, "double")
})


mf <- lazy_frame(x = 1, src = simulate_sqlite())

test_that("sample_n() return the correct query", {
  expect_equal(
    mf %>% sample_n(10) %>% sql_render(simulate_sqlite()),
    sql("SELECT *\nFROM `df`\nWHERE _rowid_ IN (SELECT _rowid_ FROM (`df`) ORDER BY RANDOM() LIMIT 10.0)")
  )
})

test_that("sample_frac() return the expected error message", {
  expect_error(
    mf %>% sample_frac(0.1) %>% sql_render(simulate_sqlite()),
    "Only number of rows is supported."
  )
})
