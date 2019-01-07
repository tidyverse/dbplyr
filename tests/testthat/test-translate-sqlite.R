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
