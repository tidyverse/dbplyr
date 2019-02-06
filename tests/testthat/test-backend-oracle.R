context("test-backend-oracle.R")

test_that("custom scalar functions translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_oracle())
  }

  expect_equal(trans(as.character(x)), sql("CAST(`x` AS VARCHAR2(255))"))
  expect_equal(trans(as.integer64(x)), sql("CAST(`x` AS NUMBER(19))"))
  expect_equal(trans(as.double(x)),    sql("CAST(`x` AS NUMBER)"))

})

test_that("queries translate correctly", {
  mf <- lazy_frame(x = 1, con = simulate_oracle())

  expect_match(
    mf %>% head() %>% sql_render(simulate_oracle()),
    sql("^SELECT [*] FROM [(]SELECT [*]\nFROM [(]`df`[)] [)] `[^`]*` WHERE ROWNUM [<][=] 6")
  )
})

test_that("paste and paste0 translate correctly", {
  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_oracle(), window = FALSE)
  }

  expect_equal(trans(paste(x, y)), sql("`x` || ' ' || `y`"))
  expect_equal(trans(paste0(x, y)), sql("`x` || `y`"))
})
