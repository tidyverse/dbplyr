context("translate-Oracle")

test_that("custom scalar functions translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_oracle())
  }

  expect_equal(trans(as.character(x)), sql("CAST(`x` AS VARCHAR2(255))"))
  expect_equal(trans(as.integer64(x)), sql("CAST(`x` AS NUMBER(19))"))
  expect_equal(trans(as.double(x)),    sql("CAST(`x` AS NUMBER)"))

})

test_that("queries translate correctly", {
  mf <- lazy_frame(x = 1, src = simulate_oracle())

  expect_match(
    mf %>% head() %>% sql_render(simulate_oracle()),
    sql("^SELECT [*] FROM [(]SELECT [*]\nFROM [(]`df`[)] [)] `[^`]*` WHERE ROWNUM [<][=] 6")
  )

  expect_match(
    mf %>%
      group_by(x) %>%
      tally %>%
      ungroup() %>%
      tally() %>%
      sql_render(simulate_oracle()),
    sql("^SELECT COUNT[(][*][)] AS `nn`\nFROM [(]SELECT `x`, COUNT[(][*][)] AS `n`\nFROM [(]`df`[)] \nGROUP BY `x`[)] `[^`]*`$")
  )

})

mf <- lazy_frame(x = 1, src = simulate_oracle())

test_that("sample_frac() returns the correct query", {
  expect_equal(
    mf %>% sample_frac(0.1) %>% show_query(),
    sql("SELECT *\nFROM (`df`) \nSAMPLE (10)")
  )
})

test_that("sample_n() returns the expected error message", {
  expect_error(
    mf %>% sample_n(10) %>% show_query(),
    "Only sample fractions are supported"
  )
})
