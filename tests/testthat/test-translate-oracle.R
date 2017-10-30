context("translate-Oracle")

test_that("custom scalar functions translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_oracle())
  }

  expect_equal(trans(as.character(x)), sql("CAST(`x` AS VARCHAR(255))"))
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
