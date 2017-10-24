context("translate-Oracle")

# Oracle base_scalar conversions -----------------------------------------

test_that("as.numeric() translated to VARCHAR(255) ", {
  expect_equivalent(
    translate_sql(as.character(field_name), con = simulate_oracle()),
    sql("CAST(`field_name` AS VARCHAR(255))")
  )
})

test_that("as.numeric() translated to NUMBER ", {
  expect_equivalent(
    translate_sql(as.double(field_name), con = simulate_oracle()),
    sql("CAST(`field_name` AS NUMBER)")
  )
})

# Oracle query tests  ------------------------------------------------

df <- data.frame(x = 1, y = 2)
df_oracle <- tbl_lazy(df, src = simulate_oracle())

test_that("query uses FETCH FIRST x ROWS instead of LIMIT ", {
  expect_match(
    sql_render(df_oracle %>% head, simulate_oracle()),
    sql("^SELECT [*] FROM [(]SELECT [*]\nFROM [(]`df`[)] [)] `[^`]*` WHERE ROWNUM [<][=] 6"))
})

test_that(" alias is produced without AS ", {
  expect_match(
    sql_render(
      df_oracle %>%
        group_by(x) %>%
        tally %>%
        ungroup() %>%
        tally,
      simulate_oracle()),
    sql("^SELECT COUNT[(][*][)] AS `nn`\nFROM [(]SELECT `x`, COUNT[(][*][)] AS `n`\nFROM [(]`df`[)] \nGROUP BY `x`[)] `[^`]*`$"))
})
