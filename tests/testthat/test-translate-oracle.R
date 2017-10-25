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

test_that("query uses IS NULL to filter is.na() ", {
  expect_match(
    sql_render(df_oracle %>% filter(is.na(x)), simulate_oracle()),
    sql("^SELECT [*]\nFROM [(]`df`[)] \nWHERE [(][(][(]`x`[)] IS NULL[)][)]"))
})

test_that("query uses CASE WHEN to mutate is.na() ", {
  expect_match(
    sql_render(df_oracle %>% mutate(is.na(x)), simulate_oracle()),
    sql("SELECT `x`, `y`, CASE WHEN`x` IS NULL THEN 1 ELSE 0 END  AS `is.na[(]x[)]`\nFROM [(]`df`[)] "))
})



