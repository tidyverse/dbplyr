context("compute")

test_that("compute doesn't change representation", {
  mf1 <- memdb_frame(x = 5:1, y = 1:5, z = "a")
  expect_equal_tbl(mf1, mf1 %>% compute)
  expect_equal_tbl(mf1, mf1 %>% compute %>% compute)

  mf2 <- mf1 %>% mutate(z = x + y)
  expect_equal_tbl(mf2, mf2 %>% compute)
})

test_that("compute can create indexes", {
  mfs <- test_frame(x = 5:1, y = 1:5, z = 10)

  mfs %>%
    map(. %>% compute(indexes = c("x", "y"))) %>%
    expect_equal_tbls()

  mfs %>%
    map(. %>% compute(indexes = list("x", "y", c("x", "y")))) %>%
    expect_equal_tbls()

  mfs %>%
    map(. %>% compute(indexes = "x", unique_indexes = "y")) %>%
    expect_equal_tbls()

  mfs %>%
    map(. %>% compute(unique_indexes = list(c("x", "z"), c("y", "z")))) %>%
    expect_equal_tbls()
})

test_that("unique index fails if values are duplicated", {
  mfs <- test_frame(x = 5:1, y = "a", ignore = "df")
  map(mfs, function(.) expect_error(compute(., unique_indexes = "y")))
})


# copy_to -----------------------------------------------------------------

test_that("only overwrite existing table if explicitly requested", {
  con <- DBI::dbConnect(RSQLite::SQLite())
  DBI::dbWriteTable(con, "df", data.frame(x = 1:5))

  expect_error(copy_to(con, data.frame(x = 1), name = "df"), "exists")
  expect_silent(copy_to(con, data.frame(x = 1), name = "df", overwrite = TRUE))
})
