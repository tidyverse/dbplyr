test_that("fill returns same results as tidyr", {
  df <- tibble::tribble(
    ~id , ~group , ~letter , ~n1 , ~n2 ,
      1 ,      1 , NA      , NA  ,   1 ,
      2 ,      1 , "a"     ,   2 , NA  ,
      3 ,      1 , NA      , NA  , NA  ,
      4 ,      1 , "a"     , NA  ,   4 ,
      5 ,      2 , "a"     ,   5 , NA  ,
      6 ,      2 , NA      , NA  ,   6 ,
  )
  df_db <- copy_to(memdb(), df, overwrite = TRUE)
  withr::defer(DBI::dbRemoveTable(memdb(), "df"))

  expect_equal(
    df_db |>
      window_order(id) |>
      group_by(group) |>
      tidyr::fill(n1, n2) |>
      collect(),
    df |> group_by(group) |> tidyr::fill(n1, n2)
  )
})

test_that("generates valid sql for all directions", {
  lf <- lazy_frame(id = 1, group = 1, n1 = 1)

  lf_asc <- lf |> window_order(id)
  lf_desc <- lf |> window_order(desc(id))

  expect_snapshot({
    lf_asc |> tidyr::fill(n1, .direction = "up")
    lf_desc |> tidyr::fill(n1, .direction = "up")

    lf_asc |> tidyr::fill(n1, .direction = "updown")
    lf_asc |> tidyr::fill(n1, .direction = "downup")
  })
})

test_that("fill() respects grouping", {
  lf <- lazy_frame(id = 1, group = 1, n1 = 1)
  expect_snapshot(
    lf |> group_by(group) |> window_order(id) |> tidyr::fill(n1)
  )
})

test_that("can generate variant SQL", {
  lf <- lazy_frame(id = 1, group = 1, n1 = 1, con = simulate_sqlite())

  expect_snapshot({
    lf |> window_order(id) |> tidyr::fill(n1)
    lf |> window_order(desc(id)) |> tidyr::fill(n1)
    lf |> group_by(group) |> window_order(id) |> tidyr::fill(n1)
  })
})

test_that("fill errors on unsorted data", {
  df <- local_memdb_frame(x = 1)

  expect_snapshot(tidyr::fill(df), error = TRUE)
})

test_that("fill() errors on attempted rename", {
  lf <- lazy_frame(x = 1)
  expect_snapshot(tidyr::fill(lf, y = x), error = TRUE)
})

test_that("fill() produces nice error messages", {
  lf <- lazy_frame(x = 1)
  expect_snapshot(tidyr::fill(lf, non_existent), error = TRUE)
})
