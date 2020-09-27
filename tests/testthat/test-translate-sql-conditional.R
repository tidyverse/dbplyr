test_that("case_when converted to CASE WHEN", {
  expect_snapshot(translate_sql(case_when(x > 1L ~ "a")))
})

test_that("even inside mutate", {
  out <- lazy_frame(x = 1:5) %>%
    mutate(y = case_when(x > 1L ~ "a")) %>%
    sql_build()
  expect_snapshot(out$select[[2]])
})

test_that("case_when translates correctly to ELSE when TRUE ~ is used 2", {
  expect_snapshot(
    translate_sql(
      case_when(
        x == 1L ~ "yes",
        x == 0L ~ "no",
        TRUE    ~ "undefined")
    )
  )
})
