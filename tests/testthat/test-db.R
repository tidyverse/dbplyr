test_that("useful error for 1st edition", {
  con <- structure(list(), class = c("Test", "DBIConnection"))
  expect_snapshot(check_2ed(con), error = TRUE)
})
