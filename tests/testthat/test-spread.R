context("spread")

mf <- memdb_frame(
  h = c("a", "b", "c", "d"),
  x = c("A", "A", "B", "B"),
  y = c("X", "Y", "X", "Y"),
  z = c( 1,   2,   3,   4 )
  )

df <- tibble(
  h = c("a", "b", "c", "d"),
  x = c("A", "A", "B", "B"),
  y = c("X", "Y", "X", "Y"),
  z = c( 1,   2,   3,   4 )
)

test_that("tibble and tbl_sql spread methods return the same results",{
  expect_equal(
    tidyr::spread(mf, x, z) %>% collect(),
    tidyr::spread(df, x, z)
  )
})
