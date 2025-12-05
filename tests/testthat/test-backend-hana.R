test_that("custom string translations", {
  con <- simulate_hana()

  expect_snapshot(translate_sql(paste0("a", "b"), con = con))
  expect_snapshot(translate_sql(paste("a", "b"), con = con))

  expect_snapshot(translate_sql(substr(x, 2, 4), con = con))
  expect_snapshot(translate_sql(substring(x, 2, 4), con = con))
  expect_snapshot(translate_sql(str_sub(x, 2, -2), con = con))
})

test_that("copy_inline uses UNION ALL", {
  con <- simulate_hana()
  y <- tibble::tibble(id = 1L, arr = "{1,2,3}")

  types <- c(id = "bigint", arr = "integer[]")
  expect_snapshot({
    copy_inline(con, y |> slice(0)) |> remote_query()
    copy_inline(con, y) |> remote_query()

    # with `types`
    copy_inline(con, y |> slice(0), types = types) |> remote_query()
    copy_inline(con, y, types = types) |> remote_query()
  })
})
