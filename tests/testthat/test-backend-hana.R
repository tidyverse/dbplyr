test_that("custom string translations", {
  local_con(simulate_hana())

  expect_snapshot(test_translate_sql(paste0("a", "b")))
  expect_snapshot(test_translate_sql(paste("a", "b")))

  expect_snapshot(test_translate_sql(substr(x, 2, 4)))
  expect_snapshot(test_translate_sql(substring(x, 2, 4)))
  expect_snapshot(test_translate_sql(str_sub(x, 2, -2)))
})

test_that("copy_inline uses UNION ALL", {
  con <- simulate_hana()
  y <- tibble::tibble(id = 1L, arr = "{1,2,3}")

  types <- c(id = "bigint", arr = "integer[]")
  expect_snapshot({
    copy_inline(con, y %>% slice(0)) %>% remote_query()
    copy_inline(con, y) %>% remote_query()

    # with `types`
    copy_inline(con, y %>% slice(0), types = types) %>% remote_query()
    copy_inline(con, y, types = types) %>% remote_query()
  })
})
