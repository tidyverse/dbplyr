df <- tibble::tribble(
  ~ id, ~group, ~letter, ~n1, ~ n2,
     1,      1,      NA,  NA,    1,
     2,      1,     "a",   2,   NA,
     3,      1,      NA,  NA,   NA,
     4,      1,     "a",  NA,    4,
     5,      2,     "a",   5,   NA,
     6,      2,      NA,  NA,    6,
)
df_db <- copy_to(src_memdb(), df, name = "df", overwrite = TRUE)

df <- tibble::tribble(
  ~ id, ~group, ~n1,
     1,      1,  NA,
     2,      1,   2,
     3,      1,  NA,
     4,      1,  NA,
     5,      2,   5,
     6,      2,  NA,
)
df_lazy_ns <- tbl_lazy(df, con = simulate_sqlite())
df_lazy_std <- tbl_lazy(df)

test_that("fill works", {
  expect_equal(
    df_db %>%
      window_order(id) %>%
      group_by(group) %>%
      tidyr::fill(n1, n2) %>%
      collect(),
    tibble::tribble(
      ~ id, ~group, ~letter, ~n1, ~ n2,
         1,      1,      NA,  NA,    1,
         2,      1,     "a",   2,    1,
         3,      1,      NA,   2,    1,
         4,      1,     "a",   2,    4,
         5,      2,     "a",   5,   NA,
         6,      2,      NA,   5,    6,
    ) %>%
      group_by(group)
  )
})

test_that("up-direction works", {
  expect_snapshot(
    df_lazy_ns %>%
      window_order(id) %>%
      tidyr::fill(n1, .direction = "up")
  )
  expect_snapshot(
    df_lazy_std %>%
      window_order(id) %>%
      tidyr::fill(n1, .direction = "up")
  )
})

test_that("up-direction works", {
  expect_snapshot(
    df_lazy_std %>%
      window_order(id) %>%
      tidyr::fill(n1, .direction = "updown")
  )
  expect_snapshot(
    df_lazy_std %>%
      window_order(id) %>%
      tidyr::fill(n1, .direction = "downup")
  )
})

test_that("up-direction works with descending", {
  expect_snapshot(
    df_lazy_ns %>%
      window_order(desc(id)) %>%
      tidyr::fill(n1, .direction = "up")
  )
  expect_snapshot(
    df_lazy_std %>%
      window_order(desc(id)) %>%
      tidyr::fill(n1, .direction = "up")
  )
})

test_that("groups are respected", {
  expect_snapshot(
    group_by(df_lazy_ns, group) %>%
      window_order(id) %>%
      tidyr::fill(n1)
  )

  expect_snapshot(
    group_by(df_lazy_std, group) %>%
      window_order(id) %>%
      tidyr::fill(n1)
  )
})

test_that("fill errors on unsorted data", {
  expect_snapshot({
    (expect_error(df_db %>% tidyr::fill(n1)))
  })
})

test_that("fill() produces nice error messages", {
  expect_snapshot(error = TRUE, {
    lazy_frame(x = 1) %>% tidyr::fill(non_existent)
  })
})
