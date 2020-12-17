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
      group_by(group) %>%
      dbplyr_fill(
        n1,
        n2,
        order_by = c(id)
      ) %>%
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
      group_by(group) %>%
      select(group, everything())
  )
})

test_that("up-direction works", {
  expect_snapshot(dbplyr_fill(df_lazy_ns, n1, order_by = id, .direction = "up"))
  expect_snapshot(dbplyr_fill(df_lazy_std, n1, order_by = id, .direction = "up"))
})

test_that("up-direction works with descending", {
  expect_snapshot(dbplyr_fill(df_lazy_ns, n1, order_by = desc(id), .direction = "up"))
  expect_snapshot(dbplyr_fill(df_lazy_std, n1, order_by = desc(id), .direction = "up"))
})

test_that("groups are respected", {
  expect_snapshot(
    dbplyr_fill(
      group_by(df_lazy_ns, group),
      n1,
      order_by = id
    )
  )

  expect_snapshot(
    dbplyr_fill(
      group_by(df_lazy_std, group),
      n1,
      order_by = id
    )
  )
})
