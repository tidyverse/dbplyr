test_that("symbols weights are dropped in output", {
  df <- copy_to_test("sqlite", tibble(x = 1, w = 1))
  expect_equal(dbplyr_uncount(df, w) |> collect(), tibble(x = 1))

  expect_snapshot(
    dbplyr_uncount(df, w) |> show_query(),
    transform = function(lines) {
      lines_to_transform <- grepl("INNER JOIN", lines)
      lines[lines_to_transform] <- gsub(
        "`dbplyr_\\d+`",
        "`dbplyr_table`",
        lines[lines_to_transform]
      )

      lines
    }
  )

  expect_equal(
    df |> mutate(w = w + 1) |> dbplyr_uncount(w) |> collect(),
    tibble(x = c(1L, 1L))
  )

  expect_snapshot(
    df |>
      mutate(w = w + 1) |>
      dbplyr_uncount(w) |>
      show_query()
  )
})

test_that("can request to preserve symbols", {
  df <- memdb_frame(x = 1, w = 1)

  expect_equal(
    dbplyr_uncount(df, w, .remove = FALSE) |> colnames(),
    c("x", "w")
  )
})

test_that("unique identifiers created on request", {
  df <- memdb_frame(w = 1:3)
  expect_equal(
    dbplyr_uncount(df, w, .id = "id") |> collect() |> arrange(id),
    tibble(id = c(1L, 1:2, 1:3)) |> arrange(id)
  )
})

test_that("expands constants and expressions", {
  df <- memdb_frame(x = 1, w = 2)

  expect_equal(dbplyr_uncount(df, 2) |> collect(), collect(df)[c(1, 1), ])
  expect_equal(dbplyr_uncount(df, 1 + 1) |> collect(), collect(df)[c(1, 1), ])
})


test_that("works with groups", {
  df <- memdb_frame(g = 1, x = 1, w = 1) |> dplyr::group_by(g)
  expect_equal(group_vars(dbplyr_uncount(df, w)), "g")
})

test_that("grouping variable are removed", {
  df <- memdb_frame(g = 1, x = 1, w = 1) |> dplyr::group_by(g)

  expect_equal(dbplyr_uncount(df, g) |> colnames(), c("x", "w"))
})

test_that("must evaluate to integer", {
  df <- memdb_frame(x = 1, w = 1 / 2)
  expect_error(dbplyr_uncount(df, w), class = "vctrs_error_cast_lossy")

  expect_error(dbplyr_uncount(df, "W"), class = "vctrs_error_incompatible_type")
})

test_that("works with 0 weights", {
  df <- memdb_frame(x = 1:2, w = c(0, 1))
  expect_equal(dbplyr_uncount(df, w) |> collect(), tibble(x = 2))
})
