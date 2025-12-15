sql_lines <- function(...) {
  sql(paste0(c(...), collapse = "\n"))
}

test_that("can translate cut", {
  con <- simulate_dbi()
  expect_translation(
    con,
    cut(x, 1:2),
    c(
      "CASE",
      "WHEN (`x` <= 1.0) THEN NULL",
      "WHEN (`x` <= 2.0) THEN '(1,2]'",
      "WHEN (`x` > 2.0) THEN NULL",
      "END"
    )
  )

  expect_translation(
    con,
    cut(x, 1:3),
    c(
      "CASE",
      "WHEN (`x` <= 1.0) THEN NULL",
      "WHEN (`x` <= 2.0) THEN '(1,2]'",
      "WHEN (`x` <= 3.0) THEN '(2,3]'",
      "WHEN (`x` > 3.0) THEN NULL",
      "END"
    )
  )
})

test_that("works with include.lowest = TRUE", {
  con <- simulate_dbi()
  expect_translation(
    con,
    cut(x, 1:3, include.lowest = TRUE),
    c(
      "CASE",
      "WHEN (`x` < 1.0) THEN NULL",
      "WHEN (`x` <= 2.0) THEN '[1,2]'",
      "WHEN (`x` <= 3.0) THEN '(2,3]'",
      "WHEN (`x` > 3.0) THEN NULL",
      "END"
    )
  )
})

test_that("works with right = FALSE", {
  con <- simulate_dbi()
  expect_translation(
    con,
    cut(x, 1:2, right = FALSE),
    c(
      "CASE",
      "WHEN (`x` < 1.0) THEN NULL",
      "WHEN (`x` < 2.0) THEN '[1,2)'",
      "WHEN (`x` >= 2.0) THEN NULL",
      "END"
    )
  )
})

test_that("works with right = FALSE and include.lowest = TRUE", {
  con <- simulate_dbi()
  expect_translation(
    con,
    cut(x, 1:2, right = FALSE, include.lowest = TRUE),
    c(
      "CASE",
      "WHEN (`x` < 1.0) THEN NULL",
      "WHEN (`x` <= 2.0) THEN '[1,2]'",
      "WHEN (`x` > 2.0) THEN NULL",
      "END"
    )
  )
})

test_that("works with labels = FALSE", {
  con <- simulate_dbi()
  expect_translation(
    con,
    cut(x, 1:3, labels = FALSE),
    c(
      "CASE",
      "WHEN (`x` <= 1.0) THEN NULL",
      "WHEN (`x` <= 2.0) THEN '1'",
      "WHEN (`x` <= 3.0) THEN '2'",
      "WHEN (`x` > 3.0) THEN NULL",
      "END"
    )
  )
})

test_that("works with labels a character vector", {
  con <- simulate_dbi()
  expect_translation(
    con,
    cut(x, 1:3, labels = c("a", "b")),
    c(
      "CASE",
      "WHEN (`x` <= 1.0) THEN NULL",
      "WHEN (`x` <= 2.0) THEN 'a'",
      "WHEN (`x` <= 3.0) THEN 'b'",
      "WHEN (`x` > 3.0) THEN NULL",
      "END"
    )
  )

  expect_translation_snapshot(
    con,
    cut(x, 1:3, labels = c("a", "b", "c")),
    error = TRUE
  )
})

test_that("can handle infinity", {
  con <- simulate_dbi()
  expect_translation(
    con,
    cut(x, c(-Inf, 0, 1, Inf)),
    c(
      "CASE",
      "WHEN (`x` <= 0.0) THEN '(-Inf,0]'",
      "WHEN (`x` <= 1.0) THEN '(0,1]'",
      "WHEN (`x` > 1.0) THEN '(1,Inf]'",
      "END"
    )
  )

  expect_translation(
    con,
    cut(x, c(-Inf, 0, 1, Inf), right = FALSE),
    c(
      "CASE",
      "WHEN (`x` < 0.0) THEN '[-Inf,0)'",
      "WHEN (`x` < 1.0) THEN '[0,1)'",
      "WHEN (`x` >= 1.0) THEN '[1,Inf)'",
      "END"
    )
  )

  expect_translation(
    con,
    cut(x, c(-Inf, 0, 1, Inf), include.lowest = TRUE),
    c(
      "CASE",
      "WHEN (`x` <= 0.0) THEN '[-Inf,0]'",
      "WHEN (`x` <= 1.0) THEN '(0,1]'",
      "WHEN (`x` > 1.0) THEN '(1,Inf]'",
      "END"
    )
  )
})

test_that("cut checks arguments", {
  con <- simulate_dbi()
  expect_translation_snapshot(con, cut(x, 1), error = TRUE)
  expect_translation_snapshot(con, cut(x, c(1, 1)), error = TRUE)
  expect_translation_snapshot(con, cut(x, c(1, 2, NA)), error = TRUE)
})
