sql_lines <- function(...) {
  sql(paste0(c(...), collapse = "\n"))
}

test_that("can translate cut", {
  local_con(simulate_dbi())
  expect_equal(
    test_translate_sql(cut(x, 1:2)),
    sql_lines(
      "CASE",
      "WHEN (`x` <= 1.0) THEN NULL",
      "WHEN (`x` <= 2.0) THEN '(1,2]'",
      "WHEN (`x` > 2.0) THEN NULL",
      "END"
    )
  )

  expect_equal(
    test_translate_sql(cut(x, 1:3)),
    sql_lines(
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
  local_con(simulate_dbi())
  expect_equal(
    test_translate_sql(cut(x, 1:3, include.lowest = TRUE)),
    sql_lines(
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
  local_con(simulate_dbi())
  expect_equal(
    test_translate_sql(cut(x, 1:2, right = FALSE)),
    sql_lines(
      "CASE",
      "WHEN (`x` < 1.0) THEN NULL",
      "WHEN (`x` < 2.0) THEN '[1,2)'",
      "WHEN (`x` >= 2.0) THEN NULL",
      "END"
    )
  )
})

test_that("works with right = FALSE and include.lowest = TRUE", {
  local_con(simulate_dbi())
  expect_equal(
    test_translate_sql(cut(x, 1:2, right = FALSE, include.lowest = TRUE)),
    sql_lines(
      "CASE",
      "WHEN (`x` < 1.0) THEN NULL",
      "WHEN (`x` <= 2.0) THEN '[1,2]'",
      "WHEN (`x` > 2.0) THEN NULL",
      "END"
    )
  )
})

test_that("works with labels = FALSE", {
  local_con(simulate_dbi())
  expect_equal(
    test_translate_sql(cut(x, 1:3, labels = FALSE)),
    sql_lines(
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
  local_con(simulate_dbi())
  expect_equal(
    test_translate_sql(cut(x, 1:3, labels = c("a", "b"))),
    sql_lines(
      "CASE",
      "WHEN (`x` <= 1.0) THEN NULL",
      "WHEN (`x` <= 2.0) THEN 'a'",
      "WHEN (`x` <= 3.0) THEN 'b'",
      "WHEN (`x` > 3.0) THEN NULL",
      "END"
    )
  )

  expect_snapshot(
    (expect_error(test_translate_sql(cut(x, 1:3, labels = c("a", "b", "c")))))
  )
})

test_that("can handle infinity", {
  local_con(simulate_dbi())
  expect_equal(
    test_translate_sql(cut(x, c(-Inf, 0, 1, Inf))),
    sql_lines(
      "CASE",
      "WHEN (`x` <= 0.0) THEN '(-Inf,0]'",
      "WHEN (`x` <= 1.0) THEN '(0,1]'",
      "WHEN (`x` > 1.0) THEN '(1,Inf]'",
      "END"
    )
  )

  expect_equal(
    test_translate_sql(cut(x, c(-Inf, 0, 1, Inf), right = FALSE)),
    sql_lines(
      "CASE",
      "WHEN (`x` < 0.0) THEN '[-Inf,0)'",
      "WHEN (`x` < 1.0) THEN '[0,1)'",
      "WHEN (`x` >= 1.0) THEN '[1,Inf)'",
      "END"
    )
  )

  expect_equal(
    test_translate_sql(cut(x, c(-Inf, 0, 1, Inf), include.lowest = TRUE)),
    sql_lines(
      "CASE",
      "WHEN (`x` <= 0.0) THEN '[-Inf,0]'",
      "WHEN (`x` <= 1.0) THEN '(0,1]'",
      "WHEN (`x` > 1.0) THEN '(1,Inf]'",
      "END"
    )
  )
})

test_that("cut checks arguments", {
  local_con(simulate_dbi())
  expect_snapshot({
    (expect_error(test_translate_sql(cut(x, 1))))
    (expect_error(test_translate_sql(cut(x, c(1, 1)))))
    (expect_error(test_translate_sql(cut(x, c(1, 2, NA)))))
  })
})
