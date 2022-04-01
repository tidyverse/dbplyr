sql_lines <- function(...) {
  sql(paste0(c(...), collapse = "\n"))
}

test_that("can translate cut", {
  expect_equal(
    translate_sql(cut(x, 1:2)),
    sql_lines(
      "CASE",
      "WHEN (`x` <= 1.0) THEN NULL",
      "WHEN (`x` > 1.0 AND `x` <= 2.0) THEN '(1,2]'",
      "WHEN (`x` > 2.0) THEN NULL",
      "END"
    )
  )

  expect_equal(
    translate_sql(cut(x, 1:3)),
    sql_lines(
      "CASE",
      "WHEN (`x` <= 1.0) THEN NULL",
      "WHEN (`x` > 1.0 AND `x` <= 2.0) THEN '(1,2]'",
      "WHEN (`x` > 2.0 AND `x` <= 3.0) THEN '(2,3]'",
      "WHEN (`x` > 3.0) THEN NULL",
      "END"
    )
  )
})

test_that("works with right = FALSE", {
  expect_equal(
    translate_sql(cut(x, 1:2, right = FALSE)),
    sql_lines(
      "CASE",
      "WHEN (`x` < 1.0) THEN NULL",
      "WHEN (`x` >= 1.0 AND `x` < 2.0) THEN '[1,2)'",
      "WHEN (`x` >= 2.0) THEN NULL",
      "END"
    )
  )
})

test_that("works with labels = FALSE", {
  expect_equal(
    translate_sql(cut(x, 1:3, labels = FALSE)),
    sql_lines(
      "CASE",
      "WHEN (`x` <= 1.0) THEN NULL",
      "WHEN (`x` > 1.0 AND `x` <= 2.0) THEN '1'",
      "WHEN (`x` > 2.0 AND `x` <= 3.0) THEN '2'",
      "WHEN (`x` > 3.0) THEN NULL",
      "END"
    )
  )
})

test_that("works with labels a character vector", {
  expect_equal(
    translate_sql(cut(x, 1:3, labels = c("a", "b"))),
    sql_lines(
      "CASE",
      "WHEN (`x` <= 1.0) THEN NULL",
      "WHEN (`x` > 1.0 AND `x` <= 2.0) THEN 'a'",
      "WHEN (`x` > 2.0 AND `x` <= 3.0) THEN 'b'",
      "WHEN (`x` > 3.0) THEN NULL",
      "END"
    )
  )

  expect_snapshot(
    (expect_error(translate_sql(cut(x, 1:3, labels = c("a", "b", "c")))))
  )
})

test_that("can handle infinity", {
  expect_equal(
    translate_sql(cut(x, c(-Inf, 0, 1, Inf))),
    sql_lines(
      "CASE",
      "WHEN (`x` <= 0.0) THEN '(-Inf,0]'",
      "WHEN (`x` > 0.0 AND `x` <= 1.0) THEN '(0,1]'",
      "WHEN (`x` > 1.0) THEN '(1,Inf]'",
      "END"
    )
  )
})

test_that("cut checks arguments", {
  expect_snapshot({
    (expect_error(translate_sql(cut(x, 1))))
    (expect_error(translate_sql(cut(x, c(1, 1)))))
  })
})
