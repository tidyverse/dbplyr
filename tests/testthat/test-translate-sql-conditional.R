test_that("case_when converted to CASE WHEN", {
  expect_snapshot(translate_sql(case_when(x > 1L ~ "a")))
})

test_that("even inside mutate", {
  out <- lazy_frame(x = 1:5) %>%
    mutate(y = case_when(x > 1L ~ "a")) %>%
    sql_build()
  expect_snapshot(out$select[[2]])
})

test_that("case_when translates correctly to ELSE when TRUE ~ is used 2", {
  expect_snapshot(
    translate_sql(
      case_when(
        x == 1L ~ "yes",
        x == 0L ~ "no",
        TRUE    ~ "undefined")
    )
  )
})

test_that("case_when uses the .default arg", {
  expect_snapshot(
    translate_sql(
      case_when(
        x == 1L ~ "yes",
        x == 0L ~ "no",
        .default = "undefined"
      )
    )
  )

  expect_snapshot(
    translate_sql(
      case_when(
        x == 1L ~ "yes",
        x == 0L ~ "no",
        .default = x + 1
      )
    )
  )

  # TRUE ~ has precedence over .default
  expect_snapshot(
    translate_sql(
      case_when(
        x == 1L ~ "yes",
        x == 0L ~ "no",
        TRUE ~ "true",
        .default = "undefined"
      )
    )
  )
})

test_that("case_when does not support .ptype and .size", {
  expect_snapshot({
    (expect_error(translate_sql(case_when(x == 1L ~ "yes", .ptype = character()))))
    (expect_error(translate_sql(case_when(x == 1L ~ "yes", .size = 1))))
  })
})

test_that("long case_when is on multiple lines", {
  expect_snapshot(
    translate_sql(
      case_when(
        x == 1L ~ "this is long",
        x == 0L ~ "so it should",
        TRUE    ~ "be wrapped")
    )
  )
})

test_that("all forms of if translated to case statement", {
  expected <- sql("CASE WHEN `x` THEN 1 WHEN NOT `x` THEN 2 END")

  expect_equal(translate_sql(if (x) 1L else 2L), expected)
  expect_equal(translate_sql(ifelse(x, 1L, 2L)), expected)
  expect_equal(translate_sql(if_else(x, 1L, 2L)), expected)
})

test_that("if_else can be simplified", {
  expect_equal(
    translate_sql(if_else(x, 1L, 2L, 2L)),
    sql("CASE WHEN `x` THEN 1 ELSE 2 END")
  )
})

test_that("if translation adds parens", {
  expect_equal(
    translate_sql(if (x) y),
    sql("CASE WHEN `x` THEN `y` END")
  )

  expect_equal(
    translate_sql(if (x > 1L) y + 1L),
    sql("CASE WHEN (`x` > 1) THEN (`y` + 1) END")
  )

  expect_equal(
    translate_sql(if (x) y else z),
    sql("CASE WHEN `x` THEN `y` WHEN NOT `x` THEN `z` END")
  )

  expect_equal(
    translate_sql(if (x > 1L) y + 1L else z + 1L),
    sql("CASE WHEN (`x` > 1) THEN (`y` + 1) WHEN NOT (`x` > 1) THEN (`z` + 1) END")
  )
})

test_that("if and ifelse use correctly named arguments",{
  exp <- translate_sql(if (x) 1 else 2)

  expect_equal(translate_sql(ifelse(test = x, yes = 1, no = 2)), exp)
  expect_equal(translate_sql(if_else(condition = x, true = 1, false = 2)), exp)

  expect_equal(
    translate_sql(if_else(condition = x, true = 1, false = 2, missing = 3)),
    sql("CASE WHEN `x` THEN 1.0 WHEN NOT `x` THEN 2.0 WHEN (`x` IS NULL) THEN 3.0 END")
  )
})

test_that("switch translated to CASE WHEN", {
  expect_equal(
    translate_sql(switch(x, a = 1L)),
    sql("CASE `x` WHEN ('a') THEN (1) END")
  )
  expect_equal(
    translate_sql(switch(x, a = 1L, 2L)),
    sql("CASE `x` WHEN ('a') THEN (1) ELSE (2) END")
  )
})

test_that("is.na and is.null are equivalent", {
  # Needs to be wrapped in parens to ensure correct precedence
  expect_equal(translate_sql(is.na(x + y)), sql("((`x` + `y`) IS NULL)"))
  expect_equal(translate_sql(is.null(x + y)), sql("((`x` + `y`) IS NULL)"))

  expect_equal(translate_sql(x + is.na(x)), sql("`x` + (`x` IS NULL)"))
  expect_equal(translate_sql(!is.na(x)), sql("NOT((`x` IS NULL))"))
})

test_that("magrittr pipe is translated in conditionals", {
  expect_equal(
    translate_sql(x %>% ifelse(1L, 2L)),
    sql("CASE WHEN `x` THEN 1 WHEN NOT `x` THEN 2 END")
  )
})

test_that("conditionals check arguments", {
  expect_snapshot(error = TRUE, translate_sql(case_when()))

  expect_snapshot(error = TRUE, translate_sql(switch(x, 1L, 2L)))
})
