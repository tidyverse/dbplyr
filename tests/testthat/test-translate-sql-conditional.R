test_that("case_when converted to CASE WHEN", {
  local_con(simulate_dbi())
  expect_snapshot(test_translate_sql(case_when(x > 1L ~ "a")))
})

test_that("even inside mutate", {
  out <- lazy_frame(x = 1:5) |>
    mutate(y = case_when(x > 1L ~ "a")) |>
    sql_build()
  expect_snapshot(out$select[[2]])
})

test_that("case_when translates correctly to ELSE when TRUE ~ is used 2", {
  local_con(simulate_dbi())
  expect_snapshot(
    test_translate_sql(
      case_when(
        x == 1L ~ "yes",
        x == 0L ~ "no",
        TRUE ~ "undefined"
      )
    )
  )
})

test_that("case_when uses the .default arg", {
  local_con(simulate_dbi())
  expect_snapshot(
    test_translate_sql(
      case_when(
        x == 1L ~ "yes",
        x == 0L ~ "no",
        .default = "undefined"
      )
    )
  )

  expect_snapshot(
    test_translate_sql(
      case_when(
        x == 1L ~ "yes",
        x == 0L ~ "no",
        .default = x + 1
      )
    )
  )

  # TRUE ~ has precedence over .default
  expect_snapshot(
    test_translate_sql(
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
  local_con(simulate_dbi())
  expect_snapshot({
    (expect_error(test_translate_sql(case_when(
      x == 1L ~ "yes",
      .ptype = character()
    ))))
    (expect_error(test_translate_sql(case_when(x == 1L ~ "yes", .size = 1))))
  })
})

test_that("long case_when is on multiple lines", {
  local_con(simulate_dbi())
  expect_snapshot(
    test_translate_sql(
      case_when(
        x == 1L ~ "this is long",
        x == 0L ~ "so it should",
        TRUE ~ "be wrapped"
      )
    )
  )
})

test_that("all forms of if translated to case statement", {
  local_con(simulate_dbi())
  expected <- sql("CASE WHEN `x` THEN 1 WHEN NOT `x` THEN 2 END")

  expect_equal(test_translate_sql(if (x) 1L else 2L), expected)
  expect_equal(test_translate_sql(ifelse(x, 1L, 2L)), expected)
  expect_equal(test_translate_sql(if_else(x, 1L, 2L)), expected)
})

test_that("if_else can be simplified", {
  local_con(simulate_dbi())
  expect_equal(
    test_translate_sql(if_else(x, 1L, 2L, 2L)),
    sql("CASE WHEN `x` THEN 1 ELSE 2 END")
  )
})

test_that("if translation adds parens", {
  local_con(simulate_dbi())
  expect_equal(
    test_translate_sql(if (x) y),
    sql("CASE WHEN `x` THEN `y` END")
  )

  expect_equal(
    test_translate_sql(if (x > 1L) y + 1L),
    sql("CASE WHEN (`x` > 1) THEN (`y` + 1) END")
  )

  expect_equal(
    test_translate_sql(if (x) y else z),
    sql("CASE WHEN `x` THEN `y` WHEN NOT `x` THEN `z` END")
  )

  expect_equal(
    test_translate_sql(if (x > 1L) y + 1L else z + 1L),
    sql(
      "CASE WHEN (`x` > 1) THEN (`y` + 1) WHEN NOT (`x` > 1) THEN (`z` + 1) END"
    )
  )
})

test_that("if and ifelse use correctly named arguments", {
  local_con(simulate_dbi())
  exp <- test_translate_sql(if (x) 1 else 2)

  expect_equal(test_translate_sql(ifelse(test = x, yes = 1, no = 2)), exp)
  expect_equal(
    test_translate_sql(if_else(condition = x, true = 1, false = 2)),
    exp
  )

  expect_equal(
    test_translate_sql(if_else(
      condition = x,
      true = 1,
      false = 2,
      missing = 3
    )),
    sql(
      "CASE WHEN `x` THEN 1.0 WHEN NOT `x` THEN 2.0 WHEN (`x` IS NULL) THEN 3.0 END"
    )
  )
})

test_that("switch translated to CASE WHEN", {
  local_con(simulate_dbi())
  expect_equal(
    test_translate_sql(switch(x, a = 1L)),
    sql("CASE `x` WHEN ('a') THEN (1) END")
  )
  expect_equal(
    test_translate_sql(switch(x, a = 1L, 2L)),
    sql("CASE `x` WHEN ('a') THEN (1) ELSE (2) END")
  )
})

test_that("is.na and is.null are equivalent", {
  local_con(simulate_dbi())
  # Needs to be wrapped in parens to ensure correct precedence
  expect_equal(test_translate_sql(is.na(x + y)), sql("((`x` + `y`) IS NULL)"))
  expect_equal(test_translate_sql(is.null(x + y)), sql("((`x` + `y`) IS NULL)"))

  expect_equal(test_translate_sql(x + is.na(x)), sql("`x` + (`x` IS NULL)"))
  expect_equal(test_translate_sql(!is.na(x)), sql("NOT((`x` IS NULL))"))
})

test_that("magrittr pipe is translated in conditionals", {
  local_con(simulate_dbi())
  expect_equal(
    test_translate_sql(x |> ifelse(1L, 2L)),
    sql("CASE WHEN `x` THEN 1 WHEN NOT `x` THEN 2 END")
  )
})

test_that("conditionals check arguments", {
  local_con(simulate_dbi())
  expect_snapshot(error = TRUE, test_translate_sql(case_when()))

  expect_snapshot(error = TRUE, test_translate_sql(switch(x, 1L, 2L)))
})


# case_match --------------------------------------------------------------

test_that("LHS can handle different types", {
  local_con(simulate_dbi())
  expect_equal(
    test_translate_sql(case_match(z, 1L ~ "a")),
    sql("CASE WHEN (`z` IN (1)) THEN 'a' END")
  )

  expect_equal(
    test_translate_sql(case_match(z, "x" ~ "a")),
    sql("CASE WHEN (`z` IN ('x')) THEN 'a' END")
  )

  expect_equal(
    test_translate_sql(case_match(z, y ~ "a")),
    sql("CASE WHEN (`z` IN (`y`)) THEN 'a' END")
  )

  expect_equal(
    test_translate_sql(case_match(z, as.character(y) ~ "a")),
    sql("CASE WHEN (`z` IN (CAST(`y` AS TEXT))) THEN 'a' END")
  )
})

test_that("LHS can match multiple values", {
  local_con(simulate_dbi())
  expect_equal(
    test_translate_sql(case_match(z, 1:2 ~ "a")),
    sql("CASE WHEN (`z` IN ((1, 2))) THEN 'a' END")
  )

  expect_equal(
    test_translate_sql(case_match(z, c(1L, 3L) ~ "a")),
    sql("CASE WHEN (`z` IN (1, 3)) THEN 'a' END")
  )

  expect_equal(
    test_translate_sql(case_match(z, c("x", "y") ~ "a")),
    sql("CASE WHEN (`z` IN ('x', 'y')) THEN 'a' END")
  )

  expect_equal(
    test_translate_sql(case_match(z, c(1L, y) ~ "a")),
    sql("CASE WHEN (`z` IN (1, `y`)) THEN 'a' END")
  )
})

test_that("LHS can match NA", {
  local_con(simulate_dbi())
  expect_equal(
    test_translate_sql(case_match(z, NA ~ "a")),
    sql("CASE WHEN (`z` IS NULL) THEN 'a' END")
  )

  expect_equal(
    test_translate_sql(case_match(z, c(1L, NA) ~ "a")),
    sql("CASE WHEN (`z` IN (1) OR `z` IS NULL) THEN 'a' END")
  )
})

test_that("LHS can handle bang bang", {
  local_con(simulate_dbi())
  expect_snapshot({
    test_translate_sql(case_match(x, !!1L ~ "x"))
    test_translate_sql(case_match(x, !!c(1L, 2L) ~ "x"))
    test_translate_sql(case_match(x, !!c(NA, 1L) ~ "x"))
  })
})

test_that("`NULL` values in `...` are dropped", {
  local_con(simulate_dbi())
  expect_identical(
    test_translate_sql(case_match(x, 1L ~ "a", NULL, 2L ~ "b", NULL)),
    sql("CASE WHEN (`x` IN (1)) THEN 'a' WHEN (`x` IN (2)) THEN 'b' END")
  )
})

test_that("requires at least one condition", {
  local_con(simulate_dbi())
  expect_snapshot(error = TRUE, {
    test_translate_sql(case_match(x))
  })
  expect_snapshot(error = TRUE, {
    test_translate_sql(case_match(x, NULL))
  })
})

test_that("passes through `.default` correctly", {
  local_con(simulate_dbi())
  expect_equal(
    test_translate_sql(case_match(z, 3L ~ 1L, .default = 2L)),
    sql("CASE WHEN (`z` IN (3)) THEN 1 ELSE 2 END")
  )
})

test_that("can handle multiple cases", {
  local_con(simulate_dbi())
  expect_equal(
    test_translate_sql(case_match(z, 1L ~ "a", 2L ~ "b")),
    sql("CASE WHEN (`z` IN (1)) THEN 'a' WHEN (`z` IN (2)) THEN 'b' END")
  )

  # also with .default
  expect_equal(
    test_translate_sql(case_match(z, 1L ~ "a", 2L ~ "b", .default = "default")),
    sql(
      "CASE WHEN (`z` IN (1)) THEN 'a' WHEN (`z` IN (2)) THEN 'b' ELSE 'default' END"
    )
  )
})

test_that("`.ptype` not supported", {
  local_con(simulate_dbi())
  expect_snapshot({
    (expect_error(test_translate_sql(case_match(x, 1 ~ 1, .ptype = integer()))))
  })
})

test_that(".x must be a symbol", {
  local_con(simulate_dbi())
  expect_snapshot({
    (expect_error(test_translate_sql(case_match(1, 1 ~ 1))))
  })
})
