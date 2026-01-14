test_that("case_when converted to CASE WHEN", {
  con <- dialect_ansi()
  expect_snapshot(translate_sql(case_when(x > 1L ~ "a"), con = con))
})

test_that("even inside mutate", {
  out <- lazy_frame(x = 1:5) |>
    mutate(y = case_when(x > 1L ~ "a")) |>
    sql_build()
  expect_snapshot(out$select[[2]])
})

test_that("case_when translates correctly to ELSE when TRUE ~ is used 2", {
  con <- dialect_ansi()
  expect_snapshot(
    translate_sql(
      case_when(
        x == 1L ~ "yes",
        x == 0L ~ "no",
        TRUE ~ "undefined"
      ),
      con = con
    )
  )
})

test_that("case_when uses the .default arg", {
  con <- dialect_ansi()
  expect_snapshot(
    translate_sql(
      case_when(
        x == 1L ~ "yes",
        x == 0L ~ "no",
        .default = "undefined"
      ),
      con = con
    )
  )

  expect_snapshot(
    translate_sql(
      case_when(
        x == 1L ~ "yes",
        x == 0L ~ "no",
        .default = x + 1
      ),
      con = con
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
      ),
      con = con
    )
  )
})

test_that("case_when does not support .ptype and .size", {
  con <- dialect_ansi()
  expect_snapshot({
    (expect_error(translate_sql(
      case_when(
        x == 1L ~ "yes",
        .ptype = character()
      ),
      con = con
    )))
    (expect_error(translate_sql(
      case_when(x == 1L ~ "yes", .size = 1),
      con = con
    )))
  })
})

test_that("long case_when is on multiple lines", {
  con <- dialect_ansi()
  expect_snapshot(
    translate_sql(
      case_when(
        x == 1L ~ "this is long",
        x == 0L ~ "so it should",
        TRUE ~ "be wrapped"
      ),
      con = con
    )
  )
})

test_that("all forms of if translated to case statement", {
  con <- dialect_ansi()

  expect_translation(
    con,
    if (x) 1L else 2L,
    "CASE WHEN \"x\" THEN 1 WHEN NOT \"x\" THEN 2 END"
  )
  expect_translation(
    con,
    ifelse(x, 1L, 2L),
    "CASE WHEN \"x\" THEN 1 WHEN NOT \"x\" THEN 2 END"
  )
  expect_translation(
    con,
    if_else(x, 1L, 2L),
    "CASE WHEN \"x\" THEN 1 WHEN NOT \"x\" THEN 2 END"
  )
})

test_that("if_else can be simplified", {
  con <- dialect_ansi()
  expect_translation(
    con,
    if_else(x, 1L, 2L, 2L),
    "CASE WHEN \"x\" THEN 1 ELSE 2 END"
  )
})

test_that("if translation adds parens", {
  con <- dialect_ansi()
  expect_translation(con, if (x) y, "CASE WHEN \"x\" THEN \"y\" END")
  expect_translation(
    con,
    if (x > 1L) y + 1L,
    "CASE WHEN (\"x\" > 1) THEN (\"y\" + 1) END"
  )
  expect_translation(
    con,
    if (x) y else z,
    "CASE WHEN \"x\" THEN \"y\" WHEN NOT \"x\" THEN \"z\" END"
  )
  expect_translation(
    con,
    if (x > 1L) y + 1L else z + 1L,
    "CASE WHEN (\"x\" > 1) THEN (\"y\" + 1) WHEN NOT (\"x\" > 1) THEN (\"z\" + 1) END"
  )
})

test_that("if_else can translate missing", {
  con <- dialect_ansi()
  expect_translation(
    con,
    if_else(x, 1L, 2L, 3L),
    "CASE WHEN \"x\" THEN 1 WHEN NOT \"x\" THEN 2 ELSE 3 END"
  )
})

test_that("if and ifelse use correctly named arguments", {
  con <- dialect_ansi()
  exp <- translate_sql(if (x) 1 else 2, con = con)

  expect_equal(translate_sql(ifelse(test = x, yes = 1, no = 2), con = con), exp)
  expect_equal(
    translate_sql(if_else(condition = x, true = 1, false = 2), con = con),
    exp
  )

  expect_translation(
    con,
    if_else(
      condition = x,
      true = 1,
      false = 2,
      missing = 3
    ),
    "CASE WHEN \"x\" THEN 1.0 WHEN NOT \"x\" THEN 2.0 ELSE 3.0 END"
  )
})

test_that("switch translated to CASE WHEN", {
  con <- dialect_ansi()
  expect_translation(
    con,
    switch(x, a = 1L),
    "CASE \"x\" WHEN ('a') THEN (1) END"
  )
  expect_translation(
    con,
    switch(x, a = 1L, 2L),
    "CASE \"x\" WHEN ('a') THEN (1) ELSE (2) END"
  )
})

test_that("is.na and is.null are equivalent", {
  con <- dialect_ansi()
  # Needs to be wrapped in parens to ensure correct precedence
  expect_translation(con, is.na(x + y), "((\"x\" + \"y\") IS NULL)")
  expect_translation(con, is.null(x + y), "((\"x\" + \"y\") IS NULL)")

  expect_translation(con, x + is.na(x), "\"x\" + (\"x\" IS NULL)")
  expect_translation(con, !is.na(x), "NOT((\"x\" IS NULL))")
})

test_that("magrittr pipe is translated in conditionals", {
  con <- dialect_ansi()
  expect_translation(
    con,
    x |> ifelse(1L, 2L),
    "CASE WHEN \"x\" THEN 1 WHEN NOT \"x\" THEN 2 END"
  )
})

test_that("conditionals check arguments", {
  con <- dialect_ansi()
  expect_snapshot(error = TRUE, translate_sql(case_when(), con = con))

  expect_snapshot(error = TRUE, translate_sql(switch(x, 1L, 2L), con = con))
})


# case_match --------------------------------------------------------------

test_that("LHS can handle different types", {
  con <- dialect_ansi()
  expect_translation(
    con,
    case_match(z, 1L ~ "a"),
    "CASE WHEN (\"z\" IN (1)) THEN 'a' END"
  )

  expect_translation(
    con,
    case_match(z, "x" ~ "a"),
    "CASE WHEN (\"z\" IN ('x')) THEN 'a' END"
  )

  expect_translation(
    con,
    case_match(z, y ~ "a"),
    "CASE WHEN (\"z\" IN (\"y\")) THEN 'a' END"
  )

  expect_translation(
    con,
    case_match(z, as.character(y) ~ "a"),
    "CASE WHEN (\"z\" IN (CAST(\"y\" AS TEXT))) THEN 'a' END"
  )
})

test_that("LHS can match multiple values", {
  con <- dialect_ansi()
  expect_translation(
    con,
    case_match(z, 1:2 ~ "a"),
    "CASE WHEN (\"z\" IN ((1, 2))) THEN 'a' END"
  )

  expect_translation(
    con,
    case_match(z, c(1L, 3L) ~ "a"),
    "CASE WHEN (\"z\" IN (1, 3)) THEN 'a' END"
  )

  expect_translation(
    con,
    case_match(z, c("x", "y") ~ "a"),
    "CASE WHEN (\"z\" IN ('x', 'y')) THEN 'a' END"
  )

  expect_translation(
    con,
    case_match(z, c(1L, y) ~ "a"),
    "CASE WHEN (\"z\" IN (1, \"y\")) THEN 'a' END"
  )
})

test_that("LHS can match NA", {
  con <- dialect_ansi()
  expect_translation(
    con,
    case_match(z, NA ~ "a"),
    "CASE WHEN (\"z\" IS NULL) THEN 'a' END"
  )

  expect_translation(
    con,
    case_match(z, c(1L, NA) ~ "a"),
    "CASE WHEN (\"z\" IN (1) OR \"z\" IS NULL) THEN 'a' END"
  )
})

test_that("LHS can handle bang bang", {
  con <- dialect_ansi()
  expect_snapshot({
    translate_sql(case_match(x, !!1L ~ "x"), con = con)
    translate_sql(case_match(x, !!c(1L, 2L) ~ "x"), con = con)
    translate_sql(case_match(x, !!c(NA, 1L) ~ "x"), con = con)
  })
})

test_that("`NULL` values in `...` are dropped", {
  con <- dialect_ansi()
  expect_translation(
    con,
    case_match(x, 1L ~ "a", NULL, 2L ~ "b", NULL),
    "CASE WHEN (\"x\" IN (1)) THEN 'a' WHEN (\"x\" IN (2)) THEN 'b' END"
  )
})

test_that("requires at least one condition", {
  con <- dialect_ansi()
  expect_snapshot(error = TRUE, {
    translate_sql(case_match(x), con = con)
  })
  expect_snapshot(error = TRUE, {
    translate_sql(case_match(x, NULL), con = con)
  })
})

test_that("passes through `.default` correctly", {
  con <- dialect_ansi()
  expect_translation(
    con,
    case_match(z, 3L ~ 1L, .default = 2L),
    "CASE WHEN (\"z\" IN (3)) THEN 1 ELSE 2 END"
  )
})

test_that("can handle multiple cases", {
  con <- dialect_ansi()
  expect_translation(
    con,
    case_match(z, 1L ~ "a", 2L ~ "b"),
    "CASE WHEN (\"z\" IN (1)) THEN 'a' WHEN (\"z\" IN (2)) THEN 'b' END"
  )

  # also with .default
  expect_translation(
    con,
    case_match(z, 1L ~ "a", 2L ~ "b", .default = "default"),
    "CASE WHEN (\"z\" IN (1)) THEN 'a' WHEN (\"z\" IN (2)) THEN 'b' ELSE 'default' END"
  )
})

test_that("`.ptype` not supported", {
  con <- dialect_ansi()
  expect_snapshot({
    (expect_error(translate_sql(
      case_match(x, 1 ~ 1, .ptype = integer()),
      con = con
    )))
  })
})

test_that(".x must be a symbol", {
  con <- dialect_ansi()
  expect_snapshot({
    (expect_error(translate_sql(case_match(1, 1 ~ 1), con = con)))
  })
})
