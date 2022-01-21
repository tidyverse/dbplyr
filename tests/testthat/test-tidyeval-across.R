# across() ----------------------------------------------------------------
# test partial_eval_across() indirectly via SQL generation

test_that("across() gives meaningful messages", {
  expect_snapshot({
    # expanding
    (expect_error(
      lazy_frame(x = 1) %>%
        summarise(across(x, 42))
    ))
    (expect_error(
      lazy_frame(x = 1) %>%
        summarise(across(y, mean))
    ))
  })
})

test_that("across() translates character vectors", {
  lf <- lazy_frame(a = 1, b = 2)
  expect_snapshot(lf %>% summarise(across(a:b, "log")))
  expect_snapshot(lf %>% summarise(across(a:b, "log", base = 2)))

  expect_snapshot(lf %>% summarise(across(a, c("log", "exp"))))

  out <- lf %>% summarise(across(a:b, c(x = "log", y = "exp")))
  expect_equal(colnames(out), c("a_x", "a_y", "b_x", "b_y"))
})

test_that("across() translates functions", {
  lf <- lazy_frame(a = 1, b = 2)
  expect_snapshot(lf %>% summarise(across(a:b, log)))
  expect_snapshot(lf %>% summarise(across(a:b, log, base = 2)))

  expect_snapshot(lf %>% summarise(across(a:b, list(log, exp))))

  out <- lf %>% summarise(across(a:b, list(x = log, y = exp)))
  expect_equal(colnames(out), c("a_x", "a_y", "b_x", "b_y"))
})

test_that("untranslatable functions are preserved", {
  lf <- lazy_frame(a = 1, b = 2)
  expect_snapshot(lf %>% summarise(across(a:b, SQL_LOG)))
})

test_that("across() translates formulas", {
  lf <- lazy_frame(a = 1, b = 2)
  expect_snapshot(lf %>% summarise(across(a:b, ~ log(.x, 2))))
  expect_snapshot(lf %>% summarise(across(a:b, list(~ log(.x, 2)))))
})

test_that("across() translates NULL", {
  lf <- lazy_frame(a = 1, b = 2)
  expect_snapshot(lf %>% mutate(across(a:b)))
})

test_that("across() can rename", {
  expect_equal(
    lazy_frame(x = 1:10) %>%
      mutate(across(.cols = c(y = x), .fns = as.character)) %>%
      op_vars(),
    c("x", "y")
  )
})

test_that("across() works with .cols = character()", {
  lf <- lazy_frame(x = 1:10)

  expect_equal(
    lf %>% mutate(across(.cols = character(), .fns = as.character)),
    lf
  )
})

test_that("partial_eval_dots() names automatically", {
  expect_named(
    partial_eval_dots(lazy_frame(a = 1, b = 2), across(everything())),
    c("a", "b")
  )
})

test_that("across() correctly names output columns", {
  gf <- lazy_frame(x = 1, y = 2, z = 3, s = "") %>% group_by(x)

  expect_equal(
    summarise(gf, across()) %>% op_vars(),
    c("x", "y", "z", "s")
  )
  expect_equal(
    summarise(gf, across(.names = "id_{.col}")) %>% op_vars(),
    c("x", "id_y", "id_z", "id_s")
  )
  expect_equal(
    summarise(gf, across(1:2, mean)) %>% op_vars(),
    c("x", "y", "z")
  )
  expect_equal(
    summarise(gf, across(1:2, mean, .names = "mean_{.col}")) %>% op_vars(),
    c("x", "mean_y", "mean_z")
  )
  expect_equal(
    summarise(gf, across(1:2, list(mean = mean, sum = sum))) %>% op_vars(),
    c("x", "y_mean", "y_sum", "z_mean", "z_sum")
  )
  expect_equal(
    summarise(gf, across(1:2, list(mean = mean, sum = sum), .names = "{.fn}_{.col}")) %>% op_vars(),
    c("x", "mean_y", "sum_y", "mean_z", "sum_z")
  )

  skip("not yet decided")
  expect_equal(
    summarise(gf, across(1:2, list(mean = mean, sum))) %>% op_vars(),
    c("x", "y_mean", "y_2", "z_mean", "z_2")
  )
  expect_equal(
    summarise(gf, across(1:2, list(mean, sum = sum))) %>% op_vars(),
    c("x", "y_1", "y_sum", "z_1", "z_sum")
  )
  expect_equal(
    summarise(gf, across(1:2, list(mean, sum))) %>% op_vars(),
    c("x", "y_1", "y_2", "z_1", "z_2")
  )
})

test_that("old _at functions continue to work", {
  withr::local_options(lifecycle_verbosity = "quiet")
  lf <- lazy_frame(a = 1, b = 2)

  expect_snapshot(lf %>% dplyr::summarise_at(dplyr::vars(a:b), "sum"))
  expect_snapshot(lf %>% dplyr::summarise_at(dplyr::vars(a:b), sum))
  expect_snapshot(lf %>% dplyr::summarise_at(dplyr::vars(a:b), ~ sum(.)))
})

test_that("across() defaults to everything()", {
  # SELECT `x` + 1.0 AS `x`, `y` + 1.0 AS `y`
  expect_snapshot(
    lazy_frame(x = 1, y = 1) %>% summarise(across(.fns = ~ . + 1))
  )
})

test_that("across() passes ... to functions", {
  lf <- lazy_frame(a = 1, b = 2)
  expect_equal(
    partial_eval_dots(lf, across(everything(), log, base = 2)),
    unclass(quos(a = log(a, base = 2), b = log(b, base = 2)))
  )

  expect_equal(
    partial_eval_dots(
      lf,
      across(everything(), list(times = `*`, plus = `+`), 2)
    ),
    quos(
      a_times = a * 2,
      a_plus = a + 2,
      b_times = b * 2,
      b_plus = b + 2
    ) %>%
      unclass()
  )
})

test_that("across(.names=) can use local variables in addition to {col} and {fn}", {
  res <- local({
    prefix <- "MEAN"
    lazy_frame(x = 42) %>%
      summarise(across(everything(), ~ mean(.x, na.rm = TRUE), .names = "{prefix}_{.col}"))
  })
  expect_equal(op_vars(res), "MEAN_x")
})

test_that("across() uses environment from the current quosure (dplyr#5460)", {
  # If the data frame `y` is selected, causes a subscript conversion
  # error since it is fractional
  lf <- lazy_frame(x = 1, y = 2.4)
  y <- "x"

  expect_equal(
    partial_eval_dots(lf, across(all_of(y), mean)),
    list(x = quo(mean(x)))
  )

  expect_equal(
    partial_eval_dots(lf, if_all(all_of(y), ~ .x < 2)),
    list(quo(x < 2)),
    ignore_attr = "names"
  )
})

test_that("lambdas in across() can use columns", {
  lf <- lazy_frame(x = 2, y = 4, z = 8)

  expect_equal(
    partial_eval_dots(lf, across(everything(), ~ .x / y)),
    list(
      x = quo(x / y),
      y = quo(y / y),
      z = quo(z / y)
    )
  )

  skip(".data not yet supported in across()")
  df <- tibble(x = 2, y = 4, z = 8)
  expect_identical(
    df %>% mutate(data.frame(x = x / y, y = y / y, z = z / y)),
    df %>% mutate(across(everything(), ~ .x / .data$y))
  )
})

test_that("can pass quosure through `across()`", {
  summarise_mean <- function(data, vars) {
    data %>% summarise(across({{ vars }}, ~ mean(.x, na.rm = TRUE)))
  }
  gdf <- lazy_frame(g = c(1, 1, 2), x = 1:3) %>% group_by(g)

  expect_equal(
    gdf %>% summarise_mean(x) %>% remote_query(),
    summarise(gdf, x = mean(x, na.rm = TRUE)) %>% remote_query()
  )
})

test_that("across() can use named selections", {
  df <- lazy_frame(x = 1, y = 2)

  # no fns
  expect_equal(
    df %>% summarise(across(c(a = x, b = y))) %>% remote_query(),
    sql("SELECT `x` AS `a`, `y` AS `b`\nFROM `df`")
  )

  expect_equal(
    df %>% summarise(across(all_of(c(a = "x", b = "y")))) %>% remote_query(),
    sql("SELECT `x` AS `a`, `y` AS `b`\nFROM `df`")
  )

  # one fn
  expect_equal(
    df %>%
      summarise(across(c(a = x, b = y), ~ mean(.x, na.rm = TRUE))) %>%
      remote_query(),
    sql("SELECT AVG(`x`) AS `a`, AVG(`y`) AS `b`\nFROM `df`")
  )
  expect_equal(
    df %>%
      summarise(across(all_of(c(a = "x", b = "y")), ~ mean(.x, na.rm = TRUE))) %>%
      remote_query(),
    sql("SELECT AVG(`x`) AS `a`, AVG(`y`) AS `b`\nFROM `df`")
  )

  # multiple fns
  expect_snapshot(
    df %>%
      summarise(across(c(a = x, b = y), list(mean = mean, sum = sum), na.rm = TRUE))
  )
  expect_snapshot(
    df %>%
      summarise(across(all_of(c(a = "x", b = "y")), list(mean = mean, sum = sum)), na.rm = TRUE)
  )
})



# if_all ------------------------------------------------------------------

test_that("if_all translations names, strings, and formulas", {
  lf <- lazy_frame(a = 1,  b = 2)

  expect_equal(capture_if_all(lf, if_all(a, is.na)), expr(is.na(a)))
  expect_equal(capture_if_all(lf, if_all(a, "is.na")), expr(is.na(a)))
  expect_equal(capture_if_all(lf, if_all(a, ~ is.na(.))), expr(is.na(a)))
})

test_that("if_all collapses multiple expresions", {
  lf <- lazy_frame(data.frame(a = 1,  b = 2))
  expect_equal(
    capture_if_all(lf, if_all(everything(), is.na)),
    expr(is.na(a) & is.na(b))
  )
})

test_that("if_all/any works in filter()", {
  lf <- lazy_frame(a = 1,  b = 2)

  expect_snapshot(lf %>% filter(if_all(a:b, ~ . > 0)))
  expect_snapshot(lf %>% filter(if_any(a:b, ~ . > 0)))
})

test_that("if_all/any works in mutate()", {
  lf <- lazy_frame(a = 1,  b = 2)

  expect_snapshot(lf %>% mutate(c = if_all(a:b, ~ . > 0)))
  expect_snapshot(lf %>% mutate(c = if_any(a:b, ~ . > 0)))
})

test_that("if_all/any uses every colum as default", {
  lf <- lazy_frame(a = 1, b = 2)

  expect_snapshot(lf %>% filter(if_all(.fns = ~ . > 0)))
  expect_snapshot(lf %>% filter(if_any(.fns = ~ . > 0)))
})

test_that("if_all/any works without `.fns` argument", {
  lf <- lazy_frame(a = 1, b = 2)

  expect_snapshot(lf %>% filter(if_all(a:b)))
  expect_snapshot(lf %>% filter(if_any(a:b)))
})


test_that("if_any() and if_all() expansions deal with no inputs or single inputs", {
  d <- lazy_frame(x = 1)

  # No inputs
  expect_equal(
    filter(d, if_any(starts_with("c"), ~ FALSE)) %>% remote_query(),
    sql("SELECT *\nFROM `df`")
  )
  expect_equal(
    filter(d, if_all(starts_with("c"), ~ FALSE)) %>% remote_query(),
    sql("SELECT *\nFROM `df`")
  )

  # Single inputs
  expect_equal(
    filter(d, if_any(x, ~ FALSE)) %>% remote_query(),
    sql("SELECT *\nFROM `df`\nWHERE (FALSE)")
  )
  expect_equal(
    filter(d, if_all(x, ~ FALSE)) %>% remote_query(),
    sql("SELECT *\nFROM `df`\nWHERE (FALSE)")
  )
})
