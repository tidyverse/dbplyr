test_that("namespace operators always evaluated locally", {
  expect_equal(partial_eval(quote(base::sum(1, 2))), 3)
  expect_equal(partial_eval(quote(base:::sum(1, 2))), 3)
})

test_that("namespaced calls to dplyr functions are stripped", {
  expect_equal(partial_eval(quote(dplyr::n())), expr(n()))
})

test_that("use quosure environment for unevaluted formulas", {
  x <- 1
  expect_equal(partial_eval(expr(~x)), quote(~1))
})

test_that("can look up inlined function", {
  expect_equal(
    partial_eval(expr((!!mean)(x)), vars = "x"),
    expr(mean(x))
  )
})

test_that("respects tidy evaluation pronouns", {
  x <- "X"
  X <- "XX"

  expect_equal(partial_eval(expr(.data$x)), expr(x))
  expect_equal(partial_eval(expr(.data[["x"]])), expr(x))
  expect_equal(partial_eval(expr(.data[[x]])), expr(X))

  expect_equal(partial_eval(expr(.env$x)), "X")
  expect_equal(partial_eval(expr(.env[["x"]])), "X")
  expect_equal(partial_eval(expr(.env[[x]])), "XX")
})

test_that("fails with multi-classes", {
  x <- structure(list(), class = c('a', 'b'))
  expect_error(partial_eval(x), "Unknown input type", fixed = TRUE)
})

# across() ----------------------------------------------------------------
# test partial_eval_across() indirectly via SQL generation

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
    partial_eval_dots(quos(across(everything())), c("a", "b")),
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
  expect_equal(
    partial_eval_dots(quos(across(everything(), log, base = 2)), c("a", "b")),
    unclass(quos(a = log(a, base = 2), b = log(b, base = 2)))
  )

  expect_equal(
    partial_eval_dots(
      quos(across(everything(), list(times = `*`, plus = `+`), 2)),
      vars = c("a", "b")
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
  df <- lazy_frame(x = 1, y = 2.4)
  y <- "x"

  expect_equal(
    partial_eval_dots(quos(across(all_of(y), mean)), c("x", "y")),
    list(x = quo(mean(x)))
  )

  expect_equal(
    partial_eval_dots(quos(if_all(all_of(y), ~ .x < 2)), c("x", "y")),
    set_names(list(quo(x < 2)), "")
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
