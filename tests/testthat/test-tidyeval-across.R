# across() ----------------------------------------------------------------
# test partial_eval_across() indirectly via SQL generation

test_that("across() translates NULL", {
  lf <- lazy_frame(a = 1,  b = 2)
  expect_equal(
    capture_across(lf, across(a:b)),
    list(a = expr(a), b = expr(b))
  )
})

test_that("across() drops groups", {
  lf <- lazy_frame(a = 1, b = 2)

  expect_equal(
    capture_across(group_by(lf, a), across(everything())),
    list(b = expr(b))
  )
  expect_equal(
    capture_across(group_by(lf, b), across(everything())),
    list(a = expr(a))
  )
})


test_that("across() translates functions", {
  lf <- lazy_frame(a = 1,  b = 2)

  expect_equal(
    capture_across(lf, across(a:b, log)),
    exprs(a = log(a), b = log(b))
  )

  expect_equal(
    capture_across(lf, across(a, list(log, exp))),
    exprs(a_1 = log(a), a_2 = exp(a))
  )
})

test_that("across() captures anonymous functions", {
  lf <- lazy_frame(a = 1)

  expect_equal(
    capture_across(lf, across(a, function(x) log(x))),
    list(a = expr(log(a)))
  )

  expect_snapshot(
    (expect_error(capture_across(lf, across(a, function(x) {
      x <- x + 2
      log(x)
      }
    ))))
  )
})

test_that("across() translates formulas", {
  lf <- lazy_frame(a = 1,  b = 2)

  expect_equal(
    capture_across(lf, across(a:b, ~ log(.x))),
    exprs(a = log(a), b = log(b))
  )

  expect_equal(
    capture_across(lf, across(a:b, ~2)),
    exprs(a = 2, b = 2)
  )

  expect_equal(
    capture_across(lf, across(a:b, list(~log(.x)))),
    exprs(a_1 = log(a), b_1 = log(b))
  )
})

test_that("across() translates evaluated functions", {
  lf <- lazy_frame(x = 1)

  expect_equal(
    capture_across(lf, across(.fns = !!sum)),
    exprs(x = sum(x))
  )

  expect_equal(
    capture_across(lf, across(.fns = !!function(x) sum(x + 2))),
    exprs(x = sum(x + 2))
  )
})

test_that("across() gives informative errors", {
  lf <- lazy_frame(a = 1,  b = 2)
  expect_snapshot(error = TRUE, {
    capture_across(lf, across(a, 1))
    capture_across(lf, across(a, list(1)))

    capture_across(lf, across(a:b, "log"))

    capture_across(lf, across(c, mean))
  })
})

test_that("across() can use named selections", {
  lf <- lazy_frame(x = 1, y = 2)

  # no fns
  expect_equal(
    capture_across(lf, across(c(a = x, b = y))),
    list(a = quote(x), b = quote(y))
  )
  expect_equal(
    capture_across(lf, across(all_of(c(a = "x", b = "y")))),
    list(a = quote(x), b = quote(y))
  )

  # one fn
  expect_equal(
    capture_across(lf, across(c(a = x, b = y), mean)),
    list(a = quote(mean(x)), b = quote(mean(y)))
  )
  expect_equal(
    capture_across(lf, across(all_of(c(a = "x", b = "y")), mean)),
    list(a = quote(mean(x)), b = quote(mean(y)))
  )

  # multiple fns
  expect_equal(
    capture_across(lf, across(c(a = x, b = y), list(mean, nm = sum))),
    list(
      a_1 = quote(mean(x)), a_nm = quote(sum(x)),
      b_1 = quote(mean(y)), b_nm = quote(sum(y))
    )
  )
  expect_equal(
    capture_across(lf, across(all_of(c(a = "x", b = "y")), list(mean, nm = sum))),
    list(
      a_1 = quote(mean(x)), a_nm = quote(sum(x)),
      b_1 = quote(mean(y)), b_nm = quote(sum(y))
    )
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

test_that("across(.names=) can use local variables in addition to {col} and {fn}", {
  res <- local({
    prefix <- "MEAN"
    lazy_frame(x = 42) %>%
      summarise(across(everything(), ~ mean(.x, na.rm = TRUE), .names = "{prefix}_{.col}"))
  })
  expect_equal(op_vars(res), "MEAN_x")
})

test_that("across() .cols is evaluated in across()'s calling environment", {
  lf <- lazy_frame(y = 1)
  fun <- function(x) capture_across(lf, across(all_of(x)))
  expect_equal(
    fun("y"),
    list(y = expr(y))
  )
})

test_that("across() can handle empty selection", {
  lf <- lazy_frame(x = 1, y = 2)

  expect_equal(
    lf %>% mutate(across(character(), c)) %>% remote_query(),
    sql("SELECT *\nFROM `df`")
  )
})

test_that("across() defaults to everything()", {
  # SELECT `x` + 1.0 AS `x`, `y` + 1.0 AS `y`
  expect_snapshot(
    lazy_frame(x = 1, y = 1) %>% summarise(across(.fns = ~ . + 1))
  )
})

test_that("untranslatable functions are preserved", {
  lf <- lazy_frame(a = 1, b = 2)
  expect_snapshot(lf %>% summarise(across(a:b, SQL_LOG)))
})

test_that("old _at functions continue to work", {
  withr::local_options(lifecycle_verbosity = "quiet")
  reset_warning_verbosity("dbplyr_check_na_rm")
  lf <- lazy_frame(a = 1, b = 2)

  expect_snapshot(lf %>% dplyr::summarise_at(dplyr::vars(a:b), "sum"))
  expect_snapshot(lf %>% dplyr::summarise_at(dplyr::vars(a:b), sum))
  expect_snapshot(lf %>% dplyr::summarise_at(dplyr::vars(a:b), ~ sum(.)))
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

  skip("not yet correctly supported")
  # dplyr uses the old value of `y` for division
  df <- tibble(x = 2, y = 4, z = 8)
  df %>% mutate(across(everything(), ~ .x / .data$y))
  # so this is the equivalent
  df %>% mutate(data.frame(x = x / y, y = y / y, z = z / y))
  # dbplyr uses the new value of `y`
  lf %>% mutate(across(everything(), ~ .x / .data$y))

  # so this is the dbplyr equivalent
  df %>% mutate(x = x / y, y = y / y, z = z / y)
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

test_that("across() translates evaluated lists", {
  lf <- lazy_frame(x = 1)
  fun_list <- list(mean, ~ mean(.x + 1, na.rm = TRUE), function(x) mean(x + 2))

  expect_equal(
    capture_across(lf, across(.fns = !!fun_list)),
    exprs(
      x_1 = mean(x),
      x_2 = mean(x + 1, na.rm = TRUE),
      x_3 = mean(x + 2),
    )
  )
})

test_that("across() translates evaluated quosures", {
  lf <- lazy_frame(a = 1, b = 2)

  expect_equal(
    capture_across(lf, across(a:b, !!quo(list(log)))),
    exprs(
      a_1 = log(a),
      b_1 = log(b)
    )
  )

  expect_equal(
    capture_across(lf, across(a:b, !!quo(~ log(.x, 2)))),
    exprs(
      a = log(a, 2),
      b = log(b, 2)
    )
  )

  expect_equal(
    capture_across(lf, across(a:b, !!quo(list(log2 = ~ log(.x, 2))))),
    exprs(
      a_log2 = log(a, 2),
      b_log2 = log(b, 2)
    )
  )
})

test_that("across() searches for list in environment", {
  lf <- lazy_frame(a = 1, b = 2)
  list_formula <- list(~ log(.x), sum)

  expect_equal(
    capture_across(lf, across(a:b, list_formula)),
    exprs(
      a_1 = log(a),
      a_2 = sum(a),
      b_1 = log(b),
      b_2 = sum(b)
    )
  )
})

test_that("across() handles cur_column()", {
  lf <- lazy_frame(a = 1, b = 2)

  expect_equal(
    capture_across(lf, across(a:b, ~ paste0(.x, cur_column()))),
    exprs(a = paste0(a, "a"), b = paste0(b, "b"))
  )

  expect_equal(
    capture_across(lf, across(a:b, function(x) paste0(x, cur_column()))),
    exprs(a = paste0(a, "a"), b = paste0(b, "b"))
  )
})

test_that("across() errors if named", {
  lf <- lazy_frame(a = 1, b = 2)

  expect_snapshot({
    (expect_error(mutate(lf, x = across())))
    (expect_error(group_by(lf, x = across())))
  })
})

test_that("across() throws error if unpack = TRUE", {
  lf <- lazy_frame(x = 1, y = 2)

  expect_snapshot(
    (expect_error(lf %>% mutate(across(x, .unpack = TRUE))))
  )
})


# if_all ------------------------------------------------------------------

test_that("if_all() translates functions", {
  lf <- lazy_frame(a = 1,  b = 2)

  expect_equal(
    capture_if_all(lf, if_all(a:b, log)),
    expr(log(a) & log(b))
  )

  expect_equal(
    capture_if_all(lf, if_all(a, list(log, exp))),
    expr(log(a) & exp(a))
  )
})

test_that("if_all() translates formulas", {
  lf <- lazy_frame(a = 1,  b = 2)

  expect_equal(
    capture_if_all(lf, if_all(a:b, ~ log(.x))),
    expr(log(a) & log(b))
  )

  expect_equal(
    capture_if_all(lf, if_all(a:b, ~2)),
    expr(2 & 2)
  )

  expect_equal(
    capture_if_all(lf, if_all(a:b, list(~log(.x)))),
    expr(log(a) & log(b))
  )
})

test_that("if_all() gives informative errors", {
  lf <- lazy_frame(a = 1,  b = 2)
  expect_snapshot(error = TRUE, {
    capture_if_all(lf, if_all(a, 1))
    capture_if_all(lf, if_all(a, list(1)))
  })
})

test_that("if_all collapses multiple expresions", {
  lf <- lazy_frame(a = 1,  b = 2)
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

test_that("if_all() drops groups", {
  lf <- lazy_frame(a = 1, b = 2)

  expect_equal(
    capture_if_all(group_by(lf, a), if_all(everything())),
    sym("b")
  )
  expect_equal(
    capture_if_all(group_by(lf, b), if_all(everything())),
    sym("a")
  )
})

test_that("if_any() and if_all() expansions deal with single inputs", {
  d <- lazy_frame(x = 1)

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

test_that("if_all() cannot rename variables", {
  lf <- lazy_frame(x = 1, y = 2)

  # no fns
  expect_snapshot(
    (expect_error(capture_if_all(lf, if_all(c(a = x, b = y)))))
  )
})

test_that("if_all() can handle empty selection", {
  skip("tidyselect issue #221")
  lf <- lazy_frame(x = 1, y = 2)

  expect_equal(
    lf %>% mutate(if_all(character(), c)) %>% show_query(),
    expr(lf)
  )
})

test_that("across() .cols is evaluated in across()'s calling environment", {
  lf <- lazy_frame(y = 1)
  fun <- function(x) capture_if_all(lf, if_all(all_of(x)))
  expect_equal(
    fun("y"),
    expr(y)
  )
})


# dots --------------------------------------------------------------------

test_that("across(...) is deprecated", {
  lf <- lazy_frame(x = c(1, NA))
  expect_snapshot(summarise(lf, across(everything(), mean, na.rm = TRUE)))

})

test_that("across() does not support formulas with dots", {
  options(lifecycle_verbosity = "quiet")
  lf <- lazy_frame(a = 1,  b = 2)

  expect_snapshot({
    (expect_error(capture_across(lf, across(a:b, ~log(.x, base = .y), base = 2))))
    (expect_error(capture_across(lf, across(a:b, list(~log(.x, base = .y)), base = 2))))
  })
})

test_that("across() translates functions", {
  options(lifecycle_verbosity = "quiet")
  lf <- lazy_frame(a = 1,  b = 2)

  expect_equal(
    capture_across(lf, across(a:b, log, base = 2)),
    exprs(a = log(a, base = 2), b = log(b, base = 2))
  )
})

test_that("dots are translated too", {
  options(lifecycle_verbosity = "quiet")
  fun <- function() {
    lf <- lazy_frame(a = 1, b = 2)
    z <- TRUE
    capture_across(lf, across(a, mean, na.rm = z))
  }

  expect_equal(fun(), exprs(a = mean(a, na.rm = TRUE)))
})

test_that("if_all() translates functions", {
  options(lifecycle_verbosity = "quiet")
  lf <- lazy_frame(a = 1,  b = 2)

  expect_equal(
    capture_if_all(lf, if_all(a:b, log)),
    expr(log(a) & log(b))
  )

  expect_equal(
    capture_if_all(lf, if_all(a:b, log, base = 2)),
    expr(log(a, base = 2) & log(b, base = 2))
  )

  expect_equal(
    capture_if_all(lf, if_all(a, list(log, exp))),
    expr(log(a) & exp(a))
  )
})

test_that("if_all() translates dots", {
  options(lifecycle_verbosity = "quiet")
  fun <- function() {
    lf <- lazy_frame(a = 1, b = 2)
    z <- TRUE
    capture_if_all(lf, if_all(a, mean, na.rm = z))
  }

  expect_equal(fun(), expr(mean(a, na.rm = TRUE)))
})


# pick --------------------------------------------------------------------

# pick() + arrange()

test_that("can `arrange()` with `pick()` selection", {
  df <- lazy_frame(x = c(2, 2, 1), y = c(3, 1, 3))

  expect_identical(
    arrange(df, pick(x, y)) %>% remote_query(),
    sql("SELECT *\nFROM `df`\nORDER BY `x`, `y`")
  )

  expect_identical(
    arrange(df, pick(x), y) %>% remote_query(),
    sql("SELECT *\nFROM `df`\nORDER BY `x`, `y`")
  )
})

test_that("`pick()` errors in `arrange()` are useful", {
  df <- lazy_frame(x = 1)

  expect_snapshot(error = TRUE, {
    arrange(df, pick(y))
  })
})

test_that("doesn't allow renaming", {
  expect_snapshot(error = TRUE, {
    arrange(lazy_frame(x = 1), pick(y = x))
  })
})

test_that("requires at least one input", {
  expect_snapshot(error = TRUE, {
    arrange(lazy_frame(x = 1), pick())
  })
})

# pick() + filter()

test_that("`filter()` with `pick()` that uses invalid tidy-selection errors", {
  df <- lazy_frame(x = c(1, 2, NA, 3), y = c(2, NA, 5, 3))

  expect_snapshot(error = TRUE, {
    filter(df, pick(x, a))
  })
})

# pick() + group_by()

test_that("`pick()` can be used inside `group_by()` wrappers", {
  df <- lazy_frame(a = 1:3, b = 2:4, c = 3:5)

  tidyselect_group_by <- function(data, groups) {
    group_by(data, pick({{ groups }}))
  }
  expect_identical(
    tidyselect_group_by(df, c(a, c)),
    group_by(df, a, c)
  )
})

