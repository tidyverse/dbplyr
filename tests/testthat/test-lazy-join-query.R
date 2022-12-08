test_that("lazy_join_query() checks arguments", {
  vars0 <- list(
    alias = c("x", "y", "z"),
    x = c("x", "y", NA),
    y = c(NA, NA, "z"),
    all_x = c("x", "y"),
    all_y = c("z", "x")
  )
  by0 <- list(x = "x", y = "x", x_as = ident("LHS"), y_as = ident("RHS"))
  lmod <- purrr::list_modify

  my_lazy_join_query <- function(x = lazy_frame(x = 1, y = 2)$lazy_query,
                                 y = lazy_frame(x = 1, z = 2)$lazy_query,
                                 vars = vars0,
                                 type = "left",
                                 by = by0,
                                 suffix = c(".x", ".y"),
                                 na_matches = "never") {
    lazy_join_query(
      x = x,
      y = y,
      vars = vars,
      type = type,
      by = by,
      suffix = suffix,
      na_matches = na_matches
    )
  }

  expect_snapshot(error = TRUE, {
    (my_lazy_join_query(x = lazy_frame(x = 1)))
    (my_lazy_join_query(y = lazy_frame(x = 1)))
  })

  expect_snapshot(error = TRUE, {
    (my_lazy_join_query(vars = "a"))
    (my_lazy_join_query(vars = c(vars0, list(z = 1))))
    (my_lazy_join_query(vars = lmod(vars0, alias = 1)))
    (my_lazy_join_query(vars = lmod(vars0, x = 1)))
    (my_lazy_join_query(vars = lmod(vars0, x = "a")))
    (my_lazy_join_query(vars = lmod(vars0, y = 1)))
    (my_lazy_join_query(vars = lmod(vars0, y = "a")))
    (my_lazy_join_query(vars = lmod(vars0, all_x = 1)))
    (my_lazy_join_query(vars = lmod(vars0, all_y = 1)))
  })

  expect_snapshot(error = TRUE, {
    (my_lazy_join_query(by = "a"))
    (my_lazy_join_query(by = lmod(by0, x = 1)))
    (my_lazy_join_query(by = lmod(by0, y = 1)))
    (my_lazy_join_query(by = lmod(by0, x = c("a", "b"))))
    (my_lazy_join_query(by = lmod(by0, x_as = "a")))
    (my_lazy_join_query(by = lmod(by0, x_as = ident("a", "b"))))
    (my_lazy_join_query(by = lmod(by0, y_as = "a")))
    (my_lazy_join_query(by = lmod(by0, y_as = ident("a", "b"))))
  })

  expect_snapshot(error = TRUE, {
    (my_lazy_join_query(type = "type"))
    (my_lazy_join_query(suffix = "_x"))
    (my_lazy_join_query(suffix = c(1, 2)))
    (my_lazy_join_query(na_matches = "sometimes"))
  })
})

test_that("lazy_semi_join_query() checks arguments", {
  by0 <- list(x = "x", y = "x", x_as = ident("LHS"), y_as = ident("RHS"))
  lmod <- purrr::list_modify

  my_lazy_semi_join_query <- function(x = lazy_frame(x = 1, y = 2)$lazy_query,
                                      y = lazy_frame(x = 1, z = 2)$lazy_query,
                                      anti = FALSE,
                                      by = by0,
                                      na_matches = "never") {
    lazy_semi_join_query(
      x = x,
      y = y,
      vars = tibble(name = "x", var = "x"),
      anti = anti,
      by = by,
      na_matches = na_matches
    )
  }

  expect_snapshot(error = TRUE, {
    (my_lazy_semi_join_query(x = lazy_frame(x = 1)))
    (my_lazy_semi_join_query(y = lazy_frame(x = 1)))
  })

  # expect_snapshot(error = TRUE, {
  #   (my_lazy_semi_join_query(by = lmod(by0, x = 1)))
  # })

  expect_snapshot(error = TRUE, {
    (my_lazy_semi_join_query(anti = NA))
    (my_lazy_semi_join_query(na_matches = "sometimes"))
  })
})
