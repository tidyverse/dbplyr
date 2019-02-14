context("test-joins.R")

df1 <- memdb_frame(x = 1:5, y = 1:5)
df2 <- memdb_frame(a = 5:1, b = 1:5)
df3 <- memdb_frame(x = 1:5, z = 1:5)
df4 <- memdb_frame(a = 5:1, z = 5:1)

test_that("named by join by different x and y vars", {
  j1 <- collect(inner_join(df1, df2, c("x" = "a")))
  expect_equal(names(j1), c("x", "y", "b"))
  expect_equal(nrow(j1), 5)

  j2 <- collect(inner_join(df1, df2, c("x" = "a", "y" = "b")))
  expect_equal(names(j2), c("x", "y"))
  expect_equal(nrow(j2), 1)
})

test_that("named by join by same z vars", {
  j1 <- collect(inner_join(df3, df4, c("z" = "z")))
  expect_equal(nrow(j1), 5)
  expect_equal(names(j1), c("x", "z", "a"))
})

test_that("join with both same and different vars", {
  j1 <- collect(left_join(df1, df3, by = c("y" = "z", "x")))
  expect_equal(nrow(j1), 5)
  expect_equal(names(j1), c("x", "y"))
})

test_that("joining over arbitrary predicates", {
  j1 <- collect(left_join(df1, df2, sql_on = "LHS.x = RHS.b"))
  j2 <- collect(left_join(df1, df2, by = c("x" = "b"))) %>% mutate(b = x)
  expect_equal(j1, j2)

  j1 <- collect(left_join(df1, df3, sql_on = "LHS.x = RHS.z"))
  j2 <- collect(left_join(df1, df3, by = c("x" = "z"))) %>% mutate(z = x.x)
  expect_equal(j1, j2)

  j1 <- collect(left_join(df1, df3, sql_on = "LHS.x = RHS.x"))
  j2 <- collect(left_join(df1, df3, by = "x")) %>% mutate(x.y = x) %>% rename(x.x = x)
  expect_equal(j1, j2)
})

test_that("inner join doesn't result in duplicated columns ", {
  expect_equal(colnames(inner_join(df1, df1)), c("x", "y"))
})

test_that("self-joins allowed with named by", {
  fam <- memdb_frame(id = 1:5, parent = c(NA, 1, 2, 2, 4))

  j1 <- fam %>% left_join(fam, by = c("parent" = "id"))
  j2 <- fam %>% inner_join(fam, by = c("parent" = "id"))

  expect_equal(op_vars(j1), c("id", "parent.x", "parent.y"))
  expect_equal(op_vars(j2), c("id", "parent.x", "parent.y"))
  expect_equal(nrow(collect(j1)), 5)
  expect_equal(nrow(collect(j2)), 4)

  j3 <- collect(semi_join(fam, fam, by = c("parent" = "id")))
  j4 <- collect(anti_join(fam, fam, by = c("parent" = "id")))

  expect_equal(j3, filter(collect(fam), !is.na(parent)))
  expect_equal(j4, filter(collect(fam), is.na(parent)))
})

test_that("suffix modifies duplicated variable names", {
  fam <- memdb_frame(id = 1:5, parent = c(NA, 1, 2, 2, 4))
  j1 <- collect(inner_join(fam, fam, by = c("parent" = "id"), suffix = c("1", "2")))
  j2 <- collect(left_join(fam, fam, by = c("parent" = "id"), suffix = c("1", "2")))

  expect_named(j1, c("id", "parent1", "parent2"))
  expect_named(j2, c("id", "parent1", "parent2"))
})

test_that("join variables always disambiguated (#2823)", {
  # Even if the new variable conflicts with an existing variable
  df1 <- dbplyr::memdb_frame(a = 1, b.x = 1, b = 1)
  df2 <- dbplyr::memdb_frame(a = 1, b = 1)

  both <- collect(dplyr::left_join(df1, df2, by = "a"))
  expect_named(both, c("a", "b.x", "b.x.x", "b.y"))
})

test_that("join functions error on column not found for SQL sources #1928", {
  # Rely on dplyr to test precise code
  expect_error(
    left_join(memdb_frame(x = 1:5), memdb_frame(y = 1:5), by = "x"),
    "missing|(not found)"
  )
  expect_error(
    left_join(memdb_frame(x = 1:5), memdb_frame(y = 1:5), by = "y"),
    "missing|(not found)"
  )
  expect_error(
    left_join(memdb_frame(x = 1:5), memdb_frame(y = 1:5)),
    "[Nn]o common variables"
  )
})

test_that("join generates correct sql", {
  lf1 <- memdb_frame(x = 1, y = 2)
  lf2 <- memdb_frame(x = 1, z = 3)

  out <- lf1 %>%
    inner_join(lf2, by = "x") %>%
    collect()

  expect_equal(out, data.frame(x = 1, y = 2, z = 3))
})

test_that("semi join generates correct sql", {
  lf1 <- memdb_frame(x = c(1, 2), y = c(2, 3))
  lf2 <- memdb_frame(x = 1)

  lf3 <- inner_join(lf1, lf2, by = "x")
  expect_equal(op_vars(lf3), c("x", "y"))

  out <- collect(lf3)
  expect_equal(out, data.frame(x = 1, y = 2))
})


test_that("set ops generates correct sql", {
  lf1 <- memdb_frame(x = 1)
  lf2 <- memdb_frame(x = c(1, 2))

  out <- lf1 %>%
    union(lf2) %>%
    collect()

  expect_equal(out, data.frame(x = c(1, 2)))
})
# All sources -------------------------------------------------------------

test_that("sql generated correctly for all sources", {
  x <- test_frame(a = letters[1:7], c = 2:8)
  y <- test_frame(a = letters[1:4], b = c(1, 2, 3, NA))
  xy <- purrr::map2(x, y, left_join)

  expect_equal_tbls(xy)
})

test_that("full join is promoted to cross join for no overlapping variables", {
  result <- df1 %>% full_join(df2, by = character()) %>% collect()
  expect_equal(nrow(result), 25)
})


# Consistency of results --------------------------------------------------

test_that("consistent result of left join on key column with same name in both tables", {
  test_l_j_by_x <- function(tbl_left, tbl_right) {
    left_join(tbl_left, tbl_right, by = "x") %>% arrange(x, y, z)
  }

  tbl_left <- tibble(x = 1L:4L, y = 1L:4L)
  tbl_right <- tibble(x = c(1L:3L, 5L), z = 1L:4L)

  tbls_left <- test_load(tbl_left)
  tbls_right <- test_load(tbl_right)

  compare_tbls2(tbls_left, tbls_right, op = test_l_j_by_x)
})

test_that("consistent result of inner join on key column with same name in both tables", {
  test_i_j_by_x <- function(tbl_left, tbl_right) {
    inner_join(tbl_left, tbl_right, by = "x") %>% arrange(x, y, z)
  }

  tbl_left <- tibble(x = 1L:4L, y = 1L:4L)
  tbl_right <- tibble(x = c(1L:3L, 5L), z = 1L:4L)

  tbls_left <- test_load(tbl_left)
  tbls_right <- test_load(tbl_right)

  compare_tbls2(tbls_left, tbls_right, op = test_i_j_by_x)
})

test_that("consistent result of right join on key column with same name in both tables", {
  test_r_j_by_x <- function(tbl_left, tbl_right) {
    right_join(tbl_left, tbl_right, by = "x") %>% arrange(x, y, z)
  }

  tbl_left <- tibble(x = 1L:4L, y = 1L:4L)
  tbl_right <- tibble(x = c(1L:3L, 5L), z = 1L:4L)

  # SQLite does not support right joins
  tbls_left <- test_load(tbl_left, ignore = c("sqlite"))
  tbls_right <- test_load(tbl_right, ignore = c("sqlite"))

  compare_tbls2(tbls_left, tbls_right, op = test_r_j_by_x)
})

test_that("consistent result of full join on key column with same name in both tables", {
  test_f_j_by_x <- function(tbl_left, tbl_right) {
    full_join(tbl_left, tbl_right, by = "x") %>% arrange(x, y, z)
  }

  tbl_left <- tibble(x = 1L:4L, y = 1L:4L)
  tbl_right <- tibble(x = c(1L:3L, 5L), z = 1L:4L)

  # SQLite and MySQL do not support full joins
  tbls_left <- test_load(tbl_left, ignore = c("sqlite", "mysql", "MariaDB"))
  tbls_right <- test_load(tbl_right, ignore = c("sqlite", "mysql", "MariaDB"))

  compare_tbls2(tbls_left, tbls_right, op = test_f_j_by_x)
})

test_that("consistent result of left join on key column with different names", {
  test_l_j_by_xl_xr <- function(tbl_left, tbl_right) {
    left_join(tbl_left, tbl_right, by = c("xl" = "xr")) %>% arrange(xl, y, z)
  }

  tbl_left <- tibble(xl = 1L:4L, y = 1L:4L)
  tbl_right <- tibble(xr = c(1L:3L, 5L), z = 1L:4L)

  tbls_left <- test_load(tbl_left)
  tbls_right <- test_load(tbl_right)

  compare_tbls2(tbls_left, tbls_right, op = test_l_j_by_xl_xr)
})

test_that("consistent result of inner join on key column with different names", {
  test_i_j_by_xl_xr <- function(tbl_left, tbl_right) {
    inner_join(tbl_left, tbl_right, by = c("xl" = "xr")) %>% arrange(xl, y, z)
  }

  tbl_left <- tibble(xl = 1L:4L, y = 1L:4L)
  tbl_right <- tibble(xr = c(1L:3L, 5L), z = 1L:4L)

  tbls_left <- test_load(tbl_left)
  tbls_right <- test_load(tbl_right)

  compare_tbls2(tbls_left, tbls_right, op = test_i_j_by_xl_xr)
})

test_that("consistent result of right join on key column with different names", {
  test_r_j_by_xl_xr <- function(tbl_left, tbl_right) {
    right_join(tbl_left, tbl_right, by = c("xl" = "xr")) %>% arrange(xl, y, z)
  }

  tbl_left <- tibble(xl = 1L:4L, y = 1L:4L)
  tbl_right <- tibble(xr = c(1L:3L, 5L), z = 1L:4L)

  # SQLite does not support right joins
  tbls_left <- test_load(tbl_left, ignore = c("sqlite"))
  tbls_right <- test_load(tbl_right, ignore = c("sqlite"))

  compare_tbls2(tbls_left, tbls_right, op = test_r_j_by_xl_xr)
})

test_that("consistent result of full join on key column with different names", {
  test_f_j_by_xl_xr <- function(tbl_left, tbl_right) {
    full_join(tbl_left, tbl_right, by = c("xl" = "xr")) %>% arrange(xl, y, z)
  }

  tbl_left <- tibble(xl = 1L:4L, y = 1L:4L)
  tbl_right <- tibble(xr = c(1L:3L, 5L), z = 1L:4L)

  # SQLite and MySQL do not support full joins
  tbls_left <- test_load(tbl_left, ignore = c("sqlite", "mysql", "MariaDB"))
  tbls_right <- test_load(tbl_right, ignore = c("sqlite", "mysql", "MariaDB"))

  compare_tbls2(tbls_left, tbls_right, op = test_f_j_by_xl_xr)
})

test_that("consistent result of left natural join", {
  test_l_j <- function(tbl_left, tbl_right) {
    left_join(tbl_left, tbl_right) %>% arrange(x, y, z, w)
  }

  tbl_left <- tibble(x = 1L:4L, y = 1L:4L, w = 1L:4L)
  tbl_right <- tibble(x = c(1L:3L, 5L), y = 1L:4L, z = 1L:4L)

  tbls_left <- test_load(tbl_left)
  tbls_right <- test_load(tbl_right)

  compare_tbls2(tbls_left, tbls_right, op = test_l_j)
})

test_that("consistent result of inner natural join", {
  test_i_j <- function(tbl_left, tbl_right) {
    inner_join(tbl_left, tbl_right) %>% arrange(x, y, z, w)
  }

  tbl_left <- tibble(x = 1L:4L, y = 1L:4L, w = 1L:4L)
  tbl_right <- tibble(x = c(1L:3L, 5L), y = 1L:4L, z = 1L:4L)

  tbls_left <- test_load(tbl_left)
  tbls_right <- test_load(tbl_right)

  compare_tbls2(tbls_left, tbls_right, op = test_i_j)
})

test_that("consistent result of right natural join", {
  test_r_j <- function(tbl_left, tbl_right) {
    right_join(tbl_left, tbl_right) %>% arrange(x, y, z, w)
  }

  tbl_left <- tibble(x = 1L:4L, y = 1L:4L, w = 1L:4L)
  tbl_right <- tibble(x = c(1L:3L, 5L), y = 1L:4L, z = 1L:4L)

  # SQLite does not support right joins
  tbls_left <- test_load(tbl_left, ignore = c("sqlite"))
  tbls_right <- test_load(tbl_right, ignore = c("sqlite"))

  compare_tbls2(tbls_left, tbls_right, op = test_r_j)
})

test_that("consistent result of full natural join", {
  test_f_j <- function(tbl_left, tbl_right) {
    full_join(tbl_left, tbl_right) %>% arrange(x, y, z, w)
  }

  tbl_left <- tibble(x = 1L:4L, y = 1L:4L, w = 1L:4L)
  tbl_right <- tibble(x = c(1L:3L, 5L), y = 1L:4L, z = 1L:4L)

  # SQLite and MySQL do not support full joins
  tbls_left <- test_load(tbl_left, ignore = c("sqlite", "mysql", "MariaDB"))
  tbls_right <- test_load(tbl_right, ignore = c("sqlite", "mysql", "MariaDB"))

  compare_tbls2(tbls_left, tbls_right, op = test_f_j)
})


# sql_build ---------------------------------------------------------------


test_that("join captures both tables", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)

  out <- inner_join(lf1, lf2) %>% sql_build()

  expect_s3_class(out, "join_query")
  expect_equal(op_vars(out$x), c("x", "y"))
  expect_equal(op_vars(out$y), c("x", "z"))
  expect_equal(out$type, "inner")
})

test_that("semi join captures both tables", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)

  out <- semi_join(lf1, lf2) %>% sql_build()

  expect_equal(op_vars(out$x), c("x", "y"))
  expect_equal(op_vars(out$y), c("x", "z"))
  expect_equal(out$anti, FALSE)
})

test_that("set ops captures both tables", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)

  out <- union(lf1, lf2) %>% sql_build()
  expect_equal(out$type, "UNION")
})


# ops ---------------------------------------------------------------------

test_that("joins get vars from both left and right", {
  out <- left_join(
    lazy_frame(x = 1, y = 1),
    lazy_frame(x = 2, z = 2),
    by = "x"
  )

  expect_equal(op_vars(out), c("x", "y", "z"))
})


test_that("semi joins get vars from left", {
  out <- semi_join(
    lazy_frame(x = 1, y = 1),
    lazy_frame(x = 2, z = 2),
    by = "x"
  )

  expect_equal(op_vars(out), c("x", "y"))
})

# Helpers -----------------------------------------------------------------

test_that("add_suffixes works if no suffix requested", {
  expect_equal(add_suffixes(c("x", "x"), "y", ""), c("x", "x"))
  expect_equal(add_suffixes(c("x", "y"), "y", ""), c("x", "y"))
})

