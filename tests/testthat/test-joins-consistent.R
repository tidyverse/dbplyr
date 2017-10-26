context("SQL: consistent join results")

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

  tbl_left <- data_frame(x = 1L:4L, y = 1L:4L, w = 1L:4L)
  tbl_right <- data_frame(x = c(1L:3L, 5L), y = 1L:4L, z = 1L:4L)

  # SQLite and MySQL do not support full joins
  tbls_left <- test_load(tbl_left, ignore = c("sqlite", "mysql", "MariaDB"))
  tbls_right <- test_load(tbl_right, ignore = c("sqlite", "mysql", "MariaDB"))

  compare_tbls2(tbls_left, tbls_right, op = test_f_j)
})
