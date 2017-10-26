context("joins")

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

# All sources -------------------------------------------------------------

test_that("sql generated correctly for all sources", {
  x <- test_frame(a = letters[1:7], c = 2:8)
  y <- test_frame(a = letters[1:4], b = c(1, 2, 3, NA))
  xy <- map2(x, y, left_join)

  expect_equal_tbls(xy)
})

test_that("full join is promoted to cross join for no overlapping variables", {
  result <- df1 %>% full_join(df2, by = character()) %>% collect()
  expect_equal(nrow(result), 25)
})
