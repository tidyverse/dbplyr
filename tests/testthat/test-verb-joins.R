test_that("complete join pipeline works with SQLite", {
  df1 <- memdb_frame(x = 1:5)
  df2 <- memdb_frame(x = c(1, 3, 5), y = c("a", "b", "c"))

  out <- collect(left_join(df1, df2, by = "x"))
  expect_equal(out, tibble(x = 1:5, y = c("a", NA, "b", NA, "c")))
})

test_that("complete join pipeline works with SQLite and table alias", {
  df1 <- memdb_frame(x = 1:5)
  df2 <- memdb_frame(x = c(1, 3, 5), y = c("a", "b", "c"))

  out <- left_join(df1, df2, by = "x", x_as = "df1", y_as = "df2")
  expect_equal(out %>% collect(), tibble(x = 1:5, y = c("a", NA, "b", NA, "c")))

  lf1 <- lazy_frame(x = 1:5)
  lf2 <- lazy_frame(x = c(1, 3, 5), y = c("a", "b", "c"))
  expect_snapshot(left_join(lf1, lf2, by = "x", x_as = "df1", y_as = "df2"))
})

test_that("complete semi join works with SQLite", {
  lf1 <- memdb_frame(x = c(1, 2), y = c(2, 3))
  lf2 <- memdb_frame(x = 1)

  lf3 <- inner_join(lf1, lf2, by = "x")
  expect_equal(op_vars(lf3), c("x", "y"))

  out <- collect(lf3)
  expect_equal(out, tibble(x = 1, y = 2))
})

test_that("complete semi join works with SQLite and table alias", {
  df1 <- memdb_frame(x = c(1, 2), y = c(2, 3))
  df2 <- memdb_frame(x = 1)

  out <- inner_join(df1, df2, by = "x", x_as = "df1", y_as = "df2")
  expect_equal(out %>% collect(), tibble(x = 1, y = 2))

  lf1 <- lazy_frame(x = c(1, 2), y = c(2, 3))
  lf2 <- lazy_frame(x = 1)
  expect_snapshot(inner_join(lf1, lf2, by = "x", x_as = "df1", y_as = "df2"))
})

test_that("joins with non by variables gives cross join", {
  df1 <- memdb_frame(x = 1:5)
  df2 <- memdb_frame(y = 1:5)

  out <- collect(inner_join(df1, df2, by = character()))
  expect_equal(nrow(out), 25)

  # full_join() goes through a slightly different path and
  # generates CROSS JOIN for reasons I don't fully understand
  out <- collect(full_join(df1, df2, by = character()))
  expect_equal(nrow(out), 25)
})

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
  j2 <- collect(left_join(df1, df3, by = "x")) %>%
    mutate(x.y = x) %>%
    select(x.x = x, y, x.y, z)
  expect_equal(j1, j2)
})

test_that("inner join doesn't result in duplicated columns ", {
  expect_equal(colnames(inner_join(df1, df1, by = c("x", "y"))), c("x", "y"))
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

  both <- collect(left_join(df1, df2, by = "a"))
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

test_that("join check `x_as` and `y_as`", {
  x <- lazy_frame(x = 1)
  expect_snapshot(error = TRUE, left_join(x, x, by = "x", x_as = NULL))
  expect_snapshot(error = TRUE, left_join(x, x, by = "x", y_as = c("A", "B")))
  expect_snapshot(error = TRUE, left_join(x, x, by = "x", x_as = "LHS", y_as = "LHS"))
})

# sql_build ---------------------------------------------------------------

test_that("join verbs generate expected ops", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)

  ji <- inner_join(lf1, lf2, by = "x")
  expect_s3_class(ji$lazy_query, "lazy_join_query")
  expect_equal(ji$lazy_query$type, "inner")

  jl <- left_join(lf1, lf2, by = "x")
  expect_s3_class(jl$lazy_query, "lazy_join_query")
  expect_equal(jl$lazy_query$type, "left")

  jr <- right_join(lf1, lf2, by = "x")
  expect_s3_class(jr$lazy_query, "lazy_join_query")
  expect_equal(jr$lazy_query$type, "right")

  jf <- full_join(lf1, lf2, by = "x")
  expect_s3_class(jf$lazy_query, "lazy_join_query")
  expect_equal(jf$lazy_query$type, "full")

  js <- semi_join(lf1, lf2, by = "x")
  expect_s3_class(js$lazy_query, "lazy_semi_join_query")
  expect_equal(js$lazy_query$anti, FALSE)

  ja <- anti_join(lf1, lf2, by = "x")
  expect_s3_class(ja$lazy_query, "lazy_semi_join_query")
  expect_equal(ja$lazy_query$anti, TRUE)
})

test_that("can optionally match NA values", {
  lf <- lazy_frame(x = 1)
  expect_snapshot(left_join(lf, lf, by = "x", na_matches = "na"))
})

test_that("join captures both tables", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)

  out <- inner_join(lf1, lf2, by = "x") %>% sql_build()

  expect_s3_class(out, "join_query")
  expect_equal(out$type, "inner")
})

test_that("semi join captures both tables", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)

  out <- semi_join(lf1, lf2, by = "x") %>% sql_build()

  expect_equal(out$anti, FALSE)
})

test_that("set ops captures both tables", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)

  out <- union(lf1, lf2) %>% sql_build()
  expect_equal(out$type, "UNION")
})

test_that("extra args generates error", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)

  expect_error(
    inner_join(lf1, lf2, by = "x", never_used = "na"),
    "unused argument"
  )
})

test_that("suffix arg is checked", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)

  expect_snapshot(
    error = TRUE,
    inner_join(lf1, lf2, by = "x", suffix = "a")
  )
})

test_that("copy = TRUE works", {
  lf <- memdb_frame(x = 1, y = 2)
  df <- tibble(x = 1, z = 3)

  expect_equal(
    inner_join(lf, df, by = "x", copy = TRUE) %>% collect(),
    tibble(x = 1, y = 2, z = 3)
  )
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

