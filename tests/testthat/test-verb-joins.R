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

  lf1 <- lazy_frame(x = 1:5, .name = "lf1")
  lf2 <- lazy_frame(x = c(1, 3, 5), y = c("a", "b", "c"), .name = "lf2")
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
  j1 <- collect(left_join(df1, df2, x_as = "LHS", y_as = "RHS", sql_on = "LHS.x = RHS.b"))
  j2 <- collect(left_join(df1, df2, by = c("x" = "b"))) %>% mutate(b = x)
  expect_equal(j1, j2)

  j1 <- collect(left_join(df1, df3, x_as = "LHS", y_as = "RHS", sql_on = "LHS.x = RHS.z"))
  j2 <- collect(left_join(df1, df3, by = c("x" = "z"))) %>% mutate(z = x.x)
  expect_equal(j1, j2)

  j1 <- collect(left_join(df1, df3, x_as = "LHS", y_as = "RHS", sql_on = "LHS.x = RHS.x"))
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
  x <- lazy_frame(a = 1, x = 1, .name = "x")
  y <- lazy_frame(a = 1, y = 1, .name = "y")

  expect_snapshot(error = TRUE, left_join(x, x, by = "x", y_as = c("A", "B")))
  expect_snapshot(error = TRUE, left_join(x, x, by = "x", x_as = "LHS", y_as = "LHS"))

  expect_snapshot(error = TRUE, left_join(x, y, by = "a", x_as = "y"))
  expect_snapshot(error = TRUE, {
    left_join(
      x %>% filter(x == 1),
      x,
      by = "x",
      y_as = "LHS"
    )
  })
})

test_that("join uses correct table alias", {
  x <- lazy_frame(a = 1, x = 1, .name = "x")
  y <- lazy_frame(a = 1, y = 1, .name = "y")

  by <- left_join(x, x, by = "a")$lazy_query$by
  expect_equal(by$x_as, ident("x_LHS"))
  expect_equal(by$y_as, ident("x_RHS"))

  by <- left_join(x, x, by = "a", x_as = "my_x")$lazy_query$by
  expect_equal(by$x_as, ident("my_x"))
  expect_equal(by$y_as, ident("x"))

  by <- left_join(x, x, by = "a", y_as = "my_y")$lazy_query$by
  expect_equal(by$x_as, ident("x"))
  expect_equal(by$y_as, ident("my_y"))

  by <- left_join(x, x, by = "a", x_as = "my_x", y_as = "my_y")$lazy_query$by
  expect_equal(by$x_as, ident("my_x"))
  expect_equal(by$y_as, ident("my_y"))


  by <- left_join(x, y, by = "a")$lazy_query$by
  expect_equal(by$x_as, ident("x"))
  expect_equal(by$y_as, ident("y"))

  by <- left_join(x, y, by = "a", x_as = "my_x")$lazy_query$by
  expect_equal(by$x_as, ident("my_x"))
  expect_equal(by$y_as, ident("y"))

  by <- left_join(x, y, by = "a", y_as = "my_y")$lazy_query$by
  expect_equal(by$x_as, ident("x"))
  expect_equal(by$y_as, ident("my_y"))

  by <- left_join(x, y, by = "a", x_as = "my_x", y_as = "my_y")$lazy_query$by
  expect_equal(by$x_as, ident("my_x"))
  expect_equal(by$y_as, ident("my_y"))

  by <- left_join(x, y, sql_on = sql("LHS.a = RHS.a"))$lazy_query$by
  expect_equal(by$x_as, ident("LHS"))
  expect_equal(by$y_as, ident("RHS"))

  by <- left_join(x, y, x_as = "my_x", sql_on = sql("my_x.a = RHS.a"))$lazy_query$by
  expect_equal(by$x_as, ident("my_x"))
  expect_equal(by$y_as, ident("RHS"))
})

test_that("select() before join is inlined", {
  lf <- lazy_frame(x1 = 10, a = 1, y = 3, .name = "lf1")
  lf2 <- lazy_frame(x2 = 10, b = 2, z = 4, .name = "lf2")

  test_vars <- function(lq, x, y) {
    expect_equal(lq$vars$alias, c("a2", "x", "b"))
    expect_equal(lq$vars$x, x)
    expect_equal(lq$vars$y, y)
    expect_equal(lq$vars$all_x, c("x1", "a", "y"))
    expect_equal(lq$vars$all_y, c("x2", "b", "z"))

    expect_equal(lq$by$x, "x1")
    expect_equal(lq$by$y, "x2")
  }

  out_left <- left_join(
    lf %>% select(a2 = a, x = x1),
    lf2 %>% select(x = x2, b),
    by = "x"
  )
  test_vars(out_left$lazy_query, c("a", "x1", NA), c(NA, NA, "b"))
  expect_equal(op_vars(out_left), c("a2", "x", "b"))
  expect_snapshot(out_left)

  out_inner <- inner_join(
    lf %>% select(a2 = a, x = x1),
    lf2 %>% select(x = x2, b),
    by = "x"
  )
  test_vars(out_inner$lazy_query, c("a", "x1", NA), c(NA, NA, "b"))

  out_right <- right_join(
    lf %>% select(a2 = a, x = x1),
    lf2 %>% select(x = x2, b),
    by = "x"
  )
  test_vars(out_right$lazy_query, c("a", NA, NA), c(NA, "x2", "b"))

  out_full <- full_join(
    lf %>% select(a2 = a, x = x1),
    lf2 %>% select(x = x2, b),
    by = "x"
  )
  test_vars(out_full$lazy_query, c("a", "x1", NA), c(NA, "x2", "b"))

  out_cross <- full_join(
    lf %>% select(a2 = a, x = x1),
    lf2 %>% select(x = x2, b),
    by = character()
  )
  vars <- out_cross$lazy_query$vars
  expect_equal(vars$alias, c("a2", "x.x", "x.y", "b"))
  expect_equal(vars$x, c("a", "x1", NA, NA))
  expect_equal(vars$y, c(NA, NA, "x2", "b"))
  expect_equal(vars$all_x, c("x1", "a", "y"))
  expect_equal(vars$all_y, c("x2", "b", "z"))
})

test_that("select() before join works for tables with same column name", {
  lf <- lazy_frame(id = 1, x = 1, .name = "lf1")
  lf2 <- lazy_frame(id = 12, x = 2, .name = "lf2")

  out <- left_join(
    lf %>% rename(id1 = id),
    lf2 %>% rename(id2 = id),
    by = "x"
  )

  lq <- out$lazy_query
  expect_equal(op_vars(lq), c("id1", "x", "id2"))
  expect_equal(lq$vars$x, c("id", "x", NA))
  expect_equal(lq$vars$y, c(NA, NA, "id"))
})

test_that("named by works in combination with inlined select", {
  lf <- lazy_frame(id_x = 1, x = 1, .name = "lf1")
  lf2 <- lazy_frame(id_y = 12, x = 2, .name = "lf2")

  out <- left_join(
    lf %>% select(id_x, x.x = x),
    lf2 %>% select(id_y, x.y = x),
    by = c(id_x = "id_y", x.x = "x.y")
  )

  lq <- out$lazy_query
  expect_equal(op_vars(lq), c("id_x", "x.x"))
  expect_equal(lq$vars$x, c("id_x", "x"))
  expect_equal(lq$vars$y, c(NA_character_, NA_character_))
  expect_equal(lq$by$x, c("id_x", "x"))
  expect_equal(lq$by$y, c("id_y", "x"))
})

test_that("suffix works in combination with inlined select", {
  lf <- lazy_frame(id = 1, x = 1, .name = "lf1")
  lf2 <- lazy_frame(id = 12, x = 2, .name = "lf2")

  out <- left_join(
    lf %>% rename(x2 = x),
    lf2 %>% rename(x2 = x),
    by = "id"
  )

  lq <- out$lazy_query
  expect_equal(op_vars(lq), c("id", "x2.x", "x2.y"))
  expect_equal(lq$vars$x, c("id", "x", NA))
  expect_equal(lq$vars$y, c(NA, NA, "x"))
})

test_that("select() before join is not inlined when using `sql_on`", {
  lf <- lazy_frame(x1 = 10, a = 1, y = 3, .name = "lf1")
  lf2 <- lazy_frame(x2 = 10, b = 2, z = 4, .name = "lf2")

  out <- left_join(
    lf %>% select(a2 = a, x = x1),
    lf2 %>% select(x = x2, b),
    sql_on = sql("LHS.x = RHS.x")
  )

  lq <- out$lazy_query
  expect_s3_class(lq$x, "lazy_select_query")
  expect_s3_class(lq$y, "lazy_select_query")
  expect_equal(lq$vars$x, c("a2", "x", NA, NA))
  expect_equal(lq$vars$y, c(NA, NA, "x", "b"))
})

test_that("select() before semi_join is inlined", {
  lf <- lazy_frame(x1 = 10, a = 1, y = 3, .name = "lf1")
  lf2 <- lazy_frame(x2 = 10, b = 2, z = 4, .name = "lf2")

  out_semi <- semi_join(
    lf %>% select(a2 = a, x = x1),
    lf2 %>% select(x = x2, b),
    by = "x"
  )
  lq <- out_semi$lazy_query
  expect_equal(op_vars(out_semi), c("a2", "x"))
  expect_equal(lq$vars, c(a2 = "a", x = "x1"))
  expect_equal(lq$by$x, "x1")
  expect_snapshot(out_semi)

  out_anti <- anti_join(
    lf %>% select(a2 = a, x = x1),
    lf2 %>% select(x = x2, b),
    by = "x"
  )
  lq <- out_anti$lazy_query
  expect_equal(lq$vars, c(a2 = "a", x = "x1"))
  expect_equal(lq$by$x, "x1")

  # attributes like `group`, `sort`, `frame` is kept
  lf <- lazy_frame(x = 10, a = 1, b = 1, .name = "lf1")
  lf2 <- lazy_frame(x = 10, .name = "lf2")
  out_semi <- semi_join(
    lf %>%
      group_by(a) %>%
      arrange(a) %>%
      window_frame(0, 1) %>%
      select(x, a),
    lf2,
    by = "x"
  )
  expect_equal(op_grps(out_semi), "a")
  expect_equal(op_sort(out_semi), list(quo(a)), ignore_formula_env = TRUE)
  expect_equal(op_frame(out_semi), list(range = c(0, 1)))
})

test_that("select() before join is not inlined when using `sql_on`", {
  lf <- lazy_frame(x1 = 10, a = 1, y = 3, .name = "lf1")
  lf2 <- lazy_frame(x2 = 10, b = 2, z = 4, .name = "lf2")

  out <- semi_join(
    lf %>% select(a2 = a, x = x1),
    lf2 %>% select(x = x2, b),
    sql_on = sql("LHS.x = RHS.x")
  )

  lq <- out$lazy_query
  expect_s3_class(lq$x, "lazy_select_query")
  expect_equal(lq$vars, c(a2 = "a2", x = "x"))
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

test_that("left_join/inner_join uses *", {
  lf1 <- lazy_frame(a = 1, b = 2, c = 1)
  lf2 <- lazy_frame(a = 1, b = 2, z = 1)

  con <- simulate_dbi()
  out <- lf1 %>%
    left_join(lf2, by = c("a", "b"))

  expect_equal(
    sql_join_vars(con, out$lazy_query$vars, type = "left"),
    sql("`LHS`.*", z = "`z`")
  )

  # also works after relocate
  out <- lf1 %>%
    left_join(lf2, by = c("a", "b")) %>%
    relocate(z)

  expect_equal(
    sql_join_vars(con, out$lazy_query$vars, type = "left"),
    sql(z = "`z`", "`LHS`.*")
  )

  # does not use * if variable are missing
  out <- lf1 %>%
    left_join(lf2, by = c("a", "b")) %>%
    select(a, c)

  expect_equal(
    sql_join_vars(con, out$lazy_query$vars, type = "left"),
    sql(a = "`LHS`.`a`", c = "`c`")
  )

  # does not use * if variable names changed
  lf1 <- lazy_frame(a = 1, b = 2)
  lf2 <- lazy_frame(a = 1, b = 2)
  out <- lf1 %>%
    left_join(lf2, by = c("a"))

  expect_equal(
    sql_join_vars(con, out$lazy_query$vars, type = "left"),
    sql(a = "`LHS`.`a`", `b.x` = "`LHS`.`b`", `b.y` = "`RHS`.`b`")
  )
})

test_that("right_join uses *", {
  lf1 <- lazy_frame(a = 1, b = 2, c = 1)
  lf2 <- lazy_frame(a = 1, b = 2, z = 1)

  con <- simulate_dbi()
  out <- lf1 %>%
    right_join(lf2, by = c("a", "b"))

  # cannot use * without relocate or select
  expect_equal(
    sql_join_vars(con, out$lazy_query$vars, type = "right"),
    sql(a = "`RHS`.`a`", b = "`RHS`.`b`", c = "`c`", z = "`z`")
  )

  # also works after relocate
  out <- lf1 %>%
    right_join(lf2, by = c("a", "b")) %>%
    select(a, b, z, c)

  expect_equal(
    sql_join_vars(con, out$lazy_query$vars, type = "right"),
    sql("`RHS`.*", c = "`c`")
  )

  # does not use * if variable are missing
  out <- lf1 %>%
    right_join(lf2, by = c("a", "b")) %>%
    select(a, z)

  expect_equal(
    sql_join_vars(con, out$lazy_query$vars, type = "right"),
    sql(a = "`RHS`.`a`", z = "`z`")
  )

  # does not use * if variable names changed
  lf1 <- lazy_frame(a = 1, b = 2)
  lf2 <- lazy_frame(a = 1, b = 2)
  out <- lf1 %>%
    right_join(lf2, by = c("a"))

  expect_equal(
    sql_join_vars(con, out$lazy_query$vars, type = "right"),
    sql(a = "`RHS`.`a`", `b.x` = "`LHS`.`b`", `b.y` = "`RHS`.`b`")
  )
})

test_that("cross_join uses *", {
  lf1 <- lazy_frame(a = 1, b = 1)
  lf2 <- lazy_frame(x = 1, y = 1)

  con <- simulate_dbi()
  out <- lf1 %>%
    full_join(lf2, by = character())

  expect_equal(
    sql_join_vars(con, out$lazy_query$vars, type = "cross"),
    set_names(sql("`LHS`.*", "`RHS`.*"), c("", ""))
  )

  # also works after relocate
  out <- lf1 %>%
    full_join(lf2, by = character()) %>%
    select(x, y, a, b)

  expect_equal(
    sql_join_vars(con, out$lazy_query$vars, type = "cross"),
    set_names(sql("`RHS`.*", "`LHS`.*"), c("", ""))
  )

  out <- lf1 %>%
    full_join(lf2, by = character()) %>%
    select(x, a, b, y)

  expect_equal(
    sql_join_vars(con, out$lazy_query$vars, type = "cross"),
    sql(x = "`x`", "`LHS`.*", y = "`y`")
  )

  out <- lf1 %>%
    full_join(lf2, by = character()) %>%
    select(a, x, y, b)

  expect_equal(
    sql_join_vars(con, out$lazy_query$vars, type = "cross"),
    sql(a = "`a`", "`RHS`.*", b = "`b`")
  )
})

test_that("full_join() does not use *", {
  con <- simulate_dbi()
  lf1 <- lazy_frame(a = 1, b = 2)
  lf2 <- lazy_frame(a = 1, b = 2)

  out <- lf1 %>%
    full_join(lf2, by = c("a", "b"))

  expect_equal(
    sql_join_vars(con, out$lazy_query$vars, type = "full"),
    sql(
      a = "COALESCE(`LHS`.`a`, `RHS`.`a`)",
      b = "COALESCE(`LHS`.`b`, `RHS`.`b`)"
    )
  )
})

test_that("joins reuse queries in cte mode", {
  lf1 <- lazy_frame(x = 1, .name = "lf1")
  lf <- lf1 %>%
    inner_join(lf1, by = "x")

  expect_snapshot(
    left_join(
      lf,
      lf
    ) %>%
      remote_query(cte = TRUE)
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

