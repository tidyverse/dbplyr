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

test_that("join works with in_schema", {
  withr::local_db_connection(con <- dbConnect(RSQLite::SQLite(), ":memory:"))

  DBI::dbExecute(con, "ATTACH ':memory:' AS foo")
  DBI::dbWriteTable(con, DBI::Id(schema = "foo", table = "df"), tibble(x = 1:3, y = "a"))
  DBI::dbWriteTable(con, DBI::Id(schema = "foo", table = "df2"), tibble(x = 2:3, z = "b"))

  # same schema, different name
  df1 <- tbl(con, in_schema("foo", "df"))
  df2 <- tbl(con, in_schema("foo", "df2"))
  expect_equal(
    left_join(df1, df2, by = "x") %>% collect(),
    tibble(x = 1:3, y = "a", z = c(NA, "b", "b"))
  )
  expect_snapshot(
    left_join(df1, df2, by = "x") %>% remote_query()
  )

  # different schema, same name
  DBI::dbExecute(con, "ATTACH ':memory:' AS foo2")
  DBI::dbWriteTable(con, DBI::Id(schema = "foo2", table = "df"), tibble(x = 2:3, z = "c"))
  df3 <- tbl(con, in_schema("foo2", "df"))
  expect_equal(
    left_join(df1, df3, by = "x") %>% collect(),
    tibble(x = 1:3, y = "a", z = c(NA, "c", "c"))
  )
  expect_snapshot(
    left_join(df1, df3, by = "x") %>% remote_query()
  )
})

test_that("alias truncates long table names at database limit", {
  # Postgres has max identifier length of 63; ensure it's not exceeded when generating table alias
  # Source: https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS
  con <- src_test("postgres")

  nm1 <- paste0("a", paste0(0:61 %% 10, collapse = ""))
  DBI::dbWriteTable(con, nm1, tibble(x = 1:3, y = "a"))
  mf1 <- tbl(con, nm1)

  nm2 <- paste0("b", paste0(0:61 %% 10, collapse = ""))
  DBI::dbWriteTable(con, nm2, tibble(x = 2:3, y = "b"))
  mf2 <- tbl(con, nm2)

  # 2 tables
  # aliased names are as expected
  self_join2_names <- generate_join_table_names(
    tibble::tibble(
      name = c(nm1, nm1),
      from = "name"
    )
  )

  expect_equal(max(nchar(self_join2_names)), 63)
  expect_equal(
    length(self_join2_names),
    length(unique(self_join2_names))
  )

  # joins correctly work
  self_join2 <- left_join(mf1, mf1, by = c("x", "y"))

  expect_equal(
    self_join2 %>% collect(),
    tibble(x = 1:3, y = "a")
  )

  expect_snapshot(
    self_join2 %>% remote_query()
  )

  # 3 tables
  # aliased names are as expected
  self_join3_names <- generate_join_table_names(
    tibble::tibble(
      name = c(nm1, nm1, nm2),
      from = "name"
    )
  )

  expect_equal(max(nchar(self_join3_names)), 63)
  expect_equal(
    length(self_join3_names),
    length(unique(self_join3_names))
  )

  # joins correctly work
  self_join3 <- left_join(mf1, mf1, by = c("x", "y")) %>%
    inner_join(mf2, by = "x")

  expect_equal(
    self_join3 %>% collect(),
    tibble(x = 2:3, y.x = "a", y.y = "b")
  )
  expect_snapshot(
    self_join3 %>% remote_query()
  )
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
})

test_that("join uses correct table alias", {
  x <- lazy_frame(a = 1, x = 1, .name = "x")
  y <- lazy_frame(a = 1, y = 1, .name = "y")

  # self joins
  table_vars <- sql_build(left_join(x, x, by = "a"))$table_vars
  expect_named(table_vars, c("x_LHS", "x_RHS"))

  table_vars <- sql_build(left_join(x, x, by = "a", x_as = "my_x"))$table_vars
  expect_named(table_vars, c("my_x", "x"))

  table_vars <- sql_build(left_join(x, x, by = "a", y_as = "my_y"))$table_vars
  expect_named(table_vars, c("x", "my_y"))

  table_vars <- sql_build(left_join(x, x, by = "a", x_as = "my_x", y_as = "my_y"))$table_vars
  expect_named(table_vars, c("my_x", "my_y"))

  # x-y joins
  table_vars <- sql_build(left_join(x, y, by = "a"))$table_vars
  expect_named(table_vars, c("x", "y"))

  table_vars <- sql_build(left_join(x, y, by = "a", x_as = "my_x"))$table_vars
  expect_named(table_vars, c("my_x", "y"))

  table_vars <- sql_build(left_join(x, y, by = "a", y_as = "my_y"))$table_vars
  expect_named(table_vars, c("x", "my_y"))

  table_vars <- sql_build(left_join(x, y, by = "a", x_as = "my_x", y_as = "my_y"))$table_vars
  expect_named(table_vars, c("my_x", "my_y"))

  # x_as same name as `y`
  table_vars <- sql_build(left_join(x, y, by = "a", x_as = "y"))$table_vars
  expect_named(table_vars, c("y", "y...2"))

  table_vars <- sql_build(left_join(x %>% filter(x == 1), x, by = "x", y_as = "LHS"))$table_vars
  expect_named(table_vars, c("LHS...1", "LHS"))

  # sql_on -> use alias or LHS/RHS
  table_vars <- sql_build(left_join(x, y, sql_on = sql("LHS.a = RHS.a")))$table_vars
  expect_named(table_vars, c("LHS", "RHS"))

  table_vars <- sql_build(left_join(x, y, x_as = "my_x", sql_on = sql("my_x.a = RHS.a")))$table_vars
  expect_named(table_vars, c("my_x", "RHS"))

  # triple join
  z <- lazy_frame(a = 1, z = 1, .name = "z")
  out <- left_join(x, y, by = "a") %>%
    left_join(z, by = "a") %>%
    sql_build()
  expect_named(out$table_vars, c("x", "y", "z"))

  # triple join where names need to be repaired
  out <- left_join(x, x, by = "a") %>%
    left_join(z, by = "a") %>%
    sql_build()
  expect_named(out$table_vars, c("x...1", "x...2", "z"))
})

test_that("select() before join is inlined", {
  lf <- lazy_frame(x1 = 10, a = 1, y = 3, .name = "lf1")
  lf2 <- lazy_frame(x2 = 10, b = 2, z = 4, .name = "lf2")

  test_vars <- function(lq, var, table) {
    expect_equal(lq$vars$name, c("a2", "x", "b"))
    expect_equal(lq$vars$var, var)
    expect_equal(lq$vars$table, table)

    expect_equal(lq$joins$by[[1]]$x, ident("x1"))
    expect_equal(lq$joins$by[[1]]$y, ident("x2"))
  }

  out_left <- left_join(
    lf %>% select(a2 = a, x = x1),
    lf2 %>% select(x = x2, b),
    by = "x"
  )
  test_vars(out_left$lazy_query, list("a", "x1", "b"), list(1, 1, 2))
  expect_equal(op_vars(out_left), c("a2", "x", "b"))
  expect_snapshot(out_left)

  out_inner <- inner_join(
    lf %>% select(a2 = a, x = x1),
    lf2 %>% select(x = x2, b),
    by = "x"
  )
  test_vars(out_inner$lazy_query, list("a", "x1", "b"), list(1, 1, 2))

  out_right <- right_join(
    lf %>% select(a2 = a, x = x1),
    lf2 %>% select(x = x2, b),
    by = "x"
  )
  test_vars(out_right$lazy_query, list("a", "x2", "b"), list(1, 2, 2))

  out_full <- full_join(
    lf %>% select(a2 = a, x = x1),
    lf2 %>% select(x = x2, b),
    by = "x"
  )
  vars <- out_full$lazy_query$vars
  expect_equal(vars$name, c("a2", "x", "b"))
  expect_equal(vars$var, list("a", c("x1", "x2"), "b"))
  expect_equal(vars$table, list(1, c(1, 2), 2))

  out_cross <- full_join(
    lf %>% select(a2 = a, x = x1),
    lf2 %>% select(x = x2, b),
    by = character()
  )
  vars <- out_cross$lazy_query$vars
  expect_equal(vars$name, c("a2", "x.x", "x.y", "b"))
  expect_equal(vars$var, list("a", "x1", "x2", "b"))
  expect_equal(vars$table, list(1, 1, 2, 2))

  # attributes like `group`, `sort`, `frame` is kept
  lf <- lazy_frame(x = 10, a = 1, b = 1, .name = "lf1")
  lf2 <- lazy_frame(x = 10, .name = "lf2")
  out_left <- left_join(
    lf %>%
      group_by(a) %>%
      arrange(a) %>%
      window_frame(0, 1) %>%
      select(x, a),
    lf2,
    by = "x"
  )
  expect_equal(op_grps(out_left), "a")
  expect_equal(op_sort(out_left), list(quo(a)), ignore_formula_env = TRUE)
  expect_equal(op_frame(out_left), list(range = c(0, 1)))
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
  expect_equal(lq$vars$var, list("id", "x", "id"))
  expect_equal(lq$vars$table, list(1, 1, 2))
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
  expect_equal(lq$vars$var, list("id_x", "x"))
  expect_equal(lq$vars$table, list(1, 1))
  expect_equal(lq$joins$by[[1]]$x, ident(c("id_x", "x")))
  expect_equal(lq$joins$by[[1]]$y, ident(c("id_y", "x")))
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
  expect_equal(lq$vars$var, list("id", "x", "x"))
  expect_equal(lq$vars$table, list(1L, 1L, 2L))
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
  expect_s3_class(lq$joins$table[[1]], "lazy_select_query")
  expect_equal(lq$vars$var, list("a2", "x", "x", "b"))
  expect_equal(lq$vars$table, list(1L, 1L, 2L, 2L))
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
  expect_equal(lq$vars, tibble(name = c("a2", "x"), var = c("a", "x1")))
  expect_equal(lq$by$x, "x1")
  expect_snapshot(out_semi)

  out_anti <- anti_join(
    lf %>% select(a2 = a, x = x1),
    lf2 %>% select(x = x2, b),
    by = "x"
  )
  lq <- out_anti$lazy_query
  expect_equal(lq$vars, tibble(name = c("a2", "x"), var = c("a", "x1")))
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
  expect_equal(lq$vars, tibble(name = c("a2", "x"), var = c("a2", "x")))
})

test_that("multiple joins create a single query", {
  lf <- lazy_frame(x = 1, a = 1, .name = "df1")
  lf2 <- lazy_frame(x = 1, b = 2, .name = "df2")
  lf3 <- lazy_frame(x = 1, b = 2, .name = "df3")

  out <- left_join(lf, lf2, by = "x") %>%
    inner_join(lf3, by = "x")
  lq <- out$lazy_query
  expect_s3_class(lq, "lazy_multi_join_query")
  expect_equal(lq$table_names, tibble(name = c("df1", "df2", "df3"), from = "name"))
  expect_equal(lq$vars$name, c("x", "a", "b.x", "b.y"))
  expect_equal(lq$vars$table, list(1L, 1L, 2L, 3L))
  expect_equal(lq$vars$var, list("x", "a", "b", "b"))

  expect_snapshot(out)
})

test_that("can join 4 tables with same column #1101", {
  # when there are
  # * at least 3 joins
  # * a variable appears in two table
  # * it is not joined by
  # * and it is renamed
  #
  # this produced a wrong query
  lf1 <- lazy_frame(x = 1, a = 2, .name = "lf1")
  lf2 <- lazy_frame(x = 2, b = 3, .name = "lf2")
  lf3 <- lazy_frame(x = 3, c = 4, .name = "lf3")
  lf4 <- lazy_frame(x = 4, a = 5, .name = "lf4") %>%
    rename(a4 = a)

  out <- lf1 %>%
    inner_join(lf2, by = "x") %>%
    inner_join(lf3, by = "x") %>%
    inner_join(lf4, by = "x")

  join_vars <- out$lazy_query$vars
  expect_equal(join_vars$name, c("x", "a", "b", "c", "a4"))
  expect_equal(join_vars$table, list(1L, 1L, 2L, 3L, 4L))
  expect_equal(join_vars$var, list("x", "a", "b", "c", "a"))
  # `lf4`.`a` AS `a4`
  expect_snapshot(remote_query(out))
})

test_that("multiple joins produce separate queries if using right/full join", {
  lf <- lazy_frame(x = 1, a = 1, .name = "df1")
  lf2 <- lazy_frame(x = 1, b = 2, .name = "df2")
  lf3 <- lazy_frame(x = 1, b = 2, .name = "df3")

  out <- left_join(lf, lf2, by = "x") %>%
    right_join(lf3, by = "x")

  out_build <- out %>% sql_build()
  expect_s3_class(out_build, "join_query")
  expect_s3_class(out_build$x, "multi_join_query")

  expect_snapshot(remote_query(out))

  out_build <- left_join(lf, lf2, by = "x") %>%
    full_join(lf3, by = "x") %>%
    sql_build()

  expect_s3_class(out_build, "join_query")
  expect_s3_class(out_build$x, "multi_join_query")
})

test_that("multiple joins can use by column from any table", {
  lf <- lazy_frame(x = 1, a = 1, .name = "df1")
  lf2 <- lazy_frame(x = 1, y = 2, .name = "df2")
  lf3 <- lazy_frame(x = 1, y = 2, b = 2, .name = "df3")

  out <- left_join(lf, lf2, by = "x") %>%
    left_join(lf3, by = c("x", "y"))
  joins_metadata <- out$lazy_query$joins
  expect_equal(joins_metadata$by[[1]]$x, ident("x"))
  expect_equal(joins_metadata$by[[2]]$x, ident(c("x", "y")))
  expect_equal(joins_metadata$by[[1]]$y, ident("x"))
  expect_equal(joins_metadata$by[[2]]$y, ident(c("x", "y")))
  expect_equal(joins_metadata$by_x_table_id, list(1, c(1, 2)))

  lf <- lazy_frame(x = 1, a = 1, .name = "df1")
  lf2 <- lazy_frame(x = 1, y2 = 2, .name = "df2")
  lf3 <- lazy_frame(x = 1, y = 2, b = 2, .name = "df3")

  out <- left_join(
    lf,
    lf2 %>% rename(y = y2),
    by = "x"
  ) %>%
    left_join(lf3, by = c("x", "y"))

  joins_metadata <- out$lazy_query$joins
  expect_equal(joins_metadata$by[[1]]$x, ident("x"))
  expect_equal(joins_metadata$by[[2]]$x, ident(c("x", "y2")))
  expect_equal(joins_metadata$by[[1]]$y, ident("x"))
  expect_equal(joins_metadata$by[[2]]$y, ident(c("x", "y")))
  expect_equal(joins_metadata$by_x_table_id, list(1, c(1, 2)))
})

test_that("multi joins work with x_as", {
  lf <- lazy_frame(x = 1, a = 1, .name = "df1")
  lf2 <- lazy_frame(x = 1, b = 2, .name = "df2")
  lf3 <- lazy_frame(x = 1, b = 2, .name = "df3")

  out <- left_join(lf, lf2, by = "x", x_as = "lf1", y_as = "lf2") %>%
    inner_join(lf3, by = "x", y_as = "lf3")
  lq <- out$lazy_query
  expect_s3_class(lq, "lazy_multi_join_query")
  expect_equal(lq$table_names, tibble(name = c("lf1", "lf2", "lf3"), from = "as"))

  # `x_as` provided twice with the same name -> one query
  out2 <- left_join(lf, lf2, by = "x", x_as = "lf1", y_as = "lf2") %>%
    inner_join(lf3, by = "x", x_as = "lf1", y_as = "lf3")
  expect_equal(out, out2)

  # `x_as` provided twice with different names -> two queries
  lq <- left_join(lf, lf2, by = "x", x_as = "lf1") %>%
    inner_join(lf3, by = "x", x_as = "lf2") %>%
    .$lazy_query
  expect_s3_class(lq, "lazy_multi_join_query")
  expect_s3_class(lq$x, "lazy_multi_join_query")

  # `x_as` name already used
  lq <- left_join(lf, lf2, by = "x", y_as = "lf2") %>%
    inner_join(lf3, by = "x", x_as = "lf2") %>%
    .$lazy_query
  expect_s3_class(lq, "lazy_multi_join_query")
  expect_s3_class(lq$x, "lazy_multi_join_query")

  # `y_as` name already used
  lq <- left_join(lf, lf2, by = "x", y_as = "lf2") %>%
    inner_join(lf3, by = "x", y_as = "lf2") %>%
    .$lazy_query
  expect_s3_class(lq, "lazy_multi_join_query")
  expect_s3_class(lq$x, "lazy_multi_join_query")

  lq <- left_join(lf, lf2, by = "x", x_as = "lf2") %>%
    inner_join(lf3, by = "x", y_as = "lf2") %>%
    .$lazy_query
  expect_s3_class(lq, "lazy_multi_join_query")
  expect_s3_class(lq$x, "lazy_multi_join_query")
})

# sql_build ---------------------------------------------------------------

test_that("join verbs generate expected ops", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)

  ji <- inner_join(lf1, lf2, by = "x")
  expect_s3_class(ji$lazy_query, "lazy_multi_join_query")
  expect_equal(ji$lazy_query$joins$type, "inner")

  jl <- left_join(lf1, lf2, by = "x")
  expect_s3_class(jl$lazy_query, "lazy_multi_join_query")
  expect_equal(jl$lazy_query$joins$type, "left")

  jr <- right_join(lf1, lf2, by = "x")
  expect_s3_class(jr$lazy_query, "lazy_multi_join_query")
  expect_equal(jr$lazy_query$joins$type, "right")

  jf <- full_join(lf1, lf2, by = "x")
  expect_s3_class(jf$lazy_query, "lazy_multi_join_query")
  expect_equal(jf$lazy_query$joins$type, "full")

  js <- semi_join(lf1, lf2, by = "x")
  expect_s3_class(js$lazy_query, "lazy_semi_join_query")
  expect_equal(js$lazy_query$anti, FALSE)

  ja <- anti_join(lf1, lf2, by = "x")
  expect_s3_class(ja$lazy_query, "lazy_semi_join_query")
  expect_equal(ja$lazy_query$anti, TRUE)
})

test_that("can optionally match NA values", {
  con <- simulate_postgres()
  lf1 <- lazy_frame(x = 1, .name = "lf1", con = con)
  lf2 <- lazy_frame(x = 1, .name = "lf2", con = con)
  expect_snapshot(left_join(lf1, lf2, by = "x", na_matches = "na"))
})

test_that("join captures both tables", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)

  out <- inner_join(lf1, lf2, by = "x") %>% sql_build()

  expect_s3_class(out, "multi_join_query")
  expect_equal(out$joins$type, "inner")
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
  skip_if(getRversion() < "4.0.0")
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)

  expect_error(
    inner_join(lf1, lf2, by = "x", never_used = "na"),
    "used"
  )
})

test_that("suffix arg is checked", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)

  expect_snapshot({
    (expect_error(inner_join(lf1, lf2, by = "x", suffix = "a")))
    (expect_error(inner_join(lf1, lf2, by = "x", suffix = 1L)))
  })
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
    left_join(lf2, by = c("a", "b")) %>%
    sql_build()

  expect_equal(
    sql_multi_join_vars(con, out$vars, out$table_vars),
    sql("`df_LHS`.*", z = "`z`")
  )

  # also works after relocate
  out <- lf1 %>%
    left_join(lf2, by = c("a", "b")) %>%
    relocate(z) %>%
    sql_build()

  expect_equal(
    sql_multi_join_vars(con, out$vars, out$table_vars),
    sql(z = "`z`", "`df_LHS`.*")
  )

  # does not use * if variable are missing
  out <- lf1 %>%
    left_join(lf2, by = c("a", "b")) %>%
    select(a, c) %>%
    sql_build()

  expect_equal(
    sql_multi_join_vars(con, out$vars, out$table_vars),
    sql(a = "`df_LHS`.`a`", c = "`c`")
  )

  # does not use * if variable names changed
  lf1 <- lazy_frame(a = 1, b = 2)
  lf2 <- lazy_frame(a = 1, b = 2)
  out <- lf1 %>%
    left_join(lf2, by = c("a")) %>%
    sql_build()

  expect_equal(
    sql_multi_join_vars(con, out$vars, out$table_vars),
    sql(a = "`df_LHS`.`a`", `b.x` = "`df_LHS`.`b`", `b.y` = "`df_RHS`.`b`")
  )
})

test_that("right_join uses *", {
  lf1 <- lazy_frame(a = 1, b = 2, c = 1)
  lf2 <- lazy_frame(a = 1, b = 2, z = 1)

  con <- simulate_dbi()
  out <- lf1 %>%
    right_join(lf2, by = c("a", "b")) %>%
    sql_build()

  # cannot use * without relocate or select
  expect_equal(
    sql_join_vars(con, out$vars, type = "right", x_as = out$by$x_as, y_as = out$by$y_as),
    sql(a = "`df_RHS`.`a`", b = "`df_RHS`.`b`", c = "`c`", z = "`z`")
  )

  # also works after relocate
  out <- lf1 %>%
    right_join(lf2, by = c("a", "b")) %>%
    select(a, b, z, c) %>%
    sql_build()

  expect_equal(
    sql_join_vars(con, out$vars, type = "right", x_as = out$by$x_as, y_as = out$by$y_as),
    sql("`df_RHS`.*", c = "`c`")
  )

  # does not use * if variable are missing
  out <- lf1 %>%
    right_join(lf2, by = c("a", "b")) %>%
    select(a, z) %>%
    sql_build()

  expect_equal(
    sql_join_vars(con, out$vars, type = "right", x_as = out$by$x_as, y_as = out$by$y_as),
    sql(a = "`df_RHS`.`a`", z = "`z`")
  )

  # does not use * if variable names changed
  lf1 <- lazy_frame(a = 1, b = 2)
  lf2 <- lazy_frame(a = 1, b = 2)
  out <- lf1 %>%
    right_join(lf2, by = c("a")) %>%
    sql_build()

  expect_equal(
    sql_join_vars(con, out$vars, type = "right", x_as = out$by$x_as, y_as = out$by$y_as),
    sql(a = "`df_RHS`.`a`", `b.x` = "`df_LHS`.`b`", `b.y` = "`df_RHS`.`b`")
  )
})

test_that("cross_join uses *", {
  lf1 <- lazy_frame(a = 1, b = 1)
  lf2 <- lazy_frame(x = 1, y = 1)

  con <- simulate_dbi()
  out <- lf1 %>%
    full_join(lf2, by = character()) %>%
    sql_build()

  expect_equal(
    sql_multi_join_vars(con, out$vars, out$table_vars),
    set_names(sql("`df_LHS`.*", "`df_RHS`.*"), c("", ""))
  )

  # also works after relocate
  out <- lf1 %>%
    full_join(lf2, by = character()) %>%
    select(x, y, a, b) %>%
    sql_build()

  expect_equal(
    sql_multi_join_vars(con, out$vars, out$table_vars),
    set_names(sql("`df_RHS`.*", "`df_LHS`.*"), c("", ""))
  )

  out <- lf1 %>%
    full_join(lf2, by = character()) %>%
    select(x, a, b, y) %>%
    sql_build()

  expect_equal(
    sql_multi_join_vars(con, out$vars, out$table_vars),
    sql(x = "`x`", "`df_LHS`.*", y = "`y`")
  )

  out <- lf1 %>%
    full_join(lf2, by = character()) %>%
    select(a, x, y, b) %>%
    sql_build()

  expect_equal(
    sql_multi_join_vars(con, out$vars, out$table_vars),
    sql(a = "`a`", "`df_RHS`.*", b = "`b`")
  )
})

test_that("full_join() does not use *", {
  con <- simulate_dbi()
  lf1 <- lazy_frame(a = 1, b = 2)
  lf2 <- lazy_frame(a = 1, b = 2)

  out <- lf1 %>%
    full_join(lf2, by = c("a", "b")) %>%
    sql_build()

  expect_equal(
    sql_join_vars(con, out$vars, type = "full", x_as = out$by$x_as, y_as = out$by$y_as),
    sql(
      a = "COALESCE(`df_LHS`.`a`, `df_RHS`.`a`)",
      b = "COALESCE(`df_LHS`.`b`, `df_RHS`.`b`)"
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
