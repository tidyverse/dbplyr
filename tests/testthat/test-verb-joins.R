test_that("complete join pipeline works with SQLite", {
  df1 <- memdb_frame(x = 1:5)
  df2 <- memdb_frame(x = c(1, 3, 5), y = c("a", "b", "c"))

  out <- collect(left_join(df1, df2, by = "x"))
  expect_equal(out, tibble(x = 1:5, y = c("a", NA, "b", NA, "c")))

  df1 <- memdb_frame(x = 1:3, y = c("x", "y", "z"))
  df2 <- memdb_frame(x = c(1, 3, 5), y = c("a", "b", "c"), z = 11:13)

  expect_equal(
    collect(full_join(df1, df2, by = "x")),
    tibble(
      x = c(1, 2, 3, 5),
      y.x = c("x", "y", "z", NA),
      y.y = c("a", NA, "b", "c"),
      z = c(11, NA, 12, 13)
    )
  )
})

test_that("complete join pipeline works with SQLite and table alias", {
  df1 <- memdb_frame(x = 1:5)
  df2 <- memdb_frame(x = c(1, 3, 5), y = c("a", "b", "c"))

  out <- left_join(df1, df2, by = "x", x_as = "df1", y_as = "df2")
  expect_equal(out |> collect(), tibble(x = 1:5, y = c("a", NA, "b", NA, "c")))

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
  expect_equal(out |> collect(), tibble(x = 1, y = 2))

  lf1 <- lazy_frame(x = c(1, 2), y = c(2, 3))
  lf2 <- lazy_frame(x = 1)
  expect_snapshot(inner_join(lf1, lf2, by = "x", x_as = "df1", y_as = "df2"))
})

test_that("join works with in_schema", {
  con <- local_sqlite_connection()

  DBI::dbExecute(con, "ATTACH ':memory:' AS foo")
  local_db_table(
    con,
    tibble(x = 1:3, y = "a"),
    DBI::Id(schema = "foo", table = "df"),
    temporary = FALSE
  )
  local_db_table(
    con,
    tibble(x = 2:3, z = "b"),
    DBI::Id(schema = "foo", table = "df2"),
    temporary = FALSE
  )

  # same schema, different name
  df1 <- tbl(con, in_schema("foo", "df"))
  df2 <- tbl(con, in_schema("foo", "df2"))
  expect_equal(
    left_join(df1, df2, by = "x") |> collect(),
    tibble(x = 1:3, y = "a", z = c(NA, "b", "b"))
  )
  expect_snapshot(
    left_join(df1, df2, by = "x") |> remote_query()
  )

  # different schema, same name
  DBI::dbExecute(con, "ATTACH ':memory:' AS foo2")
  local_db_table(
    con,
    tibble(x = 2:3, z = "c"),
    DBI::Id(schema = "foo2", table = "df"),
    temporary = FALSE
  )
  df3 <- tbl(con, in_schema("foo2", "df"))
  expect_equal(
    left_join(df1, df3, by = "x") |> collect(),
    tibble(x = 1:3, y = "a", z = c(NA, "c", "c"))
  )
  expect_snapshot(
    left_join(df1, df3, by = "x") |> remote_query()
  )

  # use auto name for escaped table names
  df4 <- tbl(con, I("foo.df"))
  df5 <- tbl(con, I("foo2.df"))
  expect_snapshot(
    left_join(df4, df5, by = "x") |> remote_query()
  )
  out <- left_join(df4, df5, by = "x")
  expect_equal(
    out$lazy_query$table_names,
    tibble(
      name = table_path(c("foo.df", "foo2.df")),
      from = c("name", "name")
    )
  )
})

test_that("alias truncates long table names at database limit", {
  # Postgres has max identifier length of 63; ensure it's not exceeded when generating table alias
  # Source: https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS
  con <- src_test("postgres")

  nm1 <- table_path(paste0("a", paste0(0:61 %% 10, collapse = "")))
  mf1 <- local_db_table(con, tibble(x = 1:3, y = "a"), unclass(nm1))

  nm2 <- table_path(paste0("b", paste0(0:61 %% 10, collapse = "")))
  mf2 <- local_db_table(con, tibble(x = 2:3, y = "b"), unclass(nm2))

  # 2 tables
  # aliased names are as expected
  self_join2_names <- generate_join_table_names(
    tibble::tibble(
      name = c(nm1, nm1),
      from = "name"
    ),
    con
  )
  expect_equal(max(nchar(self_join2_names)), 65)
  expect_equal(
    length(self_join2_names),
    length(unique(self_join2_names))
  )

  # joins correctly work
  self_join2 <- left_join(mf1, mf1, by = c("x", "y"))

  expect_equal(
    self_join2 |> collect(),
    tibble(x = 1:3, y = "a")
  )

  expect_snapshot(
    self_join2 |> remote_query()
  )

  # 3 tables
  # aliased names are as expected
  self_join3_names <- generate_join_table_names(
    tibble::tibble(
      name = c(nm1, nm1, nm2),
      from = "name"
    ),
    con
  )

  expect_equal(max(nchar(self_join3_names)), 65)
  expect_equal(
    length(self_join3_names),
    length(unique(self_join3_names))
  )

  # joins correctly work
  self_join3 <- left_join(mf1, mf1, by = c("x", "y")) |>
    inner_join(mf2, by = "x")

  expect_equal(
    self_join3 |> collect(),
    tibble(x = 2:3, y.x = "a", y.y = "b")
  )
  expect_snapshot(
    self_join3 |> remote_query()
  )
})

test_that("cross join via by = character() is deprecated", {
  df1 <- memdb_frame(x = 1:5)
  df2 <- memdb_frame(y = 1:5)

  expect_snapshot({
    out_inner <- collect(inner_join(df1, df2, by = character()))
    out_full <- collect(full_join(df1, df2, by = character()))
  })

  expect_equal(nrow(out_inner), 25)
  expect_equal(nrow(out_full), 25)
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
  j1 <- collect(left_join(
    df1,
    df2,
    x_as = "LHS",
    y_as = "RHS",
    sql_on = "LHS.x = RHS.b"
  ))
  j2 <- collect(left_join(df1, df2, by = c("x" = "b"))) |> mutate(b = x)
  expect_equal(j1, j2)

  j1 <- collect(left_join(
    df1,
    df3,
    x_as = "LHS",
    y_as = "RHS",
    sql_on = "LHS.x = RHS.z"
  ))
  j2 <- collect(left_join(df1, df3, by = c("x" = "z"))) |>
    mutate(z = x) |>
    rename(x.x = x)
  expect_equal(j1, j2)

  j1 <- collect(left_join(
    df1,
    df3,
    x_as = "LHS",
    y_as = "RHS",
    sql_on = "LHS.x = RHS.x"
  ))
  j2 <- collect(left_join(df1, df3, by = "x")) |>
    mutate(x.y = x) |>
    select(x.x = x, y, x.y, z)
  expect_equal(j1, j2)
})

test_that("inner join doesn't result in duplicated columns ", {
  expect_equal(colnames(inner_join(df1, df1, by = c("x", "y"))), c("x", "y"))
})

test_that("self-joins allowed with named by", {
  fam <- memdb_frame(id = 1:5, parent = c(NA, 1, 2, 2, 4))

  j1 <- fam |> left_join(fam, by = c("parent" = "id"))
  j2 <- fam |> inner_join(fam, by = c("parent" = "id"))

  expect_equal(op_vars(j1), c("id", "parent", "parent.y"))
  expect_equal(op_vars(j2), c("id", "parent", "parent.y"))
  expect_equal(nrow(collect(j1)), 5)
  expect_equal(nrow(collect(j2)), 4)

  j3 <- collect(semi_join(fam, fam, by = c("parent" = "id")))
  j4 <- collect(anti_join(fam, fam, by = c("parent" = "id")))

  expect_equal(j3, filter(collect(fam), !is.na(parent)))
  expect_equal(j4, filter(collect(fam), is.na(parent)))
})

test_that("suffix modifies duplicated variable names", {
  fam <- memdb_frame(id = 1:5, parent = c(NA, 1, 2, 2, 4))
  j1 <- collect(inner_join(
    fam,
    fam,
    by = c("parent" = "id"),
    suffix = c("1", "2")
  ))
  j2 <- collect(left_join(
    fam,
    fam,
    by = c("parent" = "id"),
    suffix = c("1", "2")
  ))

  expect_named(j1, c("id", "parent", "parent2"))
  expect_named(j2, c("id", "parent", "parent2"))
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
    "present"
  )
  expect_error(
    left_join(memdb_frame(x = 1:5), memdb_frame(y = 1:5), by = "y"),
    "present"
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
  expect_snapshot(
    error = TRUE,
    left_join(x, x, by = "x", x_as = "LHS", y_as = "LHS")
  )
})

test_that("join uses correct table alias", {
  x <- lazy_frame(a = 1, x = 1, .name = "x")
  y <- lazy_frame(a = 1, y = 1, .name = "y")

  # self joins
  table_paths <- sql_build(left_join(x, x, by = "a"))$table_names
  expect_equal(table_paths, table_path(c("`x_LHS`", "`x_RHS`")))

  table_paths <- sql_build(left_join(x, x, by = "a", x_as = "my_x"))$table_names
  expect_equal(table_paths, table_path(c("`my_x`", "`x`")))

  table_paths <- sql_build(left_join(x, x, by = "a", y_as = "my_y"))$table_names
  expect_equal(table_paths, table_path(c("`x`", "`my_y`")))

  table_paths <- sql_build(left_join(
    x,
    x,
    by = "a",
    x_as = "my_x",
    y_as = "my_y"
  ))$table_names
  expect_equal(table_paths, table_path(c("`my_x`", "`my_y`")))

  # x-y joins
  table_paths <- sql_build(left_join(x, y, by = "a"))$table_names
  expect_equal(table_paths, table_path(c("`x`", "`y`")))

  table_paths <- sql_build(left_join(x, y, by = "a", x_as = "my_x"))$table_names
  expect_equal(table_paths, table_path(c("`my_x`", "`y`")))

  table_paths <- sql_build(left_join(x, y, by = "a", y_as = "my_y"))$table_names
  expect_equal(table_paths, table_path(c("`x`", "`my_y`")))

  table_paths <- sql_build(left_join(
    x,
    y,
    by = "a",
    x_as = "my_x",
    y_as = "my_y"
  ))$table_names
  expect_equal(table_paths, table_path(c("`my_x`", "`my_y`")))

  # x_as same name as `y`
  table_paths <- sql_build(left_join(x, y, by = "a", x_as = "y"))$table_names
  expect_equal(table_paths, table_path(c("`y`", "`y...2`")))

  table_paths <- sql_build(left_join(
    x |> filter(x == 1),
    x,
    by = "x",
    y_as = "LHS"
  ))$table_names
  expect_equal(table_paths, table_path(c("`LHS...1`", "`LHS`")))

  # sql_on -> use alias or LHS/RHS
  table_paths <- sql_build(left_join(
    x,
    y,
    sql_on = sql("LHS.a = RHS.a")
  ))$table_names
  expect_equal(table_paths, table_path(c("`LHS`", "`RHS`")))

  table_paths <- sql_build(left_join(
    x,
    y,
    x_as = "my_x",
    sql_on = sql("my_x.a = RHS.a")
  ))$table_names
  expect_equal(table_paths, table_path(c("`my_x`", "`RHS`")))

  # triple join
  z <- lazy_frame(a = 1, z = 1, .name = "z")
  out <- left_join(x, y, by = "a") |>
    left_join(z, by = "a") |>
    sql_build()
  expect_equal(out$table_names, table_path(c("`x`", "`y`", "`z`")))

  # triple join where names need to be repaired
  out <- left_join(x, x, by = "a") |>
    left_join(z, by = "a") |>
    sql_build()
  expect_equal(out$table_names, table_path(c("`x...1`", "`x...2`", "`z`")))
})

test_that("select() before join is inlined", {
  lf <- lazy_frame(x1 = 10, a = 1, y = 3, .name = "lf1")
  lf2 <- lazy_frame(x2 = 10, b = 2, z = 4, .name = "lf2")

  test_vars <- function(lq, var, table) {
    expect_equal(lq$vars$name, c("a2", "x", "b"))
    expect_equal(lq$vars$var, var)
    expect_equal(lq$vars$table, table)

    expect_equal(lq$joins$by[[1]]$x, "x1")
    expect_equal(lq$joins$by[[1]]$y, "x2")
  }

  out_left <- left_join(
    lf |> select(a2 = a, x = x1),
    lf2 |> select(x = x2, b),
    by = "x"
  )
  test_vars(out_left$lazy_query, c("a", "x1", "b"), c(1, 1, 2))
  expect_equal(op_vars(out_left), c("a2", "x", "b"))
  expect_snapshot(out_left)

  out_inner <- inner_join(
    lf |> select(a2 = a, x = x1),
    lf2 |> select(x = x2, b),
    by = "x"
  )
  test_vars(out_inner$lazy_query, c("a", "x1", "b"), c(1, 1, 2))

  out_right <- right_join(
    lf |> select(a2 = a, x = x1),
    lf2 |> select(x = x2, b),
    by = "x"
  )$lazy_query

  expect_equal(out_right$vars$name, c("a2", "x", "b"))
  expect_equal(out_right$vars$x, c("a", NA, NA))
  expect_equal(out_right$vars$y, c(NA, "x2", "b"))
  expect_equal(out_right$by$x, "x1")
  expect_equal(out_right$by$y, "x2")

  out_full <- full_join(
    lf |> select(a2 = a, x = x1),
    lf2 |> select(x = x2, b),
    by = "x"
  )
  vars <- out_full$lazy_query$vars
  expect_equal(vars$name, c("a2", "x", "b"))
  expect_equal(vars$x, c("a", "x1", NA))
  expect_equal(vars$y, c(NA, "x2", "b"))

  out_cross <- cross_join(
    lf |> select(a2 = a, x = x1),
    lf2 |> select(x = x2, b)
  )
  vars <- out_cross$lazy_query$vars
  expect_equal(vars$name, c("a2", "x.x", "x.y", "b"))
  expect_equal(vars$var, c("a", "x1", "x2", "b"))
  expect_equal(vars$table, c(1, 1, 2, 2))

  # attributes like `group`, `sort`, `frame` is kept
  lf <- lazy_frame(x = 10, a = 1, b = 1, .name = "lf1")
  lf2 <- lazy_frame(x = 10, .name = "lf2")
  out_left <- left_join(
    lf |>
      group_by(a) |>
      arrange(a) |>
      window_frame(0, 1) |>
      select(x, a),
    lf2,
    by = "x"
  )
  expect_equal(op_grps(out_left), "a")
  expect_equal(op_sort(out_left), list(expr(a)))
  expect_equal(op_frame(out_left), list(range = c(0, 1)))
})

test_that("select() before join works for tables with same column name", {
  lf <- lazy_frame(id = 1, x = 1, .name = "lf1")
  lf2 <- lazy_frame(id = 12, x = 2, .name = "lf2")

  out <- left_join(
    lf |> rename(id1 = id),
    lf2 |> rename(id2 = id),
    by = "x"
  )

  lq <- out$lazy_query
  expect_equal(op_vars(lq), c("id1", "x", "id2"))
  expect_equal(lq$vars$var, c("id", "x", "id"))
  expect_equal(lq$vars$table, c(1, 1, 2))
})

test_that("named by works in combination with inlined select", {
  lf <- lazy_frame(id_x = 1, x = 1, .name = "lf1")
  lf2 <- lazy_frame(id_y = 12, x = 2, .name = "lf2")

  out <- left_join(
    lf |> select(id_x, x.x = x),
    lf2 |> select(id_y, x.y = x),
    by = c(id_x = "id_y", x.x = "x.y")
  )

  lq <- out$lazy_query
  expect_equal(op_vars(lq), c("id_x", "x.x"))
  expect_equal(lq$vars$var, c("id_x", "x"))
  expect_equal(lq$vars$table, c(1, 1))
  expect_equal(lq$joins$by[[1]]$x, c("id_x", "x"))
  expect_equal(lq$joins$by[[1]]$y, c("id_y", "x"))
})

test_that("suffix works in combination with inlined select", {
  lf <- lazy_frame(id = 1, x = 1, .name = "lf1")
  lf2 <- lazy_frame(id = 12, x = 2, .name = "lf2")

  out <- left_join(
    lf |> rename(x2 = x),
    lf2 |> rename(x2 = x),
    by = "id"
  )

  lq <- out$lazy_query
  expect_equal(op_vars(lq), c("id", "x2.x", "x2.y"))
  expect_equal(lq$vars$var, c("id", "x", "x"))
  expect_equal(lq$vars$table, c(1L, 1L, 2L))
})

test_that("select() before join is not inlined when using `sql_on`", {
  lf <- lazy_frame(x1 = 10, a = 1, y = 3, .name = "lf1")
  lf2 <- lazy_frame(x2 = 10, b = 2, z = 4, .name = "lf2")

  out <- left_join(
    lf |> select(a2 = a, x = x1),
    lf2 |> select(x = x2, b),
    sql_on = sql("LHS.x = RHS.x")
  )

  lq <- out$lazy_query
  expect_s3_class(lq$x, "lazy_select_query")
  expect_s3_class(lq$joins$table[[1]], "lazy_select_query")
  expect_equal(lq$vars$var, c("a2", "x", "x", "b"))
  expect_equal(lq$vars$table, c(1L, 1L, 2L, 2L))
})

test_that("select() before semi_join is inlined", {
  lf <- lazy_frame(x1 = 10, a = 1, y = 3, .name = "lf1")
  lf2 <- lazy_frame(x2 = 10, b = 2, z = 4, .name = "lf2")

  out_semi <- semi_join(
    lf |> select(a2 = a, x = x1),
    lf2 |> select(x = x2, b),
    by = "x"
  )
  lq <- out_semi$lazy_query
  expect_equal(op_vars(out_semi), c("a2", "x"))
  expect_equal(lq$vars, tibble(name = c("a2", "x"), var = c("a", "x1")))
  expect_equal(lq$by$x, "x1")
  expect_snapshot(out_semi)

  out_anti <- anti_join(
    lf |> select(a2 = a, x = x1),
    lf2 |> select(x = x2, b),
    by = "x"
  )
  lq <- out_anti$lazy_query
  expect_equal(lq$vars, tibble(name = c("a2", "x"), var = c("a", "x1")))
  expect_equal(lq$by$x, "x1")

  # attributes like `group`, `sort`, `frame` is kept
  lf <- lazy_frame(x = 10, a = 1, b = 1, .name = "lf1")
  lf2 <- lazy_frame(x = 10, .name = "lf2")
  out_semi <- semi_join(
    lf |>
      group_by(a) |>
      arrange(a) |>
      window_frame(0, 1) |>
      select(x, a),
    lf2,
    by = "x"
  )
  expect_equal(op_grps(out_semi), "a")
  expect_equal(op_sort(out_semi), list(expr(a)))
  expect_equal(op_frame(out_semi), list(range = c(0, 1)))
})

test_that("can combine full_join with other joins #1178", {
  lf1 <- lazy_frame(x = 1)
  lf2 <- lazy_frame(x = 1, y = 1)
  lf3 <- lazy_frame(x = 1, z = 1)

  # left join after full join
  expect_snapshot(
    full_join(lf1, lf2, by = "x") |>
      left_join(lf3, by = "x")
  )
  # full join after left join
  expect_snapshot(
    left_join(lf1, lf2, by = "x") |>
      full_join(lf3, by = "x")
  )
  # full join after full join
  expect_snapshot(
    full_join(lf1, lf2, by = "x") |>
      full_join(lf3, by = "x")
  )
})

test_that("select() before join is not inlined when using `sql_on`", {
  lf <- lazy_frame(x1 = 10, a = 1, y = 3, .name = "lf1")
  lf2 <- lazy_frame(x2 = 10, b = 2, z = 4, .name = "lf2")

  out <- semi_join(
    lf |> select(a2 = a, x = x1),
    lf2 |> select(x = x2, b),
    sql_on = sql("LHS.x = RHS.x")
  )

  lq <- out$lazy_query
  expect_s3_class(lq$x, "lazy_select_query")
  expect_equal(lq$vars, tibble(name = c("a2", "x"), var = c("a2", "x")))
})

test_that("filter() before semi join is inlined", {
  lf <- lazy_frame(x = 1, y = 2, z = 3)
  lf2 <- lazy_frame(x2 = 1, a = 2, b = 3, c = 4)

  out <- semi_join(
    lf,
    lf2 |>
      select(x = x2, a2 = a, b) |>
      filter(a2 == 1L, b == 2L),
    by = "x"
  )
  lq <- out$lazy_query
  expect_equal(
    lq$where,
    list(quo(a == 1L), quo(b == 2L)),
    ignore_formula_env = TRUE
  )
  expect_snapshot(out)
})

test_that("filter() before semi join is not when y has other operations", {
  lf <- lazy_frame(x = 1, y = 2, z = 3)
  lf2 <- lazy_frame(x = 1, a = 2, b = 3)

  out <- semi_join(
    lf,
    lf2 |>
      filter(a == 1L, b == 2L) |>
      mutate(a = a + 1),
    by = "x"
  )
  lq <- out$lazy_query
  expect_null(lq$where)

  out <- semi_join(
    lf,
    lf2 |>
      filter(a == 1L, b == 2L) |>
      distinct(),
    by = "x"
  )
  lq <- out$lazy_query
  expect_null(lq$where)

  out <- semi_join(
    lf,
    lf2 |>
      filter(a == 1L, b == 2L) |>
      head(10),
    by = "x"
  )
  lq <- out$lazy_query
  expect_null(lq$where)
})

test_that("filtered aggregates with subsequent select are not inlined away in semi_join (#1474)", {
  lf <- lazy_frame(x = 1, y = 2, z = 3)
  lf2 <- lazy_frame(x = 1, a = 2, b = 3)

  out <- semi_join(
    lf,
    lf2 |>
      dplyr::summarize(n = n(), .by = "x") |>
      filter(n == 1) |>
      select(x)
  )
  lq <- out$lazy_query

  expect_equal(lq$y$having, list(quo(n() == 1)), ignore_formula_env = TRUE)
  expect_snapshot(out)
})

test_that("multiple joins create a single query", {
  lf <- lazy_frame(x = 1, a = 1, .name = "df1")
  lf2 <- lazy_frame(x = 1, b = 2, .name = "df2")
  lf3 <- lazy_frame(x = 1, b = 2, .name = "df3")

  out <- left_join(lf, lf2, by = "x") |>
    inner_join(lf3, by = "x")
  lq <- out$lazy_query
  expect_s3_class(lq, "lazy_multi_join_query")
  expect_equal(
    lq$table_names,
    tibble(
      name = table_path(c("`df1`", "`df2`", "`df3`")),
      from = "name"
    )
  )
  expect_equal(lq$vars$name, c("x", "a", "b.x", "b.y"))
  expect_equal(lq$vars$table, c(1L, 1L, 2L, 3L))
  expect_equal(lq$vars$var, c("x", "a", "b", "b"))

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
  lf4 <- lazy_frame(x = 4, a = 5, .name = "lf4") |>
    rename(a4 = a)

  out <- lf1 |>
    inner_join(lf2, by = "x") |>
    inner_join(lf3, by = "x") |>
    inner_join(lf4, by = "x")

  join_vars <- out$lazy_query$vars
  expect_equal(join_vars$name, c("x", "a", "b", "c", "a4"))
  expect_equal(join_vars$table, c(1L, 1L, 2L, 3L, 4L))
  expect_equal(join_vars$var, c("x", "a", "b", "c", "a"))
  # `lf4`.`a` AS `a4`
  expect_snapshot(remote_query(out))
})

test_that("multiple joins produce separate queries if using right/full join", {
  lf <- lazy_frame(x = 1, a = 1, .name = "df1")
  lf2 <- lazy_frame(x = 1, b = 2, .name = "df2")
  lf3 <- lazy_frame(x = 1, b = 2, .name = "df3")

  out <- left_join(lf, lf2, by = "x") |>
    right_join(lf3, by = "x")

  out_build <- out |> sql_build()
  expect_s3_class(out_build, "join_query")
  expect_s3_class(out_build$x, "multi_join_query")

  expect_snapshot(remote_query(out))

  out_build <- left_join(lf, lf2, by = "x") |>
    full_join(lf3, by = "x") |>
    sql_build()

  expect_s3_class(out_build, "join_query")
  expect_s3_class(out_build$x, "multi_join_query")
})

test_that("multiple joins can use by column from any table", {
  lf <- lazy_frame(x = 1, a = 1, .name = "df1")
  lf2 <- lazy_frame(x = 1, y = 2, .name = "df2")
  lf3 <- lazy_frame(x = 1, y = 2, b = 2, .name = "df3")

  out <- left_join(lf, lf2, by = "x") |>
    left_join(lf3, by = c("x", "y"))
  joins_metadata <- out$lazy_query$joins
  expect_equal(joins_metadata$by[[1]]$x, "x")
  expect_equal(joins_metadata$by[[2]]$x, c("x", "y"))
  expect_equal(joins_metadata$by[[1]]$y, "x")
  expect_equal(joins_metadata$by[[2]]$y, c("x", "y"))
  expect_equal(joins_metadata$by_x_table_id, list(1, c(1, 2)))

  lf <- lazy_frame(x = 1, a = 1, .name = "df1")
  lf2 <- lazy_frame(x = 1, y2 = 2, .name = "df2")
  lf3 <- lazy_frame(x = 1, y = 2, b = 2, .name = "df3")

  out <- left_join(
    lf,
    lf2 |> rename(y = y2),
    by = "x"
  ) |>
    left_join(lf3, by = c("x", "y"))

  joins_metadata <- out$lazy_query$joins
  expect_equal(joins_metadata$by[[1]]$x, "x")
  expect_equal(joins_metadata$by[[2]]$x, c("x", "y2"))
  expect_equal(joins_metadata$by[[1]]$y, "x")
  expect_equal(joins_metadata$by[[2]]$y, c("x", "y"))
  expect_equal(joins_metadata$by_x_table_id, list(1, c(1, 2)))
})

test_that("multi joins work with x_as", {
  lf <- lazy_frame(x = 1, a = 1, .name = "df1")
  lf2 <- lazy_frame(x = 1, b = 2, .name = "df2")
  lf3 <- lazy_frame(x = 1, b = 2, .name = "df3")

  out <- left_join(lf, lf2, by = "x", x_as = "lf1", y_as = "lf2") |>
    inner_join(lf3, by = "x", y_as = "lf3")
  lq <- out$lazy_query
  expect_s3_class(lq, "lazy_multi_join_query")
  expect_equal(
    lq$table_names,
    tibble(name = table_path(c("`lf1`", "`lf2`", "`lf3`")), from = "as")
  )

  # `x_as` provided twice with the same name -> one query
  out2 <- left_join(lf, lf2, by = "x", x_as = "lf1", y_as = "lf2") |>
    inner_join(lf3, by = "x", x_as = "lf1", y_as = "lf3")
  expect_equal(out, out2)

  # `x_as` provided twice with different names -> two queries
  lq <- left_join(lf, lf2, by = "x", x_as = "lf1") |>
    inner_join(lf3, by = "x", x_as = "lf2") |>
    _$lazy_query
  expect_s3_class(lq, "lazy_multi_join_query")
  expect_s3_class(lq$x, "lazy_multi_join_query")

  # `x_as` name already used
  lq <- left_join(lf, lf2, by = "x", y_as = "lf2") |>
    inner_join(lf3, by = "x", x_as = "lf2") |>
    _$lazy_query
  expect_s3_class(lq, "lazy_multi_join_query")
  expect_s3_class(lq$x, "lazy_multi_join_query")

  # `y_as` name already used
  lq <- left_join(lf, lf2, by = "x", y_as = "lf2") |>
    inner_join(lf3, by = "x", y_as = "lf2") |>
    _$lazy_query
  expect_s3_class(lq, "lazy_multi_join_query")
  expect_s3_class(lq$x, "lazy_multi_join_query")

  lq <- left_join(lf, lf2, by = "x", x_as = "lf2") |>
    inner_join(lf3, by = "x", y_as = "lf2") |>
    _$lazy_query
  expect_s3_class(lq, "lazy_multi_join_query")
  expect_s3_class(lq$x, "lazy_multi_join_query")
})

test_that("when keep = TRUE, left_join() preserves both sets of keys", {
  # when keys have different names
  df1 <- memdb_frame(a = c(2, 3), b = c(1, 2))
  df2 <- memdb_frame(x = c(3, 4), y = c(3, 4))
  out <- left_join(df1, df2, by = c("a" = "x"), keep = TRUE) |> collect()
  expect_equal(out$a, c(2, 3))
  expect_equal(out$x, c(NA, 3))

  # when keys have same name
  df1 <- memdb_frame(a = c(2, 3), b = c(1, 2))
  df2 <- memdb_frame(a = c(3, 4), y = c(3, 4))
  out <- left_join(df1, df2, by = c("a"), keep = TRUE) |> collect()
  expect_equal(out$a.x, c(2, 3))
  expect_equal(out$a.y, c(NA, 3))
})

test_that("when keep = TRUE, right_join() preserves both sets of keys", {
  # when keys have different names
  df1 <- memdb_frame(a = c(2, 3), b = c(1, 2))
  df2 <- memdb_frame(x = c(3, 4), y = c(3, 4))
  out <- right_join(df1, df2, by = c("a" = "x"), keep = TRUE) |> collect()
  expect_equal(out$a, c(3, NA))
  expect_equal(out$x, c(3, 4))

  # when keys have same name
  df1 <- memdb_frame(a = c(2, 3), b = c(1, 2))
  df2 <- memdb_frame(a = c(3, 4), y = c(3, 4))
  out <- right_join(df1, df2, by = c("a"), keep = TRUE) |> collect()
  expect_equal(out$a.x, c(3, NA))
  expect_equal(out$a.y, c(3, 4))
})

test_that("when keep = TRUE, full_join() preserves both sets of keys", {
  # when keys have different names
  df1 <- memdb_frame(a = c(2, 3), b = c(1, 2))
  df2 <- memdb_frame(x = c(3, 4), y = c(3, 4))
  out <- full_join(df1, df2, by = c("a" = "x"), keep = TRUE) |>
    collect() |>
    arrange(a)
  expect_equal(out$a, c(2, 3, NA))
  expect_equal(out$x, c(NA, 3, 4))

  # when keys have same name
  df1 <- memdb_frame(a = c(2, 3), b = c(1, 2))
  df2 <- memdb_frame(a = c(3, 4), y = c(3, 4))
  out <- full_join(df1, df2, by = c("a"), keep = TRUE) |>
    collect() |>
    arrange(a.x)
  expect_equal(out$a.x, c(2, 3, NA))
  expect_equal(out$a.y, c(NA, 3, 4))
})

test_that("when keep = TRUE, inner_join() preserves both sets of keys (#5581)", {
  # when keys have different names
  df1 <- memdb_frame(a = c(2, 3), b = c(1, 2))
  df2 <- memdb_frame(x = c(3, 4), y = c(3, 4))
  out <- inner_join(df1, df2, by = c("a" = "x"), keep = TRUE) |> collect()
  expect_equal(out$a, c(3))
  expect_equal(out$x, c(3))

  # when keys have same name
  df1 <- memdb_frame(a = c(2, 3), b = c(1, 2))
  df2 <- memdb_frame(a = c(3, 4), y = c(3, 4))
  out <- inner_join(df1, df2, by = c("a"), keep = TRUE) |> collect()
  expect_equal(out$a.x, c(3))
  expect_equal(out$a.y, c(3))
})

test_that("can't use `keep = FALSE` with non-equi conditions (#6499)", {
  join_by <- dplyr::join_by
  df1 <- memdb_frame(xl = c(1, 3), xu = c(4, 7))
  df2 <- memdb_frame(yl = c(2, 5, 8), yu = c(6, 8, 9))

  expect_snapshot(error = TRUE, {
    left_join(df1, df2, join_by(overlaps(xl, xu, yl, yu)), keep = FALSE)
  })

  # Would never make sense here.
  # Based on how the binary conditions are generated we'd merge:
  # - `yu` into `xl`
  # - `yl` into `xu`
  # Which results in `xl` and `xu` columns that don't maintain `xl <= xu`.
  expect_snapshot(error = TRUE, {
    full_join(df1, df2, join_by(overlaps(xl, xu, yl, yu)), keep = FALSE)
  })
})

test_that("by default, `by` columns omitted from `y` with equi-conditions, but not non-equi conditions", {
  # equi keys always keep the LHS name, regardless of whether of not a duplicate exists in the RHS
  # non-equi keys will get a suffix if a duplicate exists
  lf <- lazy_frame(x = 1, y = 1, z = 1)
  lf2 <- lazy_frame(x = 2, y = 1, z = 2)
  out <- right_join(
    lf,
    lf2,
    by = join_by(x == y, y > z),
    keep = NULL
  )
  vars <- out$lazy_query$vars
  expect_equal(vars$name, c("x", "y", "z.x", "x.y", "z.y"))
  expect_equal(vars$x, c(NA, "y", "z", NA, NA))
  expect_equal(vars$y, c("y", NA, NA, "x", "z"))

  # unless specifically requested with `keep = TRUE`
  lf <- lazy_frame(x = 1, y = 1, z = 1)
  lf2 <- lazy_frame(x = 2, y = 1, z = 2)
  out <- right_join(
    lf,
    lf2,
    by = join_by(x == y, y > z),
    keep = TRUE
  )
  vars <- out$lazy_query$vars
  expect_equal(vars$name, c("x.x", "y.x", "z.x", "x.y", "y.y", "z.y"))
  expect_equal(vars$x, c("x", "y", "z", NA, NA, NA))
  expect_equal(vars$y, c(NA, NA, NA, "x", "y", "z"))
})

test_that("can translate join conditions", {
  lf1 <- lazy_frame(a = 1, b = 1, c = 1)
  expect_snapshot({
    left_join(
      lf1,
      lf1,
      by = join_by(
        a == a,
        b >= b,
        c < c
      ),
      keep = TRUE
    )
  })
})

test_that("joins using `between(bounds =)` work as expected", {
  df1 <- memdb_frame(x = 1:5)
  df2 <- memdb_frame(lower = 2, upper = 4)

  out <- left_join(
    df1,
    df2,
    by = join_by(between(x, lower, upper, bounds = "[]"))
  ) |>
    collect()
  # out <- full_join(df1, df2, by = join_by(between(x, lower, upper, bounds = "[]")))
  expect_identical(out$lower, c(NA, 2, 2, 2, NA))
  expect_identical(out$upper, c(NA, 4, 4, 4, NA))

  out <- left_join(
    df1,
    df2,
    by = join_by(between(x, lower, upper, bounds = "[)"))
  ) |>
    collect()
  expect_identical(out$lower, c(NA, 2, 2, NA, NA))
  expect_identical(out$upper, c(NA, 4, 4, NA, NA))

  out <- left_join(
    df1,
    df2,
    by = join_by(between(x, lower, upper, bounds = "(]"))
  ) |>
    collect()
  expect_identical(out$lower, c(NA, NA, 2, 2, NA))
  expect_identical(out$upper, c(NA, NA, 4, 4, NA))

  out <- left_join(
    df1,
    df2,
    by = join_by(between(x, lower, upper, bounds = "()"))
  ) |>
    collect()
  expect_identical(out$lower, c(NA, NA, 2, NA, NA))
  expect_identical(out$upper, c(NA, NA, 4, NA, NA))
})

test_that("joins using `overlaps(bounds =)` work as expected", {
  df1 <- tibble(x_lower = c(1, 1, 3, 4), x_upper = c(2, 3, 4, 5))
  df2 <- tibble(y_lower = 2, y_upper = 4)

  expect_closed <- vctrs::vec_cbind(df1, vctrs::vec_c(df2, df2, df2, df2))
  mf1 <- memdb_frame(x_lower = c(1, 1, 3, 4), x_upper = c(2, 3, 4, 5))
  mf2 <- memdb_frame(y_lower = 2, y_upper = 4)

  out <- left_join(
    mf1,
    mf2,
    by = join_by(overlaps(x_lower, x_upper, y_lower, y_upper, bounds = "[]"))
  ) |>
    collect()
  expect_identical(out, expect_closed)

  # `[)`, `(]`, and `()` all generate the same binary conditions but are useful
  # for consistency with `between(bounds =)`
  expect_open <- vctrs::vec_cbind(df1, vctrs::vec_c(NA, df2, df2, NA))

  out <- left_join(
    mf1,
    mf2,
    by = join_by(overlaps(x_lower, x_upper, y_lower, y_upper, bounds = "[)"))
  ) |>
    collect()
  expect_identical(out, expect_open)
  out <- left_join(
    mf1,
    mf2,
    by = join_by(overlaps(x_lower, x_upper, y_lower, y_upper, bounds = "(]"))
  ) |>
    collect()
  expect_identical(out, expect_open)
  out <- left_join(
    mf1,
    mf2,
    by = join_by(overlaps(x_lower, x_upper, y_lower, y_upper, bounds = "()"))
  ) |>
    collect()
  expect_identical(out, expect_open)
})

test_that("rolling joins aren't supported", {
  lf <- lazy_frame(x = 1, y = 1)

  expect_snapshot({
    (expect_error(left_join(lf, lf, join_by(closest(x >= y)))))
    (expect_error(semi_join(lf, lf, join_by(closest(x >= y)))))
  })
})

test_that("`na_matches` is validated", {
  df <- lazy_frame(x = 1)

  # Mutating joins
  expect_snapshot(error = TRUE, {
    left_join(df, df, by = "x", na_matches = 1)
  })
  expect_snapshot(error = TRUE, {
    left_join(df, df, by = "x", na_matches = "foo")
  })

  # Filtering joins
  expect_snapshot(error = TRUE, {
    semi_join(df, df, by = "x", na_matches = 1)
  })
  expect_snapshot(error = TRUE, {
    semi_join(df, df, by = "x", na_matches = "foo")
  })
})

test_that("using multiple gives an informative error", {
  lf <- lazy_frame(x = 1)

  expect_no_error(left_join(lf, lf, by = "x", multiple = NULL))
  expect_no_error(left_join(lf, lf, by = "x", multiple = "all"))

  expect_snapshot(error = TRUE, {
    left_join(lf, lf, by = "x", multiple = "first")
  })
})

test_that("using unmatched gives an informative error", {
  lf <- lazy_frame(x = 1)
  expect_no_error(left_join(lf, lf, by = "x", unmatched = "drop"))

  expect_snapshot(error = TRUE, {
    left_join(lf, lf, by = "x", unmatched = "error")
  })
})

test_that("using relationship gives an informative error", {
  lf <- lazy_frame(x = 1)
  expect_no_error(left_join(lf, lf, by = "x", relationship = NULL))
  expect_no_error(left_join(lf, lf, by = "x", relationship = "many-to-many"))

  expect_snapshot(error = TRUE, {
    left_join(lf, lf, by = "x", relationship = "one-to-one")
  })
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
  expect_s3_class(jr$lazy_query, "lazy_rf_join_query")
  expect_equal(jr$lazy_query$type, "right")

  jf <- full_join(lf1, lf2, by = "x")
  expect_s3_class(jf$lazy_query, "lazy_rf_join_query")
  expect_equal(jf$lazy_query$type, "full")

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
  expect_snapshot(semi_join(lf1, lf2, by = "x", na_matches = "na"))
})

test_that("join captures both tables", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)

  out <- inner_join(lf1, lf2, by = "x") |> sql_build()

  expect_s3_class(out, "multi_join_query")
  expect_equal(out$joins$type, "inner")
})

test_that("semi join captures both tables", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)

  out <- semi_join(lf1, lf2, by = "x") |> sql_build()

  expect_equal(out$anti, FALSE)
})

test_that("set ops captures both tables", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)

  out <- union(lf1, lf2) |> sql_build()
  expect_s3_class(out, "union_query")
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
    inner_join(lf, df, by = "x", copy = TRUE) |> collect(),
    tibble(x = 1, y = 2, z = 3)
  )
})

test_that("left_join/inner_join uses *", {
  lf1 <- lazy_frame(a = 1, b = 2, c = 1)
  lf2 <- lazy_frame(a = 1, b = 2, z = 1)

  con <- simulate_dbi()
  out <- lf1 |>
    left_join(lf2, by = c("a", "b")) |>
    sql_build()

  expect_equal(out$select, sql("`df_LHS`.*", z = "`z`"))

  out <- lf1 |>
    left_join(lf2, by = c("a", "b")) |>
    sql_build(sql_options = sql_options(use_star = FALSE))

  expect_equal(
    out$select,
    sql(
      a = "`df_LHS`.`a`",
      b = "`df_LHS`.`b`",
      c = "`c`",
      z = "`z`"
    )
  )

  # also works after relocate
  out <- lf1 |>
    left_join(lf2, by = c("a", "b")) |>
    relocate(z) |>
    sql_build()

  expect_equal(out$select, sql(z = "`z`", "`df_LHS`.*"))

  # does not use * if variable are missing
  out <- lf1 |>
    left_join(lf2, by = c("a", "b")) |>
    select(a, c) |>
    sql_build()

  expect_equal(out$select, sql(a = "`df_LHS`.`a`", c = "`c`"))

  # does not use * if variable names changed
  lf1 <- lazy_frame(a = 1, b = 2)
  lf2 <- lazy_frame(a = 1, b = 2)
  out <- lf1 |>
    left_join(lf2, by = c("a")) |>
    sql_build()

  expect_equal(
    out$select,
    sql(a = "`df_LHS`.`a`", `b.x` = "`df_LHS`.`b`", `b.y` = "`df_RHS`.`b`")
  )
})

test_that("right_join uses *", {
  lf1 <- lazy_frame(a = 1, b = 2, c = 1)
  lf2 <- lazy_frame(a = 1, b = 2, z = 1)

  con <- simulate_dbi()
  out <- lf1 |>
    right_join(lf2, by = c("a", "b")) |>
    sql_build()

  # cannot use * without relocate or select
  expect_equal(
    out$select,
    sql(a = "`df_RHS`.`a`", b = "`df_RHS`.`b`", c = "`c`", z = "`z`")
  )

  # also works after relocate
  out <- lf1 |>
    right_join(lf2, by = c("a", "b")) |>
    select(a, b, z, c) |>
    sql_build()

  expect_equal(
    out$select,
    sql("`df_RHS`.*", c = "`c`")
  )

  # does not use * if `use_star = FALSE`
  out <- lf1 |>
    right_join(lf2, by = c("a", "b")) |>
    select(a, b, z, c) |>
    sql_build(sql_options = sql_options(use_star = FALSE))

  expect_equal(
    out$select,
    sql(
      a = "`df_RHS`.`a`",
      b = "`df_RHS`.`b`",
      z = "`z`",
      c = "`c`"
    )
  )

  # does not use * if variable are missing
  out <- lf1 |>
    right_join(lf2, by = c("a", "b")) |>
    select(a, z) |>
    sql_build()

  expect_equal(
    out$select,
    sql(a = "`df_RHS`.`a`", z = "`z`")
  )

  # does not use * if variable names changed
  lf1 <- lazy_frame(a = 1, b = 2)
  lf2 <- lazy_frame(a = 1, b = 2)
  out <- lf1 |>
    right_join(lf2, by = c("a")) |>
    sql_build()

  expect_equal(
    out$select,
    sql(a = "`df_RHS`.`a`", `b.x` = "`df_LHS`.`b`", `b.y` = "`df_RHS`.`b`")
  )
})

test_that("cross_join uses *", {
  lf1 <- lazy_frame(a = 1, b = 1)
  lf2 <- lazy_frame(x = 1, y = 1)

  con <- simulate_dbi()
  out <- lf1 |>
    cross_join(lf2) |>
    sql_build()

  expect_equal(
    out$select,
    set_names(sql("`df_LHS`.*", "`df_RHS`.*"), c("", ""))
  )

  # does not use * if `use_star = FALSE`
  out <- lf1 |>
    cross_join(lf2) |>
    sql_build(sql_options = sql_options(use_star = FALSE))

  expect_equal(
    out$select,
    sql(
      a = "`a`",
      b = "`b`",
      x = "`x`",
      y = "`y`"
    )
  )

  # also works after relocate
  out <- lf1 |>
    cross_join(lf2) |>
    select(x, y, a, b) |>
    sql_build()

  expect_equal(
    out$select,
    set_names(sql("`df_RHS`.*", "`df_LHS`.*"), c("", ""))
  )

  out <- lf1 |>
    cross_join(lf2) |>
    select(x, a, b, y) |>
    sql_build()

  expect_equal(out$select, sql(x = "`x`", "`df_LHS`.*", y = "`y`"))

  out <- lf1 |>
    cross_join(lf2) |>
    select(a, x, y, b) |>
    sql_build()

  expect_equal(out$select, sql(a = "`a`", "`df_RHS`.*", b = "`b`"))
})

test_that("full_join() does not use *", {
  con <- simulate_dbi()
  lf1 <- lazy_frame(a = 1, b = 2)
  lf2 <- lazy_frame(a = 1, b = 2)

  out <- lf1 |>
    full_join(lf2, by = c("a", "b")) |>
    sql_build()

  expect_equal(
    out$select,
    sql(
      a = "COALESCE(`df_LHS`.`a`, `df_RHS`.`a`)",
      b = "COALESCE(`df_LHS`.`b`, `df_RHS`.`b`)"
    )
  )
})

test_that("joins reuse queries in cte mode", {
  lf1 <- lazy_frame(x = 1, .name = "lf1")
  lf <- lf1 |>
    inner_join(lf1, by = "x")

  expect_snapshot(
    left_join(
      lf,
      lf
    ) |>
      remote_query(sql_options = sql_options(cte = TRUE))
  )
})

test_that("can force to qualify all columns", {
  lf1 <- lazy_frame(x = 1, a = 2, y = 1, .name = "lf1")
  lf2 <- lazy_frame(x = 1, a = 2, z = 1, .name = "lf2")

  unforced <- left_join(lf1, lf2, by = "x") |>
    sql_build(sql_options = sql_options(qualify_all_columns = FALSE))
  expect_equal(
    unforced$select,
    sql(
      x = "`lf1`.`x`",
      a.x = "`lf1`.`a`",
      y = "`y`",
      a.y = "`lf2`.`a`",
      z = "`z`"
    )
  )
  forced <- left_join(lf1, lf2, by = "x") |>
    sql_build(sql_options = sql_options(qualify_all_columns = TRUE))
  expect_equal(
    forced$select,
    sql(
      x = "`lf1`.`x`",
      a.x = "`lf1`.`a`",
      y = "`lf1`.`y`",
      a.y = "`lf2`.`a`",
      z = "`lf2`.`z`"
    )
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
