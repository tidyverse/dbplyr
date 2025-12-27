test_that("can construct and print", {
  expect_snapshot(in_schema("schema", "table"))
  expect_snapshot(in_catalog("catalog", "schema", "table"))
})

test_that("can copy and collect with schema or Id", {
  con <- local_sqlite_con_with_aux()
  df <- tibble(x = 1:10)

  db <- copy_to(con, df, in_schema("aux", "db1"), temporary = FALSE)
  expect_equal(collect(db), df)
  expect_equal(collect(filter(db, x < 2)), df[1, ])

  db <- copy_to(
    con,
    df,
    DBI::Id(schema = "aux", table = "db2"),
    temporary = FALSE
  )
  expect_equal(collect(db), df)
  expect_equal(collect(filter(db, x < 2)), df[1, ])
})

test_that("quoted identifier correctly escaped", {
  con <- simulate_dbi()
  x2 <- ident_q('"x"')
  expect_equal(escape(x2, con = con), sql('"x"'))

  expect_equal(sql_vector(ident_q(), collapse = NULL, con = con), sql())
  expect_equal(
    sql_vector(ident_q(), parens = FALSE, collapse = "", con = con),
    sql("")
  )
  expect_equal(
    sql_vector(ident_q(), parens = TRUE, collapse = "", con = con),
    sql("()")
  )
})
