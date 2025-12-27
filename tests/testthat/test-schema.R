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
