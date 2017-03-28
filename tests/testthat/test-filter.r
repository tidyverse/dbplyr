context("Filter")

df <- expand.grid(a = 1:10, b = letters[1:10],
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE)

tbls <- test_load(df)

test_that("filter results independent of data tbl (simple)", {
  skip_if_no_sqlite()

  expected <- df[df$a > 6, , drop = FALSE]
  compare_tbls(tbls[c("df", "sqlite")], function(x) {
    filter(x, a > 6)
  }, expected)
})

test_that("filter captures local variables", {
  sel <- c("d", "g", "a")
  expected <- df[df$b %in% sel, , drop = FALSE]

  compare_tbls(tbls, function(x) x %>% filter(b %in% sel), ref = expected)
})

test_that("two filters equivalent to one", {
  expected <- filter(df, a > 4 & b == "a")

  compare_tbls(tbls, function(x) x %>% filter(a > 4) %>% filter(b == "a"),
    ref = expected)
})

