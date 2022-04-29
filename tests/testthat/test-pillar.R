test_that("custom header", {
  expect_snapshot({
    "Number of rows is shown"
    x <- memdb_frame(a = 1:3) %>% filter(a > 0)
    setup <- pillar::tbl_format_setup(x)
    tbl_format_header(x, setup)[[1]]

    "Number of rows still can't be shown"
    x <- memdb_frame(a = 1:11) %>% filter(a > 0)
    setup <- pillar::tbl_format_setup(x)
    tbl_format_header(x, setup)[[1]]
  })
})
