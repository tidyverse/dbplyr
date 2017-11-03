context("output")

test_that("ungrouped output", {
  if (packageVersion("tibble") < "1.0-10")
    skip("need tibble 1.0-10 or later for this test")

  mtcars_mem <- src_memdb() %>%
    copy_to(mtcars, name = "mtcars-output-test", overwrite = TRUE)
  iris_mem <- src_memdb() %>%
    copy_to(iris, name = "iris-output-test", overwrite = TRUE)

  withr::with_options(
    list(digits = 4, width = 80),
    with_mock(
      `dbplyr::sqlite_version` = function() "x.y.z",
      expect_equal(
        mtcars_mem %>%
          tbl_sum(),
        c(
          Source = "table<mtcars-output-test> [?? x 11]",
          Database = "sqlite x.y.z [:memory:]"
        )
      ),

      expect_equal(
        mtcars_mem %>%
          group_by(cyl, gear) %>%
          tbl_sum(),
        c(
          Source = "table<mtcars-output-test> [?? x 11]",
          Database = "sqlite x.y.z [:memory:]",
          Groups = "cyl, gear"
        )
      ),

      expect_equal(
        iris_mem %>%
          group_by(Species) %>%
          arrange(Sepal.Length, Sepal.Width) %>%
          tbl_sum(),
        c(
          Source = "table<iris-output-test> [?? x 5]",
          Database = "sqlite x.y.z [:memory:]",
          Groups = "Species",
          "Ordered by" = "Sepal.Length, Sepal.Width"
        )
      )
    )
  )
})
