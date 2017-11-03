context("output")

test_that("ungrouped output", {
  if (packageVersion("tibble") < "1.0-10")
    skip("need tibble 1.0-10 or later for this test")

  mtcars_mem <- src_memdb() %>%
    copy_to(mtcars, name = "mtcars-output-test", overwrite = TRUE)
  iris_mem <- src_memdb() %>%
    copy_to(iris, name = "iris-output-test", overwrite = TRUE) %>%
    group_by(Species) %>%
    arrange(Sepal.Length)

  withr::with_options(
    list(digits = 4, width = 80),
    with_mock(
      `dbplyr::sqlite_version` = function() "x.y.z",
      {
        expect_output_file_rel(
          cat(tbl_sum(mtcars_mem), sep = "\n"),
          "mtcars.txt"
        )

        expect_output_file_rel(
          cat(tbl_sum(iris_mem), sep = "\n"),
          "iris.txt"
        )
      }
    )
  )
})
