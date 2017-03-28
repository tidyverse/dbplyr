context("compute")

test_that("compute doesn't change representation", {
  tbls <- test_frame(x = 5:1, y = 1:5, z = "a")

  compare_tbls(tbls, . %>% compute)
  compare_tbls(tbls, . %>% mutate(a = x) %>% compute)
})

test_that("compute can create indexes", {
  tbls <- test_frame(x = 5:1, y = 1:5, z = "a")

  compare_tbls(tbls, . %>% compute(indexes = c("x", "y")))
  compare_tbls(tbls, . %>% compute(indexes = list("x", "y", c("x", "y"))))
  compare_tbls(tbls, . %>% compute(indexes = "x", unique_indexes = "y"))
  compare_tbls(tbls, . %>% compute(unique_indexes = c("x", "y")))
  compare_tbls(tbls, . %>% compute(unique_indexes = list(c("x", "z"), c("y", "z"))))

  eval_tbls(
    tbls[!(names(tbls) %in% c("df"))],
    function(tbl) {
      expect_error(compute(tbl, unique_indexes = "z"), ".")
      expect_error(compute(tbl, indexes = "x", unique_indexes = "z"))
      data_frame()
    })
})
