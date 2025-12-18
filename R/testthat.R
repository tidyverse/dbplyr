compare_tbl <- function(x, y, label = NULL, expected.label = NULL) {
  testthat::expect_equal(
    arrange(collect(x), dplyr::across(everything())),
    arrange(collect(y), dplyr::across(everything())),
    label = label,
    expected.label = expected.label
  )
}

