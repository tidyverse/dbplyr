test_that("CTE quoting works right", {
  lf1 <- lazy_frame(x = 1, y = 2, .name = "lf1")
  lf2 <- lazy_frame(x = 1, z = 2, .name = "lf2")

  # The query is nonsensical, because I took a failing query from the wild and
  # whiddled it down to the minimum amount of code required to reveal two
  # separate quoting issues when using CTEs

  double_it <- function(.data, column_name) {
    .data %>%
      mutate(
        "{ column_name }" := .data[[column_name]] * 2
      )
  }

  skinny <- function(column_name) {
    lf1 %>%
      double_it(column_name) %>%
      mutate(
        column_name = column_name,
        .keep = "none"
      )
  }

  tall_tbl <- purrr::map(c("x", "y"), skinny) %>%
    purrr::reduce(dplyr::union_all)

  query <- tall_tbl %>%
    left_join(tall_tbl, by = join_by(column_name)) %>%
    left_join(tall_tbl, by = join_by(column_name)) %>%
    sql_render(sql_options = sql_options(cte = TRUE)) %>%
    expect_snapshot()
})
