# window_order errors for data frame

    Code
      (expect_error(window_order(data.frame(x = 1))))
    Output
      <error/rlang_error>
      Error in `window_order()`:
      ! `.data` must be a <tbl_lazy>, not a data frame.
      i Did you mean to use `arrange()` instead?
    Code
      (expect_error(window_order("a")))
    Output
      <error/rlang_error>
      Error in `window_order()`:
      ! `.data` must be a <tbl_lazy>, not a string.

# window_order only accepts variables

    Code
      (expect_error(window_order(lf, x + y)))
    Output
      <error/rlang_error>
      Error in `window_order()`:
      ! Each element of `...` must be a single column name or a column wrapped in `desc()`.
      x Element 1 is `x + y`.

# window order works afer renaming variable

    Code
      lazy_frame(x = 1, y = 1) %>% window_order(y) %>% rename(y2 = y) %>% mutate(
        x_cum = cumsum(x))
    Output
      <SQL>
      SELECT *, SUM(`x`) OVER (ORDER BY `y2` ROWS UNBOUNDED PRECEDING) AS `x_cum`
      FROM (
        SELECT `x`, `y` AS `y2`
        FROM `df`
      ) `q01`
    Code
      lazy_frame(x = 1, y = 1) %>% rename(y2 = y) %>% window_order(y2) %>% mutate(
        x_cum = cumsum(x))
    Output
      <SQL>
      SELECT *, SUM(`x`) OVER (ORDER BY `y2` ROWS UNBOUNDED PRECEDING) AS `x_cum`
      FROM (
        SELECT `x`, `y` AS `y2`
        FROM `df`
      ) `q01`

# window_frame errors for data frame

    Code
      (expect_error(window_frame(data.frame(x = 1))))
    Output
      <error/rlang_error>
      Error in `window_frame()`:
      ! `.data` must be a <tbl_lazy>, not a <data.frame>.

