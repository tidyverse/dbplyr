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
      window_order(lf, x + y)
    Condition
      Error in `window_order()`:
      ! Every element of `...` must be a single column name or a column wrapped in `desc()`.
      x Element 1 is `x + y`.
    Code
      window_order(lf, foo())
    Condition
      Error in `window_order()`:
      ! Every element of `...` must be a single column name or a column wrapped in `desc()`.
      x Element 1 is `foo()`.
    Code
      window_order(lf, desc(x + y))
    Condition
      Error in `window_order()`:
      ! Every element of `...` must be a single column name or a column wrapped in `desc()`.
      x Element 1 is `desc(x + y)`.

# window order works afer renaming variable

    Code
      mutate(rename(window_order(lazy_frame(x = 1, y = 1), y), y2 = y), x_cum = cumsum(
        x))
    Output
      <SQL>
      SELECT
        "q01".*,
        SUM("x") OVER (ORDER BY "y2" ROWS UNBOUNDED PRECEDING) AS "x_cum"
      FROM (
        SELECT "x", "y" AS "y2"
        FROM "df"
      ) AS "q01"
    Code
      mutate(window_order(rename(lazy_frame(x = 1, y = 1), y2 = y), y2), x_cum = cumsum(
        x))
    Output
      <SQL>
      SELECT
        "q01".*,
        SUM("x") OVER (ORDER BY "y2" ROWS UNBOUNDED PRECEDING) AS "x_cum"
      FROM (
        SELECT "x", "y" AS "y2"
        FROM "df"
      ) AS "q01"

# window_frame errors for data frame

    Code
      (expect_error(window_frame(data.frame(x = 1))))
    Output
      <error/rlang_error>
      Error in `window_frame()`:
      ! `.data` must be a <tbl_lazy>, not a <data.frame>.

# window_order works with qualified desc()

    Code
      show_query(out)
    Output
      <SQL>
      SELECT "y", SUM("y") OVER (ORDER BY "y" DESC ROWS UNBOUNDED PRECEDING) AS "z"
      FROM "df"
