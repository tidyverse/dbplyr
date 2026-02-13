# two filters equivalent to one

    Code
      show_query(df1)
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`x` > 3.0) AND (`y` < 3.0)

---

    Code
      show_query(df2)
    Output
      <SQL>
      SELECT `x`, `y`, `id`
      FROM (
        SELECT *, AVG(`x`) OVER () AS `col01`
        FROM `df`
      ) AS `q01`
      WHERE (`col01` > 2.0) AND (`y` < 3.0)

# errors for named input

    Code
      filter(lf, x = 1)
    Condition
      Error in `filter()`:
      ! Problem with `filter()` input `..1`.
      x Input `..1` is named.
      i This usually means that you've used `=` instead of `==`.
      i Did you mean `x == 1`?

---

    Code
      filter(lf, y > 1, x = 1)
    Condition
      Error in `filter()`:
      ! Problem with `filter()` input `..2`.
      x Input `..2` is named.
      i This usually means that you've used `=` instead of `==`.
      i Did you mean `x == 1`?

# .preserve is not supported

    Code
      filter(lf, x == 1, .preserve = TRUE)
    Condition
      Error in `filter()`:
      ! `.preserve = TRUE` isn't supported on database backends.
      i It must be FALSE instead.

# catches `.by` with grouped-df

    Code
      filter(gdf, .by = x)
    Condition
      Error:
      ! Can't supply `.by` when `.data` is a grouped data frame.

# filter() can use window function and external vector - #1048

    Code
      filter(lazy_frame(x = 1L), x == max(x, na.rm = T), x %in% to_filter)
    Output
      <SQL>
      SELECT "x"
      FROM (
        SELECT *, MAX("x") OVER () AS "col01"
        FROM "df"
      ) AS "q01"
      WHERE ("x" = "col01") AND ("x" IN (1, 2))

# filter() after summarise() uses `HAVING`

    Code
      (out <- filter(lf, g == 1))
    Output
      <SQL>
      SELECT "g", "h", AVG("x") AS "x_mean"
      FROM "df"
      GROUP BY "g", "h"
      HAVING ("g" = 1.0)

---

    Code
      (out <- filter(lf, x_mean > 1))
    Output
      <SQL>
      SELECT "g", "h", AVG("x") AS "x_mean"
      FROM "df"
      GROUP BY "g", "h"
      HAVING (AVG("x") > 1.0)

---

    Code
      (out <- filter(filter(lf, g == 1), g == 2))
    Output
      <SQL>
      SELECT "g", "h", AVG("x") AS "x_mean"
      FROM "df"
      GROUP BY "g", "h"
      HAVING ("g" = 1.0) AND ("g" = 2.0)

---

    Code
      (out <- filter(filter(lf, g == 1), h == 2))
    Output
      <SQL>
      SELECT "g", "h", AVG("x") AS "x_mean"
      FROM "df"
      GROUP BY "g", "h"
      HAVING ("g" = 1.0) AND ("h" = 2.0)

# `HAVING` supports expressions #1128

    Code
      filter(summarise(lf, x_sum = sum(x, na.rm = TRUE)), !is.na(x_sum))
    Output
      <SQL>
      SELECT SUM("x") AS "x_sum"
      FROM "df"
      HAVING (NOT(((SUM("x")) IS NULL)))

# filter() after mutate() does not use `HAVING`

    Code
      (out <- filter(lf, x_mean > 1))
    Output
      <SQL>
      SELECT *
      FROM (
        SELECT *, AVG("x") OVER (PARTITION BY "g", "h") AS "x_mean"
        FROM "df"
      ) AS "q01"
      WHERE ("x_mean" > 1.0)

# filter() using a window function after summarise() does not use `HAVING`

    Code
      (out <- filter(lf, cumsum(x_mean) == 1))
    Condition
      Warning:
      Windowed expression `SUM("x_mean")` does not have explicit order.
      i Please use `arrange()`, `window_order()`, or `.order` to make deterministic.
    Output
      <SQL>
      SELECT "g", "h", "x_mean"
      FROM (
        SELECT
          *,
          SUM("x_mean") OVER (PARTITION BY "g" ROWS UNBOUNDED PRECEDING) AS "col01"
        FROM (
          SELECT "g", "h", AVG("x") AS "x_mean"
          FROM "df"
          GROUP BY "g", "h"
        ) AS "q01"
      ) AS "q01"
      WHERE ("col01" = 1.0)

