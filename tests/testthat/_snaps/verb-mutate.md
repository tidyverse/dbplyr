# mutate() isn't inlined after distinct() #1119

    Code
      mutate(distinct(lf, x), x = 0)
    Output
      <SQL>
      SELECT 0.0 AS "x"
      FROM (
        SELECT DISTINCT "df".*
        FROM "df"
      ) AS "q01"

# can use window function after summarise and pure projection #1104

    Code
      (expect_no_error(mutate(lf, r = row_number())))
    Output
      <SQL>
      SELECT "q01".*, ROW_NUMBER() OVER () AS "r"
      FROM (
        SELECT "g"
        FROM "df"
        GROUP BY "g"
      ) AS "q01"

# can refer to fresly created values

    Code
      show_query(out2)
    Output
      <SQL>
      SELECT `x` + 4.0 AS `x`
      FROM (
        SELECT `x` + 2.0 AS `x`
        FROM (
          SELECT `x` + 1.0 AS `x`
          FROM `multi_mutate`
        ) AS `q01`
      ) AS `q01`

# transmute includes all needed variables

    Code
      out
    Output
      <SQL>
      SELECT "x", "x" + "y" AS "x2"
      FROM (
        SELECT "x" / 2.0 AS "x", "y"
        FROM "df"
      ) AS "q01"

# across() does not select grouping variables

    Code
      mutate(group_by(df, g), across(.fns = ~0))
    Output
      <SQL>
      SELECT "g", 0.0 AS "x"
      FROM "df"

---

    Code
      transmute(group_by(df, g), across(.fns = ~0))
    Output
      <SQL>
      SELECT "g", 0.0 AS "x"
      FROM "df"

# across() can access previously created variables

    Code
      remote_query(lf)
    Output
      <SQL> SELECT "x", SQRT("y") AS "y"
      FROM (
        SELECT "df".*, 2.0 AS "y"
        FROM "df"
      ) AS "q01"

# across() uses original column rather than overridden one

    Code
      mutate(lf, x = -x, across(everything(), ~ .x / x), y = y + x)
    Output
      <SQL>
      SELECT "x", "y" + "x" AS "y", "z"
      FROM (
        SELECT "x" / "x" AS "x", "y" / "x" AS "y", "z" / "x" AS "z"
        FROM (
          SELECT -"x" AS "x", "y", "z"
          FROM "df"
        ) AS "q01"
      ) AS "q01"

# new columns take precedence over global variables

    Code
      remote_query(lf)
    Output
      <SQL> SELECT "q01".*, "y" + 1.0 AS "z"
      FROM (
        SELECT "df".*, 2.0 AS "y"
        FROM "df"
      ) AS "q01"

# mutate() produces nice error messages

    Code
      mutate(lazy_frame(x = 1), z = non_existent + 1)
    Condition
      Error in `mutate()`:
      i In argument: `z = non_existent + 1`
      Caused by error:
      ! Object `non_existent` not found.
    Code
      mutate(lazy_frame(x = 1), across(x, mean, na.rm = z))
    Condition
      Error in `mutate()`:
      i In argument: `across(x, mean, na.rm = z)`
      Caused by error in `across()`:
      ! Problem while evaluating `na.rm = z`.
      Caused by error:
      ! Object `z` not found.
    Code
      mutate(lazy_frame(x = 1), across(x, .fns = "a"))
    Condition
      Error in `mutate()`:
      i In argument: `across(x, .fns = "a")`
      Caused by error in `across()`:
      ! `.fns` must be a function, a formula, or list of functions/formulas.

# .order and .frame are scoped to mutate call

    Code
      out
    Output
      <SQL>
      SELECT "q01".*, AVG("x") OVER (PARTITION BY "g") AS "r2"
      FROM (
        SELECT
          "df".*,
          SUM("x") OVER (PARTITION BY "g" ORDER BY "y" ROWS UNBOUNDED PRECEDING) AS "r1"
        FROM "df"
      ) AS "q01"

# .order generates correct SQL

    Code
      mutate(lf, r = cumsum(x), .by = g, .order = y)
    Output
      <SQL>
      SELECT
        "df".*,
        SUM("x") OVER (PARTITION BY "g" ORDER BY "y" ROWS UNBOUNDED PRECEDING) AS "r"
      FROM "df"
    Code
      mutate(lf, r = cumsum(x), .by = g, .order = c(x, desc(y)))
    Output
      <SQL>
      SELECT
        "df".*,
        SUM("x") OVER (PARTITION BY "g" ORDER BY "x", "y" DESC ROWS UNBOUNDED PRECEDING) AS "r"
      FROM "df"

# .frame generates correct SQL

    Code
      mutate(lf, r = sum(x), .by = g, .order = y, .frame = c(-Inf, 0))
    Output
      <SQL>
      SELECT
        "df".*,
        SUM("x") OVER (PARTITION BY "g" ORDER BY "y" ROWS UNBOUNDED PRECEDING) AS "r"
      FROM "df"
    Code
      mutate(lf, r = sum(x), .by = g, .order = y, .frame = c(-1, 1))
    Output
      <SQL>
      SELECT
        "df".*,
        SUM("x") OVER (PARTITION BY "g" ORDER BY "y" ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING) AS "r"
      FROM "df"

# .order validates inputs

    Code
      mutate(lf, r = cumsum(x), .order = x + y)
    Condition
      Error in `mutate()`:
      ! Every element of `order` must be a single column name or a column wrapped in `desc()`.
      x Element 1 is `x + y`.
    Code
      mutate(lf, r = cumsum(x), .order = foo())
    Condition
      Error in `mutate()`:
      ! Every element of `order` must be a single column name or a column wrapped in `desc()`.
      x Element 1 is `foo()`.

# .frame validates inputs

    Code
      mutate(lf, r = sum(x), .frame = c(1, 2, 3))
    Condition
      Error in `mutate()`:
      ! `frame` must have size 2, not size 3.
    Code
      mutate(lf, r = sum(x), .frame = c("a", "b"))
    Condition
      Error in `mutate()`:
      ! `frame` must be a whole number, not the string "a".

# mutate generates subqueries as needed

    Code
      mutate(lf, x = x + 1, x = x + 1)
    Output
      <SQL>
      SELECT `x` + 1.0 AS `x`
      FROM (
        SELECT `x` + 1.0 AS `x`
        FROM `df`
      ) AS `q01`

---

    Code
      mutate(lf, x1 = x + 1, x2 = x1 + 1)
    Output
      <SQL>
      SELECT `q01`.*, `x1` + 1.0 AS `x2`
      FROM (
        SELECT `df`.*, `x` + 1.0 AS `x1`
        FROM `df`
      ) AS `q01`

# mutate collapses over nested select

    Code
      mutate(select(lf, x:y), x = x * 2, y = y * 2)
    Output
      <SQL>
      SELECT "x" * 2.0 AS "x", "y" * 2.0 AS "y"
      FROM "df"

---

    Code
      mutate(select(lf, y:x), x = x * 2, y = y * 2)
    Output
      <SQL>
      SELECT "y" * 2.0 AS "y", "x" * 2.0 AS "x"
      FROM "df"

# var = NULL works when var is in original data

    Code
      remote_query(lf)
    Output
      <SQL> SELECT "x" * 2.0 AS "z"
      FROM (
        SELECT 2.0 AS "x"
        FROM "df"
      ) AS "q01"

# var = NULL when var is in final output

    Code
      remote_query(lf)
    Output
      <SQL> SELECT "df".*, 3.0 AS "y"
      FROM "df"

# temp var with nested arguments

    Code
      remote_query(lf)
    Output
      <SQL> SELECT "x", "y" * 2.0 AS "z"
      FROM (
        SELECT "df".*, 2.0 AS "y"
        FROM "df"
      ) AS "q01"

