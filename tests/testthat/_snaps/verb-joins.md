# complete join pipeline works with SQLite and table alias

    Code
      left_join(lf1, lf2, by = "x", x_as = "df1", y_as = "df2")
    Output
      <SQL>
      SELECT `df1`.`x` AS `x`, `y`
      FROM `lf1` AS `df1`
      LEFT JOIN `lf2` AS `df2`
        ON (`df1`.`x` = `df2`.`x`)

# complete semi join works with SQLite and table alias

    Code
      inner_join(lf1, lf2, by = "x", x_as = "df1", y_as = "df2")
    Output
      <SQL>
      SELECT `df1`.*
      FROM `df` AS `df1`
      INNER JOIN `df` AS `df2`
        ON (`df1`.`x` = `df2`.`x`)

# join works with in_schema

    Code
      remote_query(left_join(df1, df2, by = "x"))
    Output
      <SQL> SELECT `df`.*, `z`
      FROM `foo`.`df`
      LEFT JOIN `foo`.`df2`
        ON (`df`.`x` = `df2`.`x`)

---

    Code
      remote_query(left_join(df1, df3, by = "x"))
    Output
      <SQL> SELECT `df_LHS`.*, `z`
      FROM `foo`.`df` AS `df_LHS`
      LEFT JOIN `foo2`.`df` AS `df_RHS`
        ON (`df_LHS`.`x` = `df_RHS`.`x`)

---

    Code
      remote_query(left_join(df4, df5, by = "x"))
    Output
      <SQL> SELECT `df_LHS`.*, `z`
      FROM foo.df AS `df_LHS`
      LEFT JOIN foo2.df AS `df_RHS`
        ON (`df_LHS`.`x` = `df_RHS`.`x`)

# alias truncates long table names at database limit

    Code
      remote_query(self_join2)
    Output
      <SQL> SELECT "a01234567890123456789012345678901234567890123456789012345678901".*
      FROM "a01234567890123456789012345678901234567890123456789012345678901"
      LEFT JOIN "a01234567890123456789012345678901234567890123456789012345678901" AS "34567890123456789012345678901234567890123456789012345678901_RHS"
        ON (
          "a01234567890123456789012345678901234567890123456789012345678901"."x" = "34567890123456789012345678901234567890123456789012345678901_RHS"."x" AND
          "a01234567890123456789012345678901234567890123456789012345678901"."y" = "34567890123456789012345678901234567890123456789012345678901_RHS"."y"
        )

---

    Code
      remote_query(self_join3)
    Output
      <SQL> SELECT
        "a01234567890123456789012345678901234567890123456789012345678901"."x" AS "x",
        "a01234567890123456789012345678901234567890123456789012345678901"."y" AS "y.x",
        "b01234567890123456789012345678901234567890123456789012345678901"."y" AS "y.y"
      FROM "a01234567890123456789012345678901234567890123456789012345678901"
      LEFT JOIN "a01234567890123456789012345678901234567890123456789012345678901" AS "34567890123456789012345678901234567890123456789012345678901...2"
        ON (
          "a01234567890123456789012345678901234567890123456789012345678901"."x" = "34567890123456789012345678901234567890123456789012345678901...2"."x" AND
          "a01234567890123456789012345678901234567890123456789012345678901"."y" = "34567890123456789012345678901234567890123456789012345678901...2"."y"
        )
      INNER JOIN "b01234567890123456789012345678901234567890123456789012345678901"
        ON ("a01234567890123456789012345678901234567890123456789012345678901"."x" = "b01234567890123456789012345678901234567890123456789012345678901"."x")

# cross join via by = character() is deprecated

    Code
      out_inner <- collect(inner_join(df1, df2, by = character()))
    Condition
      Warning:
      Using `by = character()` to perform a cross join was deprecated in dbplyr 1.1.0.
      i Please use `cross_join()` instead.
    Code
      out_full <- collect(full_join(df1, df2, by = character()))
    Condition
      Warning:
      Using `by = character()` to perform a cross join was deprecated in dbplyr 1.1.0.
      i Please use `cross_join()` instead.

# join check `x_as` and `y_as`

    Code
      left_join(x, x, by = "x", y_as = c("A", "B"))
    Condition
      Error in `left_join()`:
      ! `y_as` must be a single string or `NULL`, not a character vector.

---

    Code
      left_join(x, x, by = "x", x_as = "LHS", y_as = "LHS")
    Condition
      Error in `left_join()`:
      ! `y_as` must be different from `x_as`.

# select() before join is inlined

    Code
      out_left
    Output
      <SQL>
      SELECT `a` AS `a2`, `x1` AS `x`, `b`
      FROM `lf1`
      LEFT JOIN `lf2`
        ON (`lf1`.`x1` = `lf2`.`x2`)

# rename works with duplicate column names in join_by (#1572)

    Code
      out
    Output
      <SQL>
      SELECT `x`, `y` AS `z`
      FROM `df` AS `df_LHS`
      LEFT JOIN `df` AS `df_RHS`
        ON (`df_LHS`.`x` >= `df_RHS`.`y` AND `df_LHS`.`x` <= `df_RHS`.`y`)

# select() before semi_join is inlined

    Code
      out_semi
    Output
      <SQL>
      SELECT `a` AS `a2`, `x1` AS `x`
      FROM `lf1`
      WHERE EXISTS (
        SELECT 1 FROM `lf2`
        WHERE (`lf1`.`x1` = `lf2`.`x2`)
      )

# can combine full_join with other joins #1178

    Code
      left_join(full_join(lf1, lf2, by = "x"), lf3, by = "x")
    Output
      <SQL>
      SELECT `LHS`.*, `z`
      FROM (
        SELECT COALESCE(`df_LHS`.`x`, `df_RHS`.`x`) AS `x`, `y`
        FROM `df` AS `df_LHS`
        FULL JOIN `df` AS `df_RHS`
          ON (`df_LHS`.`x` = `df_RHS`.`x`)
      ) AS `LHS`
      LEFT JOIN `df`
        ON (`LHS`.`x` = `df`.`x`)

---

    Code
      full_join(left_join(lf1, lf2, by = "x"), lf3, by = "x")
    Output
      <SQL>
      SELECT COALESCE(`LHS`.`x`, `df`.`x`) AS `x`, `y`, `z`
      FROM (
        SELECT `df_LHS`.`x` AS `x`, `y`
        FROM `df` AS `df_LHS`
        LEFT JOIN `df` AS `df_RHS`
          ON (`df_LHS`.`x` = `df_RHS`.`x`)
      ) AS `LHS`
      FULL JOIN `df`
        ON (`LHS`.`x` = `df`.`x`)

---

    Code
      full_join(full_join(lf1, lf2, by = "x"), lf3, by = "x")
    Output
      <SQL>
      SELECT COALESCE(`LHS`.`x`, `df`.`x`) AS `x`, `y`, `z`
      FROM (
        SELECT COALESCE(`df_LHS`.`x`, `df_RHS`.`x`) AS `x`, `y`
        FROM `df` AS `df_LHS`
        FULL JOIN `df` AS `df_RHS`
          ON (`df_LHS`.`x` = `df_RHS`.`x`)
      ) AS `LHS`
      FULL JOIN `df`
        ON (`LHS`.`x` = `df`.`x`)

# filter() before semi join is inlined

    Code
      out
    Output
      <SQL>
      SELECT `df_LHS`.*
      FROM `df` AS `df_LHS`
      WHERE EXISTS (
        SELECT 1 FROM `df` AS `df_RHS`
        WHERE
          (`df_LHS`.`x` = `df_RHS`.`x2`) AND
          (`df_RHS`.`a` = 1) AND
          (`df_RHS`.`b` = 2)
      )

# filtered aggregates with subsequent select are not inlined away in semi_join (#1474)

    Code
      out
    Output
      <SQL>
      SELECT `df`.*
      FROM `df`
      WHERE EXISTS (
        SELECT 1 FROM (
        SELECT `x`
        FROM `df`
        GROUP BY `x`
        HAVING (COUNT(*) = 1.0)
      ) AS `RHS`
        WHERE (`df`.`x` = `RHS`.`x`)
      )

# filtered window joins work in a semi_join

    Code
      show_query(out)
    Output
      <SQL>
      SELECT `df1`.*
      FROM `df1`
      WHERE NOT EXISTS (
        SELECT 1 FROM (
        SELECT `df2`.*, ROW_NUMBER() OVER () AS `col01`
        FROM `df2`
      ) AS `RHS`
        WHERE (`df1`.`id` = `RHS`.`id`) AND (`RHS`.`col01` <= 3.0)
      )

# multiple joins create a single query

    Code
      out
    Output
      <SQL>
      SELECT `df1`.*, `df2`.`b` AS `b.x`, `df3`.`b` AS `b.y`
      FROM `df1`
      LEFT JOIN `df2`
        ON (`df1`.`x` = `df2`.`x`)
      INNER JOIN `df3`
        ON (`df1`.`x` = `df3`.`x`)

# can join 4 tables with same column #1101

    Code
      remote_query(out)
    Output
      <SQL> SELECT `lf1`.*, `b`, `c`, `lf4`.`a` AS `a4`
      FROM `lf1`
      INNER JOIN `lf2`
        ON (`lf1`.`x` = `lf2`.`x`)
      INNER JOIN `lf3`
        ON (`lf1`.`x` = `lf3`.`x`)
      INNER JOIN `lf4`
        ON (`lf1`.`x` = `lf4`.`x`)

# multiple joins produce separate queries if using right/full join

    Code
      remote_query(out)
    Output
      <SQL> SELECT `df3`.`x` AS `x`, `a`, `LHS`.`b` AS `b.x`, `df3`.`b` AS `b.y`
      FROM (
        SELECT `df1`.*, `b`
        FROM `df1`
        LEFT JOIN `df2`
          ON (`df1`.`x` = `df2`.`x`)
      ) AS `LHS`
      RIGHT JOIN `df3`
        ON (`LHS`.`x` = `df3`.`x`)

# can't use `keep = FALSE` with non-equi conditions (#6499)

    Code
      left_join(df1, df2, join_by(overlaps(xl, xu, yl, yu)), keep = FALSE)
    Condition
      Error in `left_join()`:
      ! Can't set `keep = FALSE` when using an inequality, rolling, or overlap join.

---

    Code
      full_join(df1, df2, join_by(overlaps(xl, xu, yl, yu)), keep = FALSE)
    Condition
      Error in `full_join()`:
      ! Can't set `keep = FALSE` when using an inequality, rolling, or overlap join.

# can translate join conditions

    Code
      left_join(lf1, lf1, by = join_by(a == a, b >= b, c < c), keep = TRUE)
    Output
      <SQL>
      SELECT
        `df_LHS`.`a` AS `a.x`,
        `df_LHS`.`b` AS `b.x`,
        `df_LHS`.`c` AS `c.x`,
        `df_RHS`.`a` AS `a.y`,
        `df_RHS`.`b` AS `b.y`,
        `df_RHS`.`c` AS `c.y`
      FROM `df` AS `df_LHS`
      LEFT JOIN `df` AS `df_RHS`
        ON (
          `df_LHS`.`a` = `df_RHS`.`a` AND
          `df_LHS`.`b` >= `df_RHS`.`b` AND
          `df_LHS`.`c` < `df_RHS`.`c`
        )

# rolling joins aren't supported

    Code
      (expect_error(left_join(lf, lf, join_by(closest(x >= y)))))
    Output
      <error/rlang_error>
      Error in `left_join()`:
      ! Rolling joins aren't supported on database backends.
    Code
      (expect_error(semi_join(lf, lf, join_by(closest(x >= y)))))
    Output
      <error/rlang_error>
      Error in `semi_join()`:
      ! Rolling joins aren't supported on database backends.

# `na_matches` is validated

    Code
      left_join(df, df, by = "x", na_matches = 1)
    Condition
      Error in `left_join()`:
      ! `na_matches` must be a character vector, not the number 1.

---

    Code
      left_join(df, df, by = "x", na_matches = "foo")
    Condition
      Error in `left_join()`:
      ! `na_matches` must be one of "na" or "never", not "foo".

---

    Code
      semi_join(df, df, by = "x", na_matches = 1)
    Condition
      Error in `semi_join()`:
      ! `na_matches` must be a character vector, not the number 1.

---

    Code
      semi_join(df, df, by = "x", na_matches = "foo")
    Condition
      Error in `semi_join()`:
      ! `na_matches` must be one of "na" or "never", not "foo".

# using multiple gives an informative error

    Code
      left_join(lf, lf, by = "x", multiple = "first")
    Condition
      Error in `left_join()`:
      ! Argument `multiple` isn't supported on database backends.
      For equi joins you can instead add a unique index for the join columns in `y`.
      `db_create_index( con = remote_con(y), table = remote_name(y), columns = "x", unique = TRUE )`

# using unmatched gives an informative error

    Code
      left_join(lf, lf, by = "x", unmatched = "error")
    Condition
      Error in `left_join()`:
      ! Argument `unmatched` isn't supported on database backends.
      i For equi joins you can instead add a foreign key from `x` to `y` for the join columns.

# using relationship gives an informative error

    Code
      left_join(lf, lf, by = "x", relationship = "one-to-one")
    Condition
      Error in `left_join()`:
      ! `relationship = "one-to-one"` isn't supported on database backends.
      i It must be "many-to-many" or `NULL` instead.

# can optionally match NA values

    Code
      left_join(lf1, lf2, by = "x", na_matches = "na")
    Output
      <SQL>
      SELECT `lf1`.`x` AS `x`
      FROM `lf1`
      LEFT JOIN `lf2`
        ON (`lf1`.`x` IS NOT DISTINCT FROM `lf2`.`x`)

---

    Code
      semi_join(lf1, lf2, by = "x", na_matches = "na")
    Output
      <SQL>
      SELECT `lf1`.*
      FROM `lf1`
      WHERE EXISTS (
        SELECT 1 FROM `lf2`
        WHERE (`lf1`.`x` IS NOT DISTINCT FROM `lf2`.`x`)
      )

---

    Code
      left_join(lf1, lf3, by = by, na_matches = "na")
    Output
      <SQL>
      SELECT `x`, `lf3`.*
      FROM `lf1`
      LEFT JOIN `lf3`
        ON (
          (`lf1`.`x` >= `lf3`.`lower` OR (`lf1`.`x` IS NULL AND `lf3`.`lower` IS NULL)) AND
          (`lf1`.`x` <= `lf3`.`upper` OR (`lf1`.`x` IS NULL AND `lf3`.`upper` IS NULL))
        )

# suffix arg is checked

    Code
      (expect_error(inner_join(lf1, lf2, by = "x", suffix = "a")))
    Output
      <error/rlang_error>
      Error in `inner_join()`:
      ! `suffix` must be a character vector of length 2, not a string of length 1.
    Code
      (expect_error(inner_join(lf1, lf2, by = "x", suffix = 1L)))
    Output
      <error/rlang_error>
      Error in `inner_join()`:
      ! `suffix` must be a character vector of length 2, not an integer of length 1.

# joins reuse queries in cte mode

    Code
      remote_query(left_join(lf, lf, by = "x"), sql_options = sql_options(cte = TRUE))
    Output
      <SQL> WITH `q01` AS (
        SELECT `lf1_LHS`.`x` AS `x`
        FROM `lf1` AS `lf1_LHS`
        INNER JOIN `lf1` AS `lf1_RHS`
          ON (`lf1_LHS`.`x` = `lf1_RHS`.`x`)
      )
      SELECT `lf1...1`.`x` AS `x`
      FROM `lf1` AS `lf1...1`
      INNER JOIN `lf1` AS `lf1...2`
        ON (`lf1...1`.`x` = `lf1...2`.`x`)
      LEFT JOIN `q01` AS `...3`
        ON (`lf1...1`.`x` = `...3`.`x`)

# joins correctly quote reused queries in CTEs

    Code
      show_query(left_join(lf, lf, by = join_by(z)), sql_options = sql_options(cte = TRUE))
    Output
      <SQL>
      WITH `q01` AS (
        SELECT `lf`.*, `x` * 2.0 AS `z`
        FROM `lf`
      )
      SELECT `LHS`.`x` AS `x.x`, `LHS`.`z` AS `z`, `RHS`.`x` AS `x.y`
      FROM `q01` AS `LHS`
      LEFT JOIN `q01` AS `RHS`
        ON (`LHS`.`z` = `RHS`.`z`)

