# pasting translated correctly

    Code
      translate_sql(paste0(x, collapse = ""), con = con, window = FALSE)
    Condition
      Error in `check_collapse()`:
      ! `collapse` not supported in DB translation of `paste()`.
      i Please use `str_flatten()` instead.

# custom lubridate functions translated correctly

    Code
      translate_sql(quarter(x, fiscal_start = 2), con = con)
    Condition
      Error in `quarter()`:
      ! `fiscal_start = 2` isn't supported in PostgreSQL translation.
      i It must be 1 instead.

# custom clock functions translated correctly

    Code
      translate_sql(date_count_between(date_column_1, date_column_2, "year"), con = con)
    Condition
      Error in `date_count_between()`:
      ! `precision = "year"` isn't supported on database backends.
      i It must be "day" instead.

---

    Code
      translate_sql(date_count_between(date_column_1, date_column_2, "day", n = 5),
      con = con)
    Condition
      Error in `date_count_between()`:
      ! `n = 5` isn't supported on database backends.
      i It must be 1 instead.

# difftime is translated correctly

    Code
      translate_sql(difftime(start_date, end_date, units = "auto"), con = con)
    Condition
      Error in `difftime()`:
      ! `units = "auto"` isn't supported on database backends.
      i It must be "days" instead.

---

    Code
      translate_sql(difftime(start_date, end_date, tz = "UTC", units = "days"), con = con)
    Condition
      Error in `difftime()`:
      ! Argument `tz` isn't supported on database backends.

# custom window functions translated correctly

    Code
      translate_sql(quantile(x, 0.3, na.rm = TRUE), con = con, window = TRUE)
    Condition
      Error in `quantile()`:
      ! Translation of `quantile()` in `mutate()` is not supported for PostgreSQL.
      i Use a combination of `summarise()` and `left_join()` instead:
        `df |> left_join(summarise(<col> = quantile(x, 0.3, na.rm = TRUE)))`.

---

    Code
      translate_sql(median(x, na.rm = TRUE), con = con, window = TRUE)
    Condition
      Error in `median()`:
      ! Translation of `median()` in `mutate()` is not supported for PostgreSQL.
      i Use a combination of `summarise()` and `left_join()` instead:
        `df |> left_join(summarise(<col> = median(x, na.rm = TRUE)))`.

# custom SQL translation

    Code
      left_join(lf, lf, by = "x", na_matches = "na")
    Output
      <SQL>
      SELECT "df_LHS"."x" AS "x"
      FROM "df" AS "df_LHS"
      LEFT JOIN "df" AS "df_RHS"
        ON ("df_LHS"."x" IS NOT DISTINCT FROM "df_RHS"."x")

---

    Code
      remote_query(copy_inline(con, tibble(x = integer(), y = character())))
    Output
      <SQL> SELECT CAST(NULL AS INTEGER) AS "x", CAST(NULL AS TEXT) AS "y"
      WHERE (0 = 1)

---

    Code
      remote_query(copy_inline(con, tibble(x = 1:2, y = letters[1:2])))
    Output
      <SQL> SELECT CAST("x" AS INTEGER) AS "x", CAST("y" AS TEXT) AS "y"
      FROM (  VALUES (1, 'a'), (2, 'b')) AS drvd("x", "y")

# `sql_query_insert()` works

    Code
      (sql_query_insert(con = con, table = ident("df_x"), from = sql_render(df_y, con,
        lvl = 1), insert_cols = colnames(df_y), by = c("a", "b"), conflict = "error",
      returning_cols = c("a", b2 = "b")))
    Condition
      Error in `sql_query_insert()`:
      ! `conflict = "error"` isn't supported on database backends.
      i It must be "ignore" instead.

---

    Code
      sql_query_insert(con = con, table = ident("df_x"), from = sql_render(df_y, con,
        lvl = 1), insert_cols = colnames(df_y), by = c("a", "b"), conflict = "ignore",
      returning_cols = c("a", b2 = "b"))
    Output
      <SQL> INSERT INTO "df_x" ("a", "b", "c", "d")
      SELECT *
      FROM (
        SELECT "a", "b", "c" + 1.0 AS "c", "d"
        FROM "df_y"
      ) AS "...y"
      ON CONFLICT ("a", "b")
      DO NOTHING
      RETURNING "df_x"."a", "df_x"."b" AS "b2"

# `sql_query_upsert()` with method = 'on_conflict' is correct

    Code
      sql_query_upsert(con = con, table = ident("df_x"), from = sql_render(df_y, con,
        lvl = 1), by = c("c", "d"), update_cols = c("a", "b"), returning_cols = c("a",
        b2 = "b"), method = "on_conflict")
    Output
      <SQL> INSERT INTO "df_x" ("c", "d", "a", "b")
      SELECT "c", "d", "a", "b"
      FROM (
        SELECT "a", "b", "c" + 1.0 AS "c", "d"
        FROM "df_y"
      ) AS "...y"
      WHERE true
      ON CONFLICT  ("c", "d")
      DO UPDATE
      SET "a" = "excluded"."a", "b" = "excluded"."b"
      RETURNING "df_x"."a", "df_x"."b" AS "b2"

# can explain

    Code
      explain(mutate(db, y = x + 1))
    Output
      <SQL>
      SELECT "test".*, "x" + 1.0 AS "y"
      FROM "test"
      
      <PLAN>
                                                 QUERY PLAN
      1 Seq Scan on test  (cost=0.00..1.04 rows=3 width=36)

# can insert with returning

    Code
      rows_insert(x, y, by = c("a", "b"), in_place = TRUE, conflict = "ignore",
      returning = everything(), method = "on_conflict")
    Condition
      Error in `rows_insert()`:
      ! Can't modify database table "df_x".
      i Using SQL: INSERT INTO "df_x" ("a", "b", "c", "d") SELECT * FROM ( SELECT "a", "b", "c" + 1.0 AS "c", "d" FROM "df_y" ) AS "...y" ON CONFLICT ("a", "b") DO NOTHING RETURNING "df_x"."a", "df_x"."b", "df_x"."c", "df_x"."d"
      Caused by error:
      ! dummy DBI error

# can upsert with returning

    Code
      rows_upsert(x, y, by = c("a", "b"), in_place = TRUE, returning = everything(),
      method = "on_conflict")
    Condition
      Error in `rows_upsert()`:
      ! Can't modify database table "df_x".
      i Using SQL: INSERT INTO "df_x" ("a", "b", "c", "d") SELECT "a", "b", "c", "d" FROM ( SELECT "a", "b", "c" + 1.0 AS "c", "d" FROM "df_y" ) AS "...y" WHERE true ON CONFLICT ("a", "b") DO UPDATE SET "c" = "excluded"."c", "d" = "excluded"."d" RETURNING "df_x"."a", "df_x"."b", "df_x"."c", "df_x"."d"
      Caused by error:
      ! dummy DBI error
