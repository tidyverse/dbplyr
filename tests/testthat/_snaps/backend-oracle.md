# string functions translate correctly

    Code
      translate_sql(str_replace(col, "pattern", "replacement"), con = con)
    Output
      <SQL> REGEXP_REPLACE("col", 'pattern', 'replacement', 1, 1)
    Code
      translate_sql(str_replace_all(col, "pattern", "replacement"), con = con)
    Output
      <SQL> REGEXP_REPLACE("col", 'pattern', 'replacement')

# queries translate correctly

    Code
      head(mf)
    Output
      <SQL>
      SELECT "df".*
      FROM "df"
      FETCH FIRST 6 ROWS ONLY

# `sql_query_upsert()` is correct

    Code
      sql_query_upsert(con = con, table = ident("df_x"), from = sql_render(df_y, con,
        lvl = 1), by = c("a", "b"), update_cols = c("c", "d"), returning_cols = c("a",
        b2 = "b"), method = "merge")
    Output
      <SQL> MERGE INTO "df_x"
      USING (
        SELECT "a", "b", "c" + 1.0 AS "c", "d"
        FROM "df_y"
      ) "...y"
        ON ("...y"."a" = "df_x"."a" AND "...y"."b" = "df_x"."b")
      WHEN MATCHED THEN
        UPDATE SET "c" = "...y"."c", "d" = "...y"."d"
      WHEN NOT MATCHED THEN
        INSERT ("a", "b", "c", "d")
        VALUES ("...y"."a", "...y"."b", "...y"."c", "...y"."d")
      RETURNING "df_x"."a", "df_x"."b" AS "b2"
      ;

# generates custom sql

    Code
      sql_table_analyze(con, in_schema("schema", "tbl"))
    Output
      <SQL> ANALYZE TABLE "schema"."tbl" COMPUTE STATISTICS

---

    Code
      sql_query_explain(con, sql("SELECT * FROM foo"))
    Output
      <SQL> EXPLAIN PLAN FOR SELECT * FROM foo
      <SQL> SELECT PLAN_TABLE_OUTPUT FROM TABLE(DBMS_XPLAN.DISPLAY())

---

    Code
      left_join(lf, lf, by = "x", na_matches = "na")
    Output
      <SQL>
      SELECT "df_LHS"."x" AS "x"
      FROM "df" "df_LHS"
      LEFT JOIN "df" "df_RHS"
        ON (decode("df_LHS"."x", "df_RHS"."x", 0, 1) = 0)

---

    Code
      sql_query_save(con, sql("SELECT * FROM foo"), in_schema("schema", "tbl"))
    Output
      <SQL> CREATE GLOBAL TEMPORARY TABLE "schema"."tbl" AS
      SELECT * FROM foo

---

    Code
      sql_query_save(con, sql("SELECT * FROM foo"), in_schema("schema", "tbl"),
      temporary = FALSE)
    Output
      <SQL> CREATE TABLE "schema"."tbl" AS
      SELECT * FROM foo

---

    Code
      slice_sample(lf, n = 1)
    Output
      <SQL>
      SELECT "x"
      FROM (
        SELECT
          "df".*,
          CASE
      WHEN (NOT(((DBMS_RANDOM.VALUE()) IS NULL))) THEN ROW_NUMBER() OVER (PARTITION BY (CASE WHEN (((DBMS_RANDOM.VALUE()) IS NULL)) THEN 1 ELSE 0 END) ORDER BY DBMS_RANDOM.VALUE())
      END AS "col01"
        FROM "df"
      ) "q01"
      WHERE ("col01" <= 1)

# copy_inline uses UNION ALL

    Code
      remote_query(copy_inline(con, slice(y, 0)))
    Output
      <SQL> SELECT CAST(NULL AS INT) AS "id", CAST(NULL AS VARCHAR2(255)) AS "arr"
      FROM "DUAL"
      WHERE (0 = 1)
    Code
      remote_query(copy_inline(con, y))
    Output
      <SQL> SELECT CAST("id" AS INT) AS "id", CAST("arr" AS VARCHAR2(255)) AS "arr"
      FROM (
        SELECT NULL AS "id", NULL AS "arr"
        FROM "DUAL"
        WHERE (0 = 1)
      
        UNION ALL
      
        SELECT 1, '{1,2,3}' FROM DUAL
      ) "values_table"
    Code
      remote_query(copy_inline(con, slice(y, 0), types = types))
    Output
      <SQL> SELECT CAST(NULL AS bigint) AS "id", CAST(NULL AS integer[]) AS "arr"
      FROM "DUAL"
      WHERE (0 = 1)
    Code
      remote_query(copy_inline(con, y, types = types))
    Output
      <SQL> SELECT CAST("id" AS bigint) AS "id", CAST("arr" AS integer[]) AS "arr"
      FROM (
        SELECT NULL AS "id", NULL AS "arr"
        FROM "DUAL"
        WHERE (0 = 1)
      
        UNION ALL
      
        SELECT 1, '{1,2,3}' FROM DUAL
      ) "values_table"

# difftime is translated correctly

    Code
      translate_sql(difftime(start_date, end_date, units = "auto"), con = con)
    Condition
      Error in `difftime()`:
      ! The only supported value for `units` on SQL backends is "days"

---

    Code
      translate_sql(difftime(start_date, end_date, tz = "UTC", units = "days"), con = con)
    Condition
      Error in `difftime()`:
      ! The `tz` argument is not supported for SQL backends.

