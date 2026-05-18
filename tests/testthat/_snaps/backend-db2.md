# uses FETCH FIRST instead of LIMIT

    Code
      head(mf)
    Output
      <SQL>
      SELECT *
      FROM "df"
      FETCH FIRST 6 ROWS ONLY

---

    Code
      head(mf, 10)
    Output
      <SQL>
      SELECT *
      FROM "df"
      FETCH FIRST 10 ROWS ONLY

# queries with WHERE and ORDER BY work

    Code
      head(arrange(filter(mf, x > 0), y), 5)
    Output
      <SQL>
      SELECT *
      FROM "df"
      WHERE ("x" > 0.0)
      ORDER BY "y"
      FETCH FIRST 5 ROWS ONLY

# custom lubridate functions translated correctly

    Code
      translate_sql(wday(x), con = con)
    Output
      <SQL> MOD(DAYOFWEEK("x") + 0 - 1, 7) + 1

---

    Code
      translate_sql(quarter(x, fiscal_start = 2), con = con)
    Condition
      Error in `quarter()`:
      ! `fiscal_start = 2` isn't supported in DB2 translation.
      i It must be 1 instead.

# difftime is translated correctly

    Code
      translate_sql(difftime(start_date, end_date, units = "auto"), con = con)
    Condition
      Error in `difftime()`:
      ! `units = "auto"` isn't supported on database backends.
      i It must be "days" instead.

# sql_table_analyze uses RUNSTATS

    Code
      sql_table_analyze(con, ident("mytable"))
    Output
      <SQL> RUNSTATS ON TABLE "mytable" WITH DISTRIBUTION AND DETAILED INDEXES ALL

# copy_inline uses VALUES with column aliases

    Code
      remote_query(copy_inline(con, df))
    Output
      <SQL> SELECT CAST("x" AS INTEGER) AS "x", CAST("y" AS VARCHAR(255)) AS "y"
      FROM (  VALUES (1, 'a'), (2, 'b')) AS drvd("x", "y")

