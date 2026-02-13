# custom string translations

    Code
      translate_sql(paste0("a", "b"), con = con)
    Output
      <SQL> 'a' || 'b'

---

    Code
      translate_sql(paste("a", "b"), con = con)
    Output
      <SQL> 'a' || ' ' || 'b'

---

    Code
      translate_sql(substr(x, 2, 4), con = con)
    Output
      <SQL> SUBSTRING("x", 2, 3)

---

    Code
      translate_sql(substring(x, 2, 4), con = con)
    Output
      <SQL> SUBSTRING("x", 2, 3)

---

    Code
      translate_sql(str_sub(x, 2, -2), con = con)
    Output
      <SQL> SUBSTRING("x", 2, LENGTH("x") - 2)

# copy_inline uses UNION ALL

    Code
      remote_query(copy_inline(con, slice(y, 0)))
    Output
      <SQL> SELECT CAST(NULL AS INTEGER) AS "id", CAST(NULL AS VARCHAR) AS "arr"
      FROM "DUMMY"
      WHERE (0 = 1)
    Code
      remote_query(copy_inline(con, y))
    Output
      <SQL> SELECT CAST("id" AS INTEGER) AS "id", CAST("arr" AS VARCHAR) AS "arr"
      FROM (
        SELECT NULL AS "id", NULL AS "arr"
        FROM "DUMMY"
        WHERE (0 = 1)
      
        UNION ALL
      
        SELECT 1, '{1,2,3}' FROM DUMMY
      ) AS "values_table"
    Code
      remote_query(copy_inline(con, slice(y, 0), types = types))
    Output
      <SQL> SELECT CAST(NULL AS bigint) AS "id", CAST(NULL AS integer[]) AS "arr"
      FROM "DUMMY"
      WHERE (0 = 1)
    Code
      remote_query(copy_inline(con, y, types = types))
    Output
      <SQL> SELECT CAST("id" AS bigint) AS "id", CAST("arr" AS integer[]) AS "arr"
      FROM (
        SELECT NULL AS "id", NULL AS "arr"
        FROM "DUMMY"
        WHERE (0 = 1)
      
        UNION ALL
      
        SELECT 1, '{1,2,3}' FROM DUMMY
      ) AS "values_table"

