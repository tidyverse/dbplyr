# custom SQL translation

    Code
      left_join(lf, lf, by = "x", na_matches = "na")
    Output
      <SQL>
      SELECT `df_LHS`.`x` AS `x`
      FROM `df` AS `df_LHS`
      LEFT JOIN `df` AS `df_RHS`
        ON (`df_LHS`.`x` IS NOT DISTINCT FROM `df_RHS`.`x`)

---

    Code
      copy_inline(con, tibble(x = integer(), y = character())) %>% remote_query()
    Output
      <SQL> SELECT CAST(NULL AS INTEGER) AS `x`, CAST(NULL AS TEXT) AS `y`
      WHERE (0 = 1)

---

    Code
      copy_inline(con, tibble(x = 1:2, y = letters[1:2])) %>% remote_query()
    Output
      <SQL> SELECT CAST(`x` AS INTEGER) AS `x`, CAST(`y` AS TEXT) AS `y`
      FROM (  VALUES (1, 'a'), (2, 'b')) AS drvd(`x`, `y`)

# `sql_query_insert()` works

    Code
      (sql_query_insert(con = simulate_postgres(), x_name = ident("df_x"), y = df_y,
      by = c("a", "b"), conflict = "error", returning_cols = c("a", b2 = "b")))
    Condition
      Error in `sql_query_insert()`:
      ! `conflict = "error"` isn't supported on database backends.
      i It must be "ignore" instead.

---

    Code
      sql_query_insert(con = simulate_postgres(), x_name = ident("df_x"), y = df_y,
      by = c("a", "b"), conflict = "ignore", returning_cols = c("a", b2 = "b"))
    Output
      <SQL> INSERT INTO `df_x` (`a`, `b`, `c`, `d`)
      SELECT *
      FROM (
        SELECT `a`, `b`, `c` + 1.0 AS `c`, `d`
        FROM `df_y`
      ) `...y`
      ON CONFLICT (`a`, `b`)
      DO NOTHING
      RETURNING `df_x`.`a`, `df_x`.`b` AS `b2`

# `sql_query_upsert()` with method = 'on_conflict' is correct

    Code
      sql_query_upsert(con = simulate_postgres(), x_name = ident("df_x"), y = df_y,
      by = c("a", "b"), update_cols = c("c", "d"), returning_cols = c("a", b2 = "b"),
      method = "on_conflict")
    Output
      <SQL> INSERT INTO `df_x` (`a`, `b`, `c`, `d`)
      SELECT *
      FROM (
        SELECT `a`, `b`, `c` + 1.0 AS `c`, `d`
        FROM `df_y`
      ) `...y`
      WHERE true
      ON CONFLICT  (`a`, `b`)
      DO UPDATE
      SET `c` = `excluded`.`c`, `d` = `excluded`.`d`
      RETURNING `df_x`.`a`, `df_x`.`b` AS `b2`

# can explain

    Code
      db %>% mutate(y = x + 1) %>% explain()
    Output
      <SQL>
      SELECT *, "x" + 1.0 AS "y"
      FROM "test"
      
      <PLAN>
                                                 QUERY PLAN
      1 Seq Scan on test  (cost=0.00..1.04 rows=3 width=36)

---

    Code
      db %>% mutate(y = x + 1) %>% explain(format = "json")
    Output
      <SQL>
      SELECT *, "x" + 1.0 AS "y"
      FROM "test"
      
      <PLAN>
                                                                                                                                                                                                                                                                 QUERY PLAN
      1 [\n  {\n    "Plan": {\n      "Node Type": "Seq Scan",\n      "Parallel Aware": false,\n      "Relation Name": "test",\n      "Alias": "test",\n      "Startup Cost": 0.00,\n      "Total Cost": 1.04,\n      "Plan Rows": 3,\n      "Plan Width": 36\n    }\n  }\n]

# can insert with returning

    Code
      rows_insert(x, y, by = c("a", "b"), in_place = TRUE, conflict = "ignore",
      returning = everything(), method = "on_conflict")
    Condition
      Error in `rows_insert()`:
      ! Can't modify database table "df_x".
      Caused by error:
      ! dummy DBI error

# can upsert with returning

    Code
      rows_upsert(x, y, by = c("a", "b"), in_place = TRUE, returning = everything(),
      method = "on_conflict")
    Condition
      Error in `rows_upsert()`:
      ! Can't modify database table "df_x".
      Caused by error:
      ! dummy DBI error

