# custom lubridate functions translated correctly

    Code
      translate_sql(month(x, label = TRUE, abbr = TRUE))
    Condition
      Error in `month()`:
      ! `abbr` is not supported in SQL Server translation

# convert between bit and boolean as needed

    Code
      mf %>% filter(is.na(x))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE ((`x` IS NULL))

---

    Code
      mf %>% filter(!is.na(x))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (NOT((`x` IS NULL)))

---

    Code
      mf %>% filter(x == 1L || x == 2L)
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`x` = 1 OR `x` = 2)

---

    Code
      mf %>% mutate(z = ifelse(x == 1L, 1L, 2L))
    Output
      <SQL>
      SELECT *, IIF(`x` = 1, 1, 2) AS `z`
      FROM `df`

---

    Code
      mf %>% mutate(z = case_when(x == 1L ~ 1L))
    Output
      <SQL>
      SELECT *, CASE WHEN (`x` = 1) THEN 1 END AS `z`
      FROM `df`

---

    Code
      mf %>% mutate(z = !is.na(x))
    Output
      <SQL>
      SELECT *, CAST(IIF(~(`x` IS NULL), 1, 0) AS BIT) AS `z`
      FROM `df`

---

    Code
      mf %>% mutate(x = x == 1L)
    Output
      <SQL>
      SELECT CAST(IIF(`x` = 1, 1, 0) AS BIT) AS `x`
      FROM `df`

---

    Code
      mf %>% mutate(x = x == 1L || x == 2L)
    Output
      <SQL>
      SELECT CAST(IIF(`x` = 1 OR `x` = 2, 1, 0) AS BIT) AS `x`
      FROM `df`

---

    Code
      mf %>% mutate(x = x == 1L || x == 2L || x == 3L)
    Output
      <SQL>
      SELECT CAST(IIF(`x` = 1 OR `x` = 2 OR `x` = 3, 1, 0) AS BIT) AS `x`
      FROM `df`

---

    Code
      mf %>% mutate(x = !(x == 1L || x == 2L || x == 3L))
    Output
      <SQL>
      SELECT CAST(IIF(~(`x` = 1 OR `x` = 2 OR `x` = 3), 1, 0) AS BIT) AS `x`
      FROM `df`

# handles ORDER BY in subqueries

    Code
      sql_query_select(simulate_mssql(), ident("x"), ident("y"), order_by = "z",
      subquery = TRUE)
    Condition
      Warning:
      ORDER BY is ignored in subqueries without LIMIT
      i Do you need to move arrange() later in the pipeline or use window_order() instead?
    Output
      <SQL> SELECT `x`
      FROM `y`

# custom limit translation

    Code
      sql_query_select(simulate_mssql(), ident("x"), ident("y"), order_by = ident("z"),
      limit = 10)
    Output
      <SQL> SELECT TOP 10 `x`
      FROM `y`
      ORDER BY `z`

# custom escapes translated correctly

    Code
      mf %>% filter(x == a)
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`x` = 0x616263)

---

    Code
      mf %>% filter(x %in% L)
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`x` IN (0x616263, 0x0102))

---

    Code
      qry
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`x` IN (0x616263, 0x0102))

# logical escaping depends on context

    Code
      mf %>% filter(x == TRUE)
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`x` = TRUE)

---

    Code
      mf %>% mutate(x = TRUE)
    Output
      <SQL>
      SELECT 1 AS `x`
      FROM `df`

# generates custom sql

    Code
      sql_table_analyze(con, in_schema("schema", "tbl"))
    Output
      <SQL> UPDATE STATISTICS `schema`.`tbl`

---

    Code
      sql_query_save(con, sql("SELECT * FROM foo"), in_schema("schema", "tbl"))
    Output
      <SQL> SELECT * INTO `schema`.`tbl` FROM (
        SELECT * FROM foo
      ) AS temp

---

    Code
      sql_query_save(con, sql("SELECT * FROM foo"), in_schema("schema", "tbl"),
      temporary = FALSE)
    Output
      <SQL> SELECT * INTO `schema`.`tbl` FROM (
        SELECT * FROM foo
      ) AS temp

---

    Code
      lf %>% slice_sample(x)
    Output
      <SQL>
      SELECT `x`
      FROM (
        SELECT *, ROW_NUMBER() OVER (ORDER BY RAND()) AS `q01`
        FROM `df`
      ) `q01`
      WHERE (`q01` <= 1)

---

    Code
      copy_inline(con, tibble(x = 1:2, y = letters[1:2])) %>% remote_query()
    Output
      <SQL> SELECT
        TRY_CAST(TRY_CAST(`x` AS NUMERIC) AS INT) AS `x`,
        TRY_CAST(`y` AS VARCHAR(MAX)) AS `y`
      FROM (  VALUES (1, 'a'), (2, 'b')) AS drvd(`x`, `y`)

---

    Code
      copy_inline(con, trees) %>% remote_query()
    Output
      <SQL> SELECT
        TRY_CAST(`Girth` AS FLOAT) AS `Girth`,
        TRY_CAST(`Height` AS FLOAT) AS `Height`,
        TRY_CAST(`Volume` AS FLOAT) AS `Volume`
      FROM (
        VALUES
          (8.3, 70.0, 10.3),
          (8.6, 65.0, 10.3),
          (8.8, 63.0, 10.2),
          (10.5, 72.0, 16.4),
          (10.7, 81.0, 18.8),
          (10.8, 83.0, 19.7),
          (11.0, 66.0, 15.6),
          (11.0, 75.0, 18.2),
          (11.1, 80.0, 22.6),
          (11.2, 75.0, 19.9),
          (11.3, 79.0, 24.2),
          (11.4, 76.0, 21.0),
          (11.4, 76.0, 21.4),
          (11.7, 69.0, 21.3),
          (12.0, 75.0, 19.1),
          (12.9, 74.0, 22.2),
          (12.9, 85.0, 33.8),
          (13.3, 86.0, 27.4),
          (13.7, 71.0, 25.7),
          (13.8, 64.0, 24.9),
          (14.0, 78.0, 34.5),
          (14.2, 80.0, 31.7),
          (14.5, 74.0, 36.3),
          (16.0, 72.0, 38.3),
          (16.3, 77.0, 42.6),
          (17.3, 81.0, 55.4),
          (17.5, 82.0, 55.7),
          (17.9, 80.0, 58.3),
          (18.0, 80.0, 51.5),
          (18.0, 80.0, 51.0),
          (20.6, 87.0, 77.0)
      ) AS drvd(`Girth`, `Height`, `Volume`)

# `sql_query_insert()` is correct

    Code
      sql_query_insert(con = simulate_mssql(), x_name = ident("df_x"), y = df_y, by = c(
        "a", "b"), conflict = "ignore", returning_cols = c("a", b2 = "b"))
    Output
      <SQL> INSERT INTO `df_x` (`a`, `b`, `c`, `d`)
      OUTPUT `INSERTED`.`a`, `INSERTED`.`b` AS `b2`
      SELECT *
      FROM (
        SELECT `a`, `b`, `c` + 1.0 AS `c`, `d`
        FROM `df_y`
      ) `...y`
      WHERE NOT EXISTS (
        SELECT 1 FROM `df_x`
        WHERE (`df_x`.`a` = `...y`.`a`) AND (`df_x`.`b` = `...y`.`b`)
      )

# `sql_query_append()` is correct

    Code
      sql_query_append(con = simulate_mssql(), x_name = ident("df_x"), y = df_y,
      returning_cols = c("a", b2 = "b"))
    Output
      <SQL> INSERT INTO `df_x` (`a`, `b`, `c`, `d`)
      OUTPUT `INSERTED`.`a`, `INSERTED`.`b` AS `b2`
      SELECT *
      FROM (
        SELECT `a`, `b`, `c` + 1.0 AS `c`, `d`
        FROM `df_y`
      ) `...y`

# `sql_query_update_from()` is correct

    Code
      sql_query_update_from(con = simulate_mssql(), x_name = ident("df_x"), y = df_y,
      by = c("a", "b"), update_values = sql(c = "COALESCE(`df_x`.`c`, `...y`.`c`)",
        d = "`...y`.`d`"), returning_cols = c("a", b2 = "b"))
    Output
      <SQL> UPDATE `df_x`
      SET `c` = COALESCE(`df_x`.`c`, `...y`.`c`), `d` = `...y`.`d`
      OUTPUT `INSERTED`.`a`, `INSERTED`.`b` AS `b2`
      FROM `df_x`
      INNER JOIN (
        SELECT `a`, `b`, `c` + 1.0 AS `c`, `d`
        FROM `df_y`
      ) `...y`
        ON `...y`.`a` = `df_x`.`a` AND `...y`.`b` = `df_x`.`b`

# `sql_query_delete()` is correct

    Code
      sql_query_delete(con = simulate_mssql(), x_name = ident("df_x"), y = df_y, by = c(
        "a", "b"), returning_cols = c("a", b2 = "b"))
    Output
      <SQL> DELETE FROM `df_x`
      OUTPUT `DELETED`.`a`, `DELETED`.`b` AS `b2`
      WHERE EXISTS (
        SELECT 1 FROM (
        SELECT `a`, `b`, `c` + 1.0 AS `c`, `d`
        FROM `df_y`
      ) `...y`
        WHERE (`...y`.`a` = `df_x`.`a`) AND (`...y`.`b` = `df_x`.`b`)
      )

# `sql_query_upsert()` is correct

    Code
      sql_query_upsert(con = simulate_mssql(), x_name = ident("df_x"), y = df_y, by = c(
        "a", "b"), update_cols = c("c", "d"), returning_cols = c("a", b2 = "b"))
    Output
      <SQL> MERGE INTO `df_x`
      USING (
        SELECT `a`, `b`, `c` + 1.0 AS `c`, `d`
        FROM `df_y`
      ) `...y`
        ON `...y`.`a` = `df_x`.`a` AND `...y`.`b` = `df_x`.`b`
      WHEN MATCHED THEN
        UPDATE SET `c` = `...y`.`c`, `d` = `...y`.`d`
      WHEN NOT MATCHED THEN
        INSERT (`a`, `b`, `c`, `d`)
        VALUES (`...y`.`a`, `...y`.`b`, `...y`.`c`, `...y`.`d`)
      OUTPUT `INSERTED`.`a`, `INSERTED`.`b` AS `b2`
      ;

