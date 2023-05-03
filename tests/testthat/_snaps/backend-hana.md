# custom string translations

    Code
      translate_sql(paste0("a", "b"))
    Output
      <SQL> 'a' || 'b'

---

    Code
      translate_sql(paste("a", "b"))
    Output
      <SQL> 'a' || ' ' || 'b'

---

    Code
      translate_sql(substr(x, 2, 4))
    Output
      <SQL> SUBSTRING(`x`, 2, 3)

---

    Code
      translate_sql(substring(x, 2, 4))
    Output
      <SQL> SUBSTRING(`x`, 2, 3)

---

    Code
      translate_sql(str_sub(x, 2, -2))
    Output
      <SQL> SUBSTRING(`x`, 2, LENGTH(`x`) - 2)

# copy_inline uses UNION ALL

    Code
      copy_inline(con, y %>% slice(0)) %>% remote_query()
    Output
      <SQL> SELECT CAST(NULL AS INTEGER) AS `id`, CAST(NULL AS VARCHAR) AS `arr`
      FROM `DUMMY`
      WHERE (0 = 1)
    Code
      copy_inline(con, y) %>% remote_query()
    Output
      <SQL> SELECT CAST(`id` AS INTEGER) AS `id`, CAST(`arr` AS VARCHAR) AS `arr`
      FROM (
        SELECT NULL AS `id`, NULL AS `arr`
        FROM `DUMMY`
        WHERE (0 = 1)
      
        UNION ALL
      
        SELECT 1, '{1,2,3}' FROM DUMMY
      ) `values_table`
    Code
      copy_inline(con, y %>% slice(0), types = types) %>% remote_query()
    Output
      <SQL> SELECT CAST(NULL AS bigint) AS `id`, CAST(NULL AS integer[]) AS `arr`
      FROM `DUMMY`
      WHERE (0 = 1)
    Code
      copy_inline(con, y, types = types) %>% remote_query()
    Output
      <SQL> SELECT CAST(`id` AS bigint) AS `id`, CAST(`arr` AS integer[]) AS `arr`
      FROM (
        SELECT NULL AS `id`, NULL AS `arr`
        FROM `DUMMY`
        WHERE (0 = 1)
      
        UNION ALL
      
        SELECT 1, '{1,2,3}' FROM DUMMY
      ) `values_table`

