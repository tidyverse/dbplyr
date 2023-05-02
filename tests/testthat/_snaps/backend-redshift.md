# copy_inline uses UNION ALL

    Code
      copy_inline(con, y %>% slice(0)) %>% remote_query()
    Output
      <SQL> SELECT CAST(NULL AS INTEGER) AS `id`, CAST(NULL AS TEXT) AS `arr`
      WHERE (0 = 1)
    Code
      copy_inline(con, y) %>% remote_query()
    Output
      <SQL> SELECT CAST(`id` AS INTEGER) AS `id`, CAST(`arr` AS TEXT) AS `arr`
      FROM (
        SELECT NULL AS `id`, NULL AS `arr`
        WHERE (0 = 1)
      
        UNION ALL
      
        SELECT 1, '{1,2,3}'
      ) `values_table`
    Code
      copy_inline(con, y %>% slice(0), types = types) %>% remote_query()
    Output
      <SQL> SELECT CAST(NULL AS bigint) AS `id`, CAST(NULL AS integer[]) AS `arr`
      WHERE (0 = 1)
    Code
      copy_inline(con, y, types = types) %>% remote_query()
    Output
      <SQL> SELECT CAST(`id` AS bigint) AS `id`, CAST(`arr` AS integer[]) AS `arr`
      FROM (
        SELECT NULL AS `id`, NULL AS `arr`
        WHERE (0 = 1)
      
        UNION ALL
      
        SELECT 1, '{1,2,3}'
      ) `values_table`

