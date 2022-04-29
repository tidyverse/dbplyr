# symbols weights are dropped in output

    Code
      dbplyr_uncount(df, w) %>% show_query()
    Output
      <SQL>
      SELECT `x`
      FROM (
        SELECT `x`, `w`, `..dbplyr_row_id`
        FROM `test` AS `LHS`
        INNER JOIN (
          SELECT CAST(`..dbplyr_row_id` AS INTEGER) AS `..dbplyr_row_id`
          FROM (
            SELECT NULL AS `..dbplyr_row_id`
            WHERE (0 = 1)
            UNION ALL
            VALUES (1)
          ) AS `values_table`
        ) AS `RHS`
          ON (`RHS`.`..dbplyr_row_id` <= `LHS`.`w`)
      )

