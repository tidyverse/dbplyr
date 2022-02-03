# symbols weights are dropped in output

    Code
      dbplyr_uncount(df, w) %>% show_query()
    Output
      <SQL>
      SELECT `x`
      FROM (
        SELECT `x`, `w`, `..dbplyr_row_id`
        FROM `test` AS `LHS`
        INNER JOIN `dbplyr_table` AS `RHS`
          ON (`RHS`.`..dbplyr_row_id` <= `LHS`.`w`)
      )

