# CTE quoting works right

    Code
      .
    Output
      <SQL> WITH `q01` AS (
        SELECT `x` * 2.0 AS `x`, `y`
        FROM `lf1`
      ),
      `q02` AS (
        SELECT 'x' AS `column_name`
        FROM `q01`
      ),
      `q03` AS (
        SELECT `x`, `y` * 2.0 AS `y`
        FROM `lf1`
      ),
      `q04` AS (
        SELECT 'y' AS `column_name`
        FROM `q03` AS `q01`
      ),
      `q05` AS (
        SELECT *
        FROM `q02`
      
        UNION ALL
      
        SELECT *
        FROM `q04`
      )
      SELECT `...1`.`column_name` AS `column_name`
      FROM `q05` AS `...1`
      LEFT JOIN `q05` AS `...2`
        ON (`...1`.`column_name` = `...2`.`column_name`)
      LEFT JOIN `q05` AS `...3`
        ON (`...1`.`column_name` = `...3`.`column_name`)

