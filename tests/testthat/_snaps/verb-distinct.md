# distinct respects window_order when .keep_all is TRUE

    Code
      lf %>% window_order(desc(y)) %>% distinct(x, .keep_all = TRUE)
    Output
      <SQL>
      SELECT `x`, `y`
      FROM (
        SELECT *, ROW_NUMBER() OVER (PARTITION BY `x` ORDER BY `y` DESC) AS `q01`
        FROM `df`
      ) `q01`
      WHERE (`q01` = 1)

