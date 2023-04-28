# distinct() produces optimized SQL

    Code
      (out <- lf %>% head(2) %>% distinct(x, y))
    Output
      <SQL>
      SELECT DISTINCT *
      FROM (
        SELECT *
        FROM `df`
        LIMIT 2
      ) `q01`

# distinct respects window_order when .keep_all is TRUE

    Code
      lf %>% window_order(desc(y)) %>% distinct(x, .keep_all = TRUE)
    Output
      <SQL>
      SELECT `x`, `y`
      FROM (
        SELECT *, ROW_NUMBER() OVER (PARTITION BY `x` ORDER BY `y` DESC) AS `q02`
        FROM `df`
      ) `q01`
      WHERE (`q02` = 1)

# distinct uses dummy window order when .keep_all is TRUE and no order is used

    Code
      lf %>% distinct(x, .keep_all = TRUE)
    Output
      <SQL>
      SELECT `x`, `y`
      FROM (
        SELECT *, ROW_NUMBER() OVER (PARTITION BY `x` ORDER BY `x`) AS `q02`
        FROM `df`
      ) `q01`
      WHERE (`q02` = 1)

