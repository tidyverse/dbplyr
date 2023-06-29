# distinct() produces optimized SQL

    Code
      (out <- lf %>% head(2) %>% distinct(x, y))
    Output
      <SQL>
      SELECT DISTINCT `q01`.*
      FROM (
        SELECT `df`.*
        FROM `df`
        LIMIT 2
      ) AS `q01`

# distinct respects window_order when .keep_all is TRUE

    Code
      lf %>% window_order(desc(y)) %>% distinct(x, .keep_all = TRUE)
    Output
      <SQL>
      SELECT `x`, `y`
      FROM (
        SELECT
          `df`.*,
          ROW_NUMBER() OVER (PARTITION BY `x` ORDER BY `y` DESC) AS `col01`
        FROM `df`
      ) AS `q01`
      WHERE (`col01` = 1)

