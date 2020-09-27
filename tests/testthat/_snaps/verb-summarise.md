# quoting for rendering summarized grouped table

    Code
      out %>% sql_render
    Output
      <SQL> SELECT `x`, COUNT(*) AS `n`
      FROM `verb-summarise`
      GROUP BY `x`

