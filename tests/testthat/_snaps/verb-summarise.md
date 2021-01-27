# summarise(.groups=)

    Code
      df %>% summarise()
    Message <message>
      `summarise()` has grouped output by 'x'. You can override using the `.groups` argument.
    Output
      <SQL>
      SELECT `x`, `y`
      FROM `df`
      GROUP BY `x`, `y`

---

    `.groups` can't be "rowwise" for lazy tables
    i Possible values are NULL (default), "drop_last", "drop", and "keep"

# quoting for rendering summarized grouped table

    Code
      out %>% sql_render
    Output
      <SQL> SELECT `x`, COUNT(*) AS `n`
      FROM `verb-summarise`
      GROUP BY `x`

