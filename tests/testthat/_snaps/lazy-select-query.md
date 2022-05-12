# can print lazy_select_query

    Code
      lazy_select_query(x = lazy_query_local(tibble(x = 1, y = 2), "df"), last_op = "select",
      select = quos(x_mean = mean(x), y2 = y), where = quos(y > 1, x == y - 2),
      group_by = quos("x"))
    Output
      <SQL SELECT>
      From:
        <IDENT> df
      Select:   x_mean = mean(x), y2 = y
      Where:    y > 1, x == y - 2
      Group by: "x"

