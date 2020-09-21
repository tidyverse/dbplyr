# case_when converted to CASE WHEN

    Code
      translate_sql(case_when(x > 1L ~ "a"))
    Output
      <SQL> CASE
      WHEN (`x` > 1) THEN ('a')
      END

# even inside mutate

    Code
      out$select[[2]]
    Output
      [1] "CASE\nWHEN (`x` > 1) THEN ('a')\nEND"

# case_when translates correctly to ELSE when TRUE ~ is used 2

    Code
      translate_sql(case_when(x == 1L ~ "yes", x == 0L ~ "no", TRUE ~ "undefined"))
    Output
      <SQL> CASE
      WHEN (`x` = 1) THEN ('yes')
      WHEN (`x` = 0) THEN ('no')
      ELSE ('undefined')
      END

