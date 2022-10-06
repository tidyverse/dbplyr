# case_when converted to CASE WHEN

    Code
      translate_sql(case_when(x > 1L ~ "a"))
    Output
      <SQL> CASE WHEN (`x` > 1) THEN 'a' END

# even inside mutate

    Code
      out$select[[2]]
    Output
      [1] "CASE WHEN (`x` > 1) THEN 'a' END"

# case_when translates correctly to ELSE when TRUE ~ is used 2

    Code
      translate_sql(case_when(x == 1L ~ "yes", x == 0L ~ "no", TRUE ~ "undefined"))
    Output
      <SQL> CASE WHEN (`x` = 1) THEN 'yes' WHEN (`x` = 0) THEN 'no' ELSE 'undefined' END

# long case_when is on multiple lines

    Code
      translate_sql(case_when(x == 1L ~ "this is long", x == 0L ~ "so it should",
      TRUE ~ "be wrapped"))
    Output
      <SQL> CASE
      WHEN (`x` = 1) THEN 'this is long'
      WHEN (`x` = 0) THEN 'so it should'
      ELSE 'be wrapped'
      END

# conditionals check arguments

    Code
      translate_sql(case_when())
    Condition
      Error in `sql_case_when()`:
      ! No cases provided

---

    Code
      translate_sql(switch(x, 1L, 2L))
    Condition
      Error in `sql_switch()`:
      ! Can only have one unnamed (ELSE) input

# LHS can match multiple values

    Code
      translate_sql(case_match(z, 1:2 ~ "z"))
    Output
      <SQL> CASE WHEN (`z` IN (1, 2)) THEN 'z' END

# requires at least one condition

    Code
      translate_sql(case_match(x))
    Condition
      Error in `sql_case_when()`:
      ! No cases provided

---

    Code
      translate_sql(case_match(x, NULL))
    Condition
      Error in `sql_case_when()`:
      ! No cases provided

# `.ptype` not supported

    Code
      (expect_error(translate_sql(case_match(x, 1 ~ 1, .ptype = integer()))))
    Output
      <error/rlang_error>
      Error in `case_match()`:
      ! `.ptype` is not supported by dbplyr.

