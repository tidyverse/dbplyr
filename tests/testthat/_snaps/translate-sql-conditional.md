# case_when converted to CASE WHEN

    Code
      test_translate_sql(case_when(x > 1L ~ "a"))
    Output
      <SQL> CASE WHEN (`x` > 1) THEN 'a' END

# even inside mutate

    Code
      out$select[[2]]
    Output
      [1] "CASE WHEN (`x` > 1) THEN 'a' END"

# case_when translates correctly to ELSE when TRUE ~ is used 2

    Code
      test_translate_sql(case_when(x == 1L ~ "yes", x == 0L ~ "no", TRUE ~
      "undefined"))
    Output
      <SQL> CASE WHEN (`x` = 1) THEN 'yes' WHEN (`x` = 0) THEN 'no' ELSE 'undefined' END

# case_when uses the .default arg

    Code
      test_translate_sql(case_when(x == 1L ~ "yes", x == 0L ~ "no", .default = "undefined"))
    Output
      <SQL> CASE WHEN (`x` = 1) THEN 'yes' WHEN (`x` = 0) THEN 'no' ELSE 'undefined' END

---

    Code
      test_translate_sql(case_when(x == 1L ~ "yes", x == 0L ~ "no", .default = x + 1))
    Output
      <SQL> CASE WHEN (`x` = 1) THEN 'yes' WHEN (`x` = 0) THEN 'no' ELSE `x` + 1.0 END

---

    Code
      test_translate_sql(case_when(x == 1L ~ "yes", x == 0L ~ "no", TRUE ~ "true",
      .default = "undefined"))
    Output
      <SQL> CASE WHEN (`x` = 1) THEN 'yes' WHEN (`x` = 0) THEN 'no' ELSE 'true' END

# case_when does not support .ptype and .size

    Code
      (expect_error(test_translate_sql(case_when(x == 1L ~ "yes", .ptype = character())))
      )
    Output
      <error/dbplyr_error_unsupported_arg>
      Error in `case_when()`:
      ! Argument `.ptype` isn't supported on database backends.
    Code
      (expect_error(test_translate_sql(case_when(x == 1L ~ "yes", .size = 1))))
    Output
      <error/dbplyr_error_unsupported_arg>
      Error in `case_when()`:
      ! Argument `.size` isn't supported on database backends.

# long case_when is on multiple lines

    Code
      test_translate_sql(case_when(x == 1L ~ "this is long", x == 0L ~ "so it should",
      TRUE ~ "be wrapped"))
    Output
      <SQL> CASE
      WHEN (`x` = 1) THEN 'this is long'
      WHEN (`x` = 0) THEN 'so it should'
      ELSE 'be wrapped'
      END

# conditionals check arguments

    Code
      test_translate_sql(case_when())
    Condition
      Error in `sql_case_when()`:
      ! No cases provided

---

    Code
      test_translate_sql(switch(x, 1L, 2L))
    Condition
      Error in `sql_switch()`:
      ! Can only have one unnamed (ELSE) input

# LHS can handle bang bang

    Code
      test_translate_sql(case_match(x, !!1L ~ "x"))
    Output
      <SQL> CASE WHEN (`x` IN (1)) THEN 'x' END
    Code
      test_translate_sql(case_match(x, !!c(1L, 2L) ~ "x"))
    Output
      <SQL> CASE WHEN (`x` IN (1, 2)) THEN 'x' END
    Code
      test_translate_sql(case_match(x, !!c(NA, 1L) ~ "x"))
    Output
      <SQL> CASE WHEN (`x` IN (1) OR `x` IS NULL) THEN 'x' END

# requires at least one condition

    Code
      test_translate_sql(case_match(x))
    Condition
      Error in `case_match()`:
      ! No cases provided

---

    Code
      test_translate_sql(case_match(x, NULL))
    Condition
      Error in `case_match()`:
      ! No cases provided

# `.ptype` not supported

    Code
      (expect_error(test_translate_sql(case_match(x, 1 ~ 1, .ptype = integer()))))
    Output
      <error/dbplyr_error_unsupported_arg>
      Error in `case_match()`:
      ! Argument `.ptype` isn't supported on database backends.

# .x must be a symbol

    Code
      (expect_error(test_translate_sql(case_match(1, 1 ~ 1))))
    Output
      <error/rlang_error>
      Error in `case_match()`:
      ! `.x` must be a variable or function call, not a number.

