# across() captures anonymous functions

    Code
      (expect_error(capture_across(lf, across(a, function(x) {
        x <- x + 2
        log(x)
      }))))
    Output
      <error/rlang_error>
      Error in `across()`:
      ! Cannot translate functions consisting of more than one statement.

# across() gives informative errors

    Code
      capture_across(lf, across(a, 1))
    Condition
      Error in `across()`:
      ! `.fns` must be a function, a formula, or list of functions/formulas.
    Code
      capture_across(lf, across(a, list(1)))
    Condition
      Error in `across()`:
      ! `.fns` must contain a function or a formula.
      x Problem with 1
    Code
      capture_across(lf, across(a:b, "log"))
    Condition
      Error in `across()`:
      ! `.fns` must be a function, a formula, or list of functions/formulas.
    Code
      capture_across(lf, across(c, mean))
    Condition
      Error in `across()`:
      ! Can't select columns that don't exist.
      x Column `c` doesn't exist.

# across() defaults to everything()

    Code
      summarise(lazy_frame(x = 1, y = 1), across(.fns = ~ . + 1))
    Output
      <SQL>
      SELECT `x` + 1.0 AS `x`, `y` + 1.0 AS `y`
      FROM `df`

# untranslatable functions are preserved

    Code
      summarise(lf, across(a:b, SQL_LOG))
    Output
      <SQL>
      SELECT SQL_LOG(`a`) AS `a`, SQL_LOG(`b`) AS `b`
      FROM `df`

# old _at functions continue to work

    Code
      dplyr::summarise_at(lf, dplyr::vars(a:b), "sum")
    Condition
      Warning:
      Missing values are always removed in SQL aggregation functions.
      Use `na.rm = TRUE` to silence this warning
      This warning is displayed once every 8 hours.
    Output
      <SQL>
      SELECT SUM(`a`) AS `a`, SUM(`b`) AS `b`
      FROM `df`

---

    Code
      dplyr::summarise_at(lf, dplyr::vars(a:b), sum)
    Output
      <SQL>
      SELECT SUM(`a`) AS `a`, SUM(`b`) AS `b`
      FROM `df`

---

    Code
      dplyr::summarise_at(lf, dplyr::vars(a:b), ~ sum(.))
    Output
      <SQL>
      SELECT SUM(`a`) AS `a`, SUM(`b`) AS `b`
      FROM `df`

# across() errors if named

    Code
      (expect_error(mutate(lf, x = across())))
    Output
      <error/rlang_error>
      Error in `mutate()`:
      ! In dbplyr, the result of `across()` must be unnamed.
      i `x = across()` is named.
    Code
      (expect_error(group_by(lf, x = across())))
    Output
      <error/rlang_error>
      Error in `group_by()`:
      ! In dbplyr, the result of `across()` must be unnamed.
      i `x = across()` is named.

# across() throws error if unpack = TRUE

    Code
      (expect_error(mutate(lf, across(x, .unpack = TRUE))))
    Output
      <error/rlang_error>
      Error in `mutate()`:
      i In argument: `across(x, .unpack = TRUE)`
      Caused by error in `mutate()`:
      ! `.unpack = TRUE` isn't supported on database backends.
      i It must be FALSE instead.

# where() isn't suppored

    Code
      capture_across(lf, across(where(is.integer), as.character))
    Condition
      Error in `across()`:
      ! This tidyselect interface doesn't support predicates.

# if_all() gives informative errors

    Code
      capture_if_all(lf, if_all(a, 1))
    Condition
      Error in `if_all()`:
      ! `.fns` must be a function, a formula, or list of functions/formulas.
    Code
      capture_if_all(lf, if_all(a, list(1)))
    Condition
      Error in `if_all()`:
      ! `.fns` must contain a function or a formula.
      x Problem with 1

# if_all/any works in filter()

    Code
      filter(lf, if_all(a:b, ~ . > 0))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE ((`a` > 0.0 AND `b` > 0.0))

---

    Code
      filter(lf, if_any(a:b, ~ . > 0))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE ((`a` > 0.0 OR `b` > 0.0))

# if_all/any works in mutate()

    Code
      mutate(lf, c = if_all(a:b, ~ . > 0))
    Output
      <SQL>
      SELECT *, (`a` > 0.0 AND `b` > 0.0) AS `c`
      FROM `df`

---

    Code
      mutate(lf, c = if_any(a:b, ~ . > 0))
    Output
      <SQL>
      SELECT *, (`a` > 0.0 OR `b` > 0.0) AS `c`
      FROM `df`

# if_all/any uses every column as default

    Code
      filter(lf, if_all(.fns = ~ . > 0))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE ((`a` > 0.0 AND `b` > 0.0))

---

    Code
      filter(lf, if_any(.fns = ~ . > 0))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE ((`a` > 0.0 OR `b` > 0.0))

# if_all/any works without `.fns` argument

    Code
      filter(lf, if_all(a:b))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE ((`a` AND `b`))

---

    Code
      filter(lf, if_any(a:b))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE ((`a` OR `b`))

# if_all() cannot rename variables

    Code
      (expect_error(capture_if_all(lf, if_all(c(a = x, b = y)))))
    Output
      <error/tidyselect:::error_disallowed_rename>
      Error in `if_all()`:
      ! Can't rename variables in this context.

# across(...) is deprecated

    Code
      summarise(lf, across(everything(), mean, na.rm = TRUE))
    Condition
      Warning:
      The `...` argument of `across()` is deprecated as of dbplyr 2.3.0.
      i Supply arguments directly to `.fns` through a lambda instead.
      
      # Previously across(a:b, mean, na.rm = TRUE)
      
      # Now across(a:b, ~mean(.x, na.rm = TRUE))
    Output
      <SQL>
      SELECT AVG(`x`) AS `x`
      FROM `df`

# across() does not support formulas with dots

    Code
      (expect_error(capture_across(lf, across(a:b, ~ log(.x, base = .y), base = 2))))
    Output
      <error/rlang_error>
      Error in `across()`:
      ! Can't use `...` when a purrr-style lambda is used in `.fns`.
      i Use a lambda instead.
      i Or inline them via a purrr-style lambda.
    Code
      (expect_error(capture_across(lf, across(a:b, list(~ log(.x, base = .y)), base = 2)))
      )
    Output
      <error/rlang_error>
      Error in `across()`:
      ! Can't use `...` when a purrr-style lambda is used in `.fns`.
      i Use a lambda instead.
      i Or inline them via a purrr-style lambda.

# `pick()` errors in `arrange()` are useful

    Code
      arrange(df, pick(y))
    Condition
      Error in `arrange()`:
      i In argument: `pick(y)`
      Caused by error in `pick()`:
      ! Can't select columns that don't exist.
      x Column `y` doesn't exist.

# doesn't allow renaming

    Code
      arrange(lazy_frame(x = 1), pick(y = x))
    Condition
      Error in `arrange()`:
      i In argument: `pick(y = x)`
      Caused by error in `pick()`:
      ! Can't rename variables in this context.

# requires at least one input

    Code
      arrange(lazy_frame(x = 1), pick())
    Condition
      Error in `arrange()`:
      i In argument: `pick()`
      Caused by error in `partial_eval_pick()`:
      ! Must supply at least one input to `pick()`.

# `filter()` with `pick()` that uses invalid tidy-selection errors

    Code
      filter(df, pick(x, a))
    Condition
      Error in `filter()`:
      i In argument: `pick(x, a)`
      Caused by error in `pick()`:
      ! Can't select columns that don't exist.
      x Column `a` doesn't exist.

