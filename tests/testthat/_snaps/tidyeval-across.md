# across() captures anonymous functions

    Code
      (expect_error(capture_across(lf, across(a, function(x) {
        x <- x + 2
        log(x)
      }))))
    Output
      <error/rlang_error>
      Error in `partial_eval_fun()`:
      ! Cannot translate functions consisting of more than one statement.

# across() does not support formulas with dots

    Code
      (expect_error(capture_across(lf, across(a:b, ~ log(.x, base = .y), base = 2))))
    Output
      <error/rlang_error>
      Error in `across_fun()`:
      ! `dbplyr::across()` does not support `...` when a purrr-style lambda is used in `.fns`.
      i Use a lambda instead.
      i Or inline them via a purrr-style lambda.
    Code
      (expect_error(capture_across(lf, across(a:b, list(~ log(.x, base = .y)), base = 2)))
      )
    Output
      <error/rlang_error>
      Error in `FUN()`:
      ! `dbplyr::across()` does not support `...` when a purrr-style lambda is used in `.fns`.
      i Use a lambda instead.
      i Or inline them via a purrr-style lambda.

# across() gives informative errors

    Code
      capture_across(lf, across(a, 1))
    Condition
      Error in `across_funs()`:
      ! `.fns` argument to `dbplyr::across()` must be a NULL, a function, formula, or list
    Code
      capture_across(lf, across(a, list(1)))
    Condition
      Error in `FUN()`:
      ! `.fns` argument to `dbplyr::across()` must contain a function or a formula
      x Problem with 1
    Code
      capture_across(lf, across(a:b, "log"))
    Condition
      Error in `across_funs()`:
      ! `.fns` argument to `dbplyr::across()` must be a NULL, a function, formula, or list
    Code
      capture_across(lf, across(c, mean))
    Condition
      Error in `chr_as_locations()`:
      ! Can't subset columns that don't exist.
      x Column `c` doesn't exist.

# across() defaults to everything()

    Code
      lazy_frame(x = 1, y = 1) %>% summarise(across(.fns = ~ . + 1))
    Output
      <SQL>
      SELECT `x` + 1.0 AS `x`, `y` + 1.0 AS `y`
      FROM `df`

# untranslatable functions are preserved

    Code
      lf %>% summarise(across(a:b, SQL_LOG))
    Output
      <SQL>
      SELECT SQL_LOG(`a`) AS `a`, SQL_LOG(`b`) AS `b`
      FROM `df`

# old _at functions continue to work

    Code
      lf %>% dplyr::summarise_at(dplyr::vars(a:b), "sum")
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
      lf %>% dplyr::summarise_at(dplyr::vars(a:b), sum)
    Output
      <SQL>
      SELECT SUM(`a`) AS `a`, SUM(`b`) AS `b`
      FROM `df`

---

    Code
      lf %>% dplyr::summarise_at(dplyr::vars(a:b), ~ sum(.))
    Output
      <SQL>
      SELECT SUM(`a`) AS `a`, SUM(`b`) AS `b`
      FROM `df`

# if_all() gives informative errors

    Code
      capture_if_all(lf, if_all(a, 1))
    Condition
      Error in `across_funs()`:
      ! `.fns` argument to `dbplyr::across()` must be a NULL, a function, formula, or list
    Code
      capture_if_all(lf, if_all(a, list(1)))
    Condition
      Error in `FUN()`:
      ! `.fns` argument to `dbplyr::across()` must contain a function or a formula
      x Problem with 1

# if_all/any works in filter()

    Code
      lf %>% filter(if_all(a:b, ~ . > 0))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`a` > 0.0 AND `b` > 0.0)

---

    Code
      lf %>% filter(if_any(a:b, ~ . > 0))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`a` > 0.0 OR `b` > 0.0)

# if_all/any works in mutate()

    Code
      lf %>% mutate(c = if_all(a:b, ~ . > 0))
    Output
      <SQL>
      SELECT *, `a` > 0.0 AND `b` > 0.0 AS `c`
      FROM `df`

---

    Code
      lf %>% mutate(c = if_any(a:b, ~ . > 0))
    Output
      <SQL>
      SELECT *, `a` > 0.0 OR `b` > 0.0 AS `c`
      FROM `df`

# if_all/any uses every colum as default

    Code
      lf %>% filter(if_all(.fns = ~ . > 0))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`a` > 0.0 AND `b` > 0.0)

---

    Code
      lf %>% filter(if_any(.fns = ~ . > 0))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`a` > 0.0 OR `b` > 0.0)

# if_all/any works without `.fns` argument

    Code
      lf %>% filter(if_all(a:b))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`a` AND `b`)

---

    Code
      lf %>% filter(if_any(a:b))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`a` OR `b`)

# if_all() cannot rename variables

    Code
      (expect_error(capture_if_all(lf, if_all(c(a = x, b = y)))))
    Output
      <error/rlang_error>
      Error in `across_setup()`:
      ! Can't rename variables in this context.

