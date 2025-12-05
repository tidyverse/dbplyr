# slice, head and tail aren't available

    Code
      slice(lf)
    Condition
      Error in `slice()`:
      ! `slice()` is not supported on database backends.

---

    Code
      slice_head(lf)
    Condition
      Error in `slice_head()`:
      ! `slice_head()` is not supported on database backends.
      i Please use `slice_min()` instead.

---

    Code
      slice_tail(lf)
    Condition
      Error in `slice_tail()`:
      ! `slice_tail()` is not supported on database backends.
      i Please use `slice_max()` instead.

# slice_min handles arguments

    Code
      slice_min(db)
    Condition
      Error in `slice_min()`:
      ! `order_by` is absent but must be supplied.

---

    Code
      slice_min(db, id, prop = 0.5, with_ties = FALSE)
    Condition
      Error in `slice_by()`:
      ! Can only use `prop` when `with_ties = TRUE`

---

    Code
      slice_min(db, id, n = 1, na_rm = FALSE)
    Condition
      Error in `slice_min()`:
      ! `na_rm = FALSE` isn't supported on database backends.
      i It must be TRUE instead.

# slice_max orders in opposite order

    Code
      slice_max(db)
    Condition
      Error in `slice_max()`:
      ! `order_by` is absent but must be supplied.

---

    Code
      slice_max(db, id, n = 1, na_rm = FALSE)
    Condition
      Error in `slice_max()`:
      ! `na_rm = FALSE` isn't supported on database backends.
      i It must be TRUE instead.

# slice_* can use data masking pronouns

    Code
      slice_max(lf, x)
    Output
      <SQL>
      SELECT `x`, `id`
      FROM (
        SELECT *, RANK() OVER (ORDER BY `x` DESC) AS `col01`
        FROM `df`
      ) AS `q01`
      WHERE (`col01` <= 1)
    Code
      slice_max(lf, .data$x)
    Output
      <SQL>
      SELECT `x`, `id`
      FROM (
        SELECT *, RANK() OVER (ORDER BY `x` DESC) AS `col01`
        FROM `df`
      ) AS `q01`
      WHERE (`col01` <= 1)
    Code
      slice_max(lf, .data$x * .env$x)
    Output
      <SQL>
      SELECT `x`, `id`
      FROM (
        SELECT *, RANK() OVER (ORDER BY `x` * -1 DESC) AS `col01`
        FROM `df`
      ) AS `q01`
      WHERE (`col01` <= 1)

# slice_sample errors when expected

    Code
      slice_sample(db, replace = TRUE)
    Condition
      Error in `slice_sample()`:
      ! Sampling with replacement is not supported on database backends

---

    Code
      slice_sample(db, weight_by = x)
    Condition
      Error in `slice_sample()`:
      ! Weighted resampling is not supported on database backends

---

    Code
      slice_sample(db, prop = 0.5)
    Condition
      Error in `slice_sample()`:
      ! Sampling by `prop` is not supported on database backends

# check_slice_size checks for common issues

    Code
      slice_sample(lf, n = 1, prop = 1)
    Condition
      Error in `slice_sample()`:
      ! Must supply exactly one of `n` and `prop` arguments.

---

    Code
      slice_sample(lf, n = "a")
    Condition
      Error in `slice_sample()`:
      ! `n` must be a single number.

---

    Code
      slice_sample(lf, prop = "a")
    Condition
      Error in `slice_sample()`:
      ! `prop` must be a single number

---

    Code
      slice_sample(lf, n = -1)
    Condition
      Error in `slice_sample()`:
      ! `n` must be a non-missing positive number.

---

    Code
      slice_sample(lf, prop = -1)
    Condition
      Error in `slice_sample()`:
      ! `prop` must be a non-missing positive number.

# slice_helper `by` errors use correct error context and correct `by_arg`

    Code
      slice_min(gdf, order_by = x, by = x)
    Condition
      Error in `slice_min()`:
      ! Can't supply `by` when `data` is a grouped data frame.
    Code
      slice_max(gdf, order_by = x, by = x)
    Condition
      Error in `slice_max()`:
      ! Can't supply `by` when `data` is a grouped data frame.
    Code
      slice_sample(gdf, n = 1, by = x)
    Condition
      Error in `slice_sample()`:
      ! Can't supply `by` when `data` is a grouped data frame.

# slice_min/max can order by multiple columns

    Code
      slice_min(lf, tibble(x))
    Output
      <SQL>
      SELECT `x`, `y`
      FROM (
        SELECT *, RANK() OVER (ORDER BY `x`) AS `col01`
        FROM `df`
      ) AS `q01`
      WHERE (`col01` <= 1)
    Code
      slice_min(lf, tibble::tibble(x, y))
    Output
      <SQL>
      SELECT `x`, `y`
      FROM (
        SELECT *, RANK() OVER (ORDER BY `x`, `y`) AS `col01`
        FROM `df`
      ) AS `q01`
      WHERE (`col01` <= 1)
    Code
      slice_min(lf, data.frame(y, x))
    Output
      <SQL>
      SELECT `x`, `y`
      FROM (
        SELECT *, RANK() OVER (ORDER BY `y`, `x`) AS `col01`
        FROM `df`
      ) AS `q01`
      WHERE (`col01` <= 1)

---

    Code
      slice_max(lf, tibble(x))
    Output
      <SQL>
      SELECT `x`, `y`
      FROM (
        SELECT *, RANK() OVER (ORDER BY `x` DESC) AS `col01`
        FROM `df`
      ) AS `q01`
      WHERE (`col01` <= 1)
    Code
      slice_max(lf, tibble::tibble(x, y))
    Output
      <SQL>
      SELECT `x`, `y`
      FROM (
        SELECT *, RANK() OVER (ORDER BY `x` DESC, `y` DESC) AS `col01`
        FROM `df`
      ) AS `q01`
      WHERE (`col01` <= 1)
    Code
      slice_max(lf, data.frame(y, x))
    Output
      <SQL>
      SELECT `x`, `y`
      FROM (
        SELECT *, RANK() OVER (ORDER BY `y` DESC, `x` DESC) AS `col01`
        FROM `df`
      ) AS `q01`
      WHERE (`col01` <= 1)

# slice_min/max informs if order_by uses c()

    Can't use `c()` in `slice_min()`
    i Did you mean to use `tibble(x, y)` instead?

