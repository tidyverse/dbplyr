# slice, head and tail aren't available

    Code
      lf %>% slice()
    Condition
      Error in `slice()`:
      ! `slice()` is not supported on database backends

---

    Code
      lf %>% slice_head()
    Condition
      Error in `slice_head()`:
      ! `slice_head()` is not supported on database backends
      i Please use `slice_min()` instead

---

    Code
      lf %>% slice_tail()
    Condition
      Error in `slice_tail()`:
      ! `slice_tail()` is not supported on database backends
      i Please use `slice_max()` instead

# slice_min handles arguments

    Code
      db %>% slice_min()
    Condition
      Error in `slice_min()`:
      ! Argument `order_by` is missing, with no default.

---

    Code
      db %>% slice_min(id, prop = 0.5, with_ties = FALSE)
    Condition
      Error in `slice_by()`:
      ! Can only use `prop` when `with_ties = TRUE`

# slice_max orders in opposite order

    Code
      db %>% slice_max()
    Condition
      Error in `slice_max()`:
      ! Argument `order_by` is missing, with no default.

# slice_sample errors when expected

    Code
      db %>% slice_sample(replace = TRUE)
    Condition
      Error in `slice_sample()`:
      ! Sampling with replacement is not supported on database backends

---

    Code
      db %>% slice_sample(weight_by = x)
    Condition
      Error in `slice_sample()`:
      ! Weighted resampling is not supported on database backends

---

    Code
      db %>% slice_sample(prop = 0.5)
    Condition
      Error in `slice_sample()`:
      ! Sampling by `prop` is not supported on database backends

# check_slice_size checks for common issues

    Code
      lf %>% slice_sample(n = 1, prop = 1)
    Condition
      Error in `slice_sample()`:
      ! Must supply exactly one of `n` and `prop` arguments.

---

    Code
      lf %>% slice_sample(n = "a")
    Condition
      Error in `slice_sample()`:
      ! `n` must be a single number.

---

    Code
      lf %>% slice_sample(prop = "a")
    Condition
      Error in `slice_sample()`:
      ! `prop` must be a single number

---

    Code
      lf %>% slice_sample(n = -1)
    Condition
      Error in `slice_sample()`:
      ! `n` must be a non-missing positive number.

---

    Code
      lf %>% slice_sample(prop = -1)
    Condition
      Error in `slice_sample()`:
      ! `prop` must be a non-missing positive number.

