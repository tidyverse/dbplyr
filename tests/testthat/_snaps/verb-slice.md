# slice, head and tail aren't available

    slice() is not suppoted on database backends

---

    slice_head() is not supported on database backends
    i Please use slice_min() instead

---

    slice_tail() is not supported on database backends
    i Please use slice_max() instead

# slice_min handles arguments

    Argument `order_by` is missing, with no default.

---

    Can only use `prop` when `with_ties = TRUE`

# slice_max orders in opposite order

    Argument `order_by` is missing, with no default.

# slice_sample errors when expected

    Sampling with replacement is not supported on database backends

---

    Weighted resampling is not supported on database backends

---

    Sampling by `prop` is not supported on database backends

# check_slice_size checks for common issues

    Must supply exactly one of `n` and `prop` arguments.

---

    `n` must be a single number.

---

    `prop` must be a single number

---

    `n` must be a non-missing positive number.

---

    `prop` must be a non-missing positive number.

