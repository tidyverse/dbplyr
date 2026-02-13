# unnamed results must be data frames

    Code
      do(mf, nrow(.))
    Condition
      Error in `label_output_dataframe()`:
      ! Results must be data frames
      Problems at positions 1 and 2

# do() argument checking works

    Code
      do(mf, nrow = nrow(.), ncol(.))
    Condition
      Error in `named_args()`:
      ! Arguments to `do()` must either be all named or all unnamed

---

    Code
      do(mf, nrow(.), ncol(.))
    Condition
      Error in `named_args()`:
      ! Can only supply single unnamed argument to `do()`

---

    Code
      do(mf, .f = nrow)
    Condition
      Error in `named_args()`:
      ! `do()` syntax changed in dplyr 0.2. Please see documentation for details

# do() is deprecated

    Code
      . <- do(local_memdb_frame(x = 1), head(.))
    Condition
      Warning:
      `do()` was deprecated in dbplyr 2.6.0.

