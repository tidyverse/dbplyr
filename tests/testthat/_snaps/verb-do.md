# unnamed results must be data frames

    Code
      mf %>% do(nrow(.))
    Condition
      Error in `label_output_dataframe()`:
      ! Results must be data frames
      Problems at positions 1 and 2

# named argument become list columns

    Code
      mf %>% do(nrow = nrow(.), ncol(.))
    Condition
      Error in `named_args()`:
      ! Arguments to `do()` must either be all named or all unnamed

---

    Code
      mf %>% do(nrow(.), ncol(.))
    Condition
      Error in `named_args()`:
      ! Can only supply single unnamed argument to `do()`

---

    Code
      mf %>% do(.f = nrow)
    Condition
      Error in `named_args()`:
      ! `do()` syntax changed in dplyr 0.2. Please see documentation for details

