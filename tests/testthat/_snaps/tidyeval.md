# old arguments are defunct

    Code
      partial_eval(quote(x), vars = c("x", "y"))
    Condition
      Error:
      ! The `vars` argument of `partial_eval()` was deprecated in dbplyr 2.1.2 and is now defunct.
    Code
      partial_eval(quote(x), data = c("x", "y"))
    Condition
      Error:
      ! The `data` argument of `partial_eval()` must be a lazy frame as of dbplyr 2.1.2.

