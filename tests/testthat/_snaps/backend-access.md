# custom scalar translated correctly

    Code
      test_translate_sql(paste(x, collapse = "-"))
    Condition
      Error in `check_collapse()`:
      ! `collapse` not supported in DB translation of `paste()`.
      i Please use `str_flatten()` instead.

# queries translate correctly

    Code
      mf %>% head()
    Output
      <SQL>
      SELECT TOP 6 `df`.*
      FROM `df`

