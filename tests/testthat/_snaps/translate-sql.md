# dplyr.strict_sql = TRUE prevents auto conversion

    Code
      translate_sql(blah(x), con = con)
    Condition
      Error in `blah()`:
      ! Don't know how to translate `blah()`
    Code
      translate_sql(x %blah% y, con = con)
    Condition
      Error in `x %blah% y`:
      ! Don't know how to translate `%blah%`

# namespace calls are translated

    Code
      translate_sql(NOSUCHPACKAGE::foo(), con = con)
    Condition
      Error:
      ! There is no package called NOSUCHPACKAGE
    Code
      translate_sql(dbplyr::NOSUCHFUNCTION(), con = con)
    Condition
      Error:
      ! "NOSUCHFUNCTION" is not an exported object from dbplyr
    Code
      translate_sql(base::abbreviate(x), con = con)
    Condition
      Error in `base::abbreviate()`:
      ! No known SQL translation

---

    Code
      mutate(lz, x = NOSUCHPACKAGE::foo())
    Condition
      Error:
      ! There is no package called NOSUCHPACKAGE
    Code
      mutate(lz, x = dbplyr::NOSUCHFUNCTION())
    Condition
      Error:
      ! "NOSUCHFUNCTION" is not an exported object from dbplyr
    Code
      mutate(lz, x = base::abbreviate(x))
    Condition
      Error in `base::abbreviate()`:
      ! No known SQL translation

