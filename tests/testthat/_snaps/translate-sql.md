# dplyr.strict_sql = TRUE prevents auto conversion

    Code
      translate_sql(blah(x))
    Condition
      Error in `blah()`:
      ! Don't know how to translate `blah()`
    Code
      translate_sql(x %blah% y)
    Condition
      Error in `x %blah% y`:
      ! Don't know how to translate `%blah%`

# namespace calls are translated

    Code
      translate_sql(NOSUCHPACKAGE::foo())
    Condition
      Error:
      ! There is no package called NOSUCHPACKAGE

---

    Code
      translate_sql(dbplyr::NOSUCHFUNCTION())
    Condition
      Error:
      ! "NOSUCHFUNCTION" is not an exported object from dbplyr

---

    Code
      translate_sql(base::abbreviate(x))
    Condition
      Error:
      ! No known translation for `base::abbreviate()`

