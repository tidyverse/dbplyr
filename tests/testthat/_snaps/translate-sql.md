# namespace calls are translated

    Code
      translate_sql(NOSUCHPACKAGE::foo())
    Condition
      Error:
      ! There is no package called 'NOSUCHPACKAGE'

---

    Code
      translate_sql(dbplyr::NOSUCHFUNCTION())
    Condition
      Error:
      ! 'NOSUCHFUNCTION' is not an exported object from 'dbplyr'

---

    Code
      translate_sql(base::abbreviate(x))
    Condition
      Error:
      ! No known translation for base::abbreviate()

# vars is deprecated

    Code
      translate_sql(sin(x), vars = c("x", "y"))
    Condition
      Error in `translate_sql()`:
      ! `vars` is deprecated. Please use db_squash() directly.

