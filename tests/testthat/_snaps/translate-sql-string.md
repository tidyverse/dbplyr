# sql_substr works as expected

    Code
      translate_sql(substr("test"), con = con)
    Condition
      Error in `substr()`:
      ! `start` must be a whole number, not absent.

---

    Code
      translate_sql(substr("test", 0), con = con)
    Condition
      Error in `substr()`:
      ! `stop` must be a whole number, not absent.

---

    Code
      translate_sql(substr("test", "x", 1), con = con)
    Condition
      Error in `substr()`:
      ! `start` must be a whole number, not the string "x".

---

    Code
      translate_sql(substr("test", 1, "x"), con = con)
    Condition
      Error in `substr()`:
      ! `stop` must be a whole number, not the string "x".

