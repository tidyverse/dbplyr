# sql_substr works as expected

    Code
      substr("test")
    Condition
      Error in `substr()`:
      ! `start` must be a whole number, not absent.

---

    Code
      substr("test", 0)
    Condition
      Error in `substr()`:
      ! `stop` must be a whole number, not absent.

---

    Code
      substr("test", "x", 1)
    Condition
      Error in `substr()`:
      ! `start` must be a whole number, not the string "x".

---

    Code
      substr("test", 1, "x")
    Condition
      Error in `substr()`:
      ! `stop` must be a whole number, not the string "x".

