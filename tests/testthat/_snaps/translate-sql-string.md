# sql_substr works as expected

    Code
      substr("test")
    Condition
      Error in `check_integer()`:
      ! argument "start" is missing, with no default

---

    Code
      substr("test", 0)
    Condition
      Error in `check_integer()`:
      ! argument "stop" is missing, with no default

---

    Code
      substr("test", "x", 1)
    Condition
      Error in `check_integer()`:
      ! `start` must be a single number

---

    Code
      substr("test", 1, "x")
    Condition
      Error in `check_integer()`:
      ! `stop` must be a single number

