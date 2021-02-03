# sql_substr works as expected

    Code
      substr("test")
    Error <simpleError>
      argument "start" is missing, with no default

---

    Code
      substr("test", 0)
    Error <simpleError>
      argument "stop" is missing, with no default

---

    Code
      substr("test", "x", 1)
    Error <rlang_error>
      `start` must be a single number

---

    Code
      substr("test", 1, "x")
    Error <rlang_error>
      `stop` must be a single number

