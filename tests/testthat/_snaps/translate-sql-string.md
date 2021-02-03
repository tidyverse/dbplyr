# sql_substr works as expected

    Code
      translate_sql(substr("test"))
    Error <simpleError>
      argument "start" is missing, with no default

---

    Code
      translate_sql(substr("test", 0))
    Error <simpleError>
      argument "stop" is missing, with no default

---

    Code
      translate_sql(substr("test", "x", 1))
    Error <rlang_error>
      `start` must be a single number

---

    Code
      translate_sql(substr("test", 1, "x"))
    Error <rlang_error>
      `stop` must be a single number

