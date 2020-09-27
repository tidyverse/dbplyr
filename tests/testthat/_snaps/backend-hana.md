# custom string translations

    Code
      translate_sql(paste0("a", "b"))
    Output
      <SQL> 'a' || 'b'

---

    Code
      translate_sql(paste("a", "b"))
    Output
      <SQL> 'a' || ' ' || 'b'

---

    Code
      translate_sql(substr(x, 2, 4))
    Output
      <SQL> SUBSTRING(`x`, 2, 3)

---

    Code
      translate_sql(substring(x, 2, 4))
    Output
      <SQL> SUBSTRING(`x`, 2, 3)

---

    Code
      translate_sql(str_sub(x, 2, -2))
    Output
      <SQL> SUBSTRING(`x`, 2, LENGTH(`x`) - 2)

