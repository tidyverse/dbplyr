# print method doesn't change unexpectedly

    Code
      sql_build(union(lf1, lf2) %>% union_all(lf3))
    Output
        <SQL SELECT>
        From:
          <dbplyr_table_ident[1]>
          [1] `lf1`
        Select:   `lf1`.*, NULL
      
        UNION
      
        <SQL SELECT>
        From:
          <dbplyr_table_ident[1]>
          [1] `lf2`
        Select:   `x`, NULL, `z`
      
        UNION ALL
      
        <SQL SELECT>
        From:
          <dbplyr_table_ident[1]>
          [1] `lf3`
        Select:   `x`, NULL, `z`

# generated sql doesn't change unexpectedly

    Code
      union(lf, lf)
    Output
      <SQL>
      SELECT *
      FROM `df`
      
      UNION
      
      SELECT *
      FROM `df`

---

    Code
      setdiff(lf, lf)
    Output
      <SQL>
      (
        SELECT *
        FROM `df`
      )
      EXCEPT
      (
        SELECT *
        FROM `df`
      )

---

    Code
      intersect(lf, lf)
    Output
      <SQL>
      (
        SELECT *
        FROM `df`
      )
      INTERSECT
      (
        SELECT *
        FROM `df`
      )

---

    Code
      union(lf, lf, all = TRUE)
    Output
      <SQL>
      SELECT *
      FROM `df`
      
      UNION ALL
      
      SELECT *
      FROM `df`

---

    Code
      setdiff(lf, lf, all = TRUE)
    Output
      <SQL>
      (
        SELECT *
        FROM `df`
      )
      EXCEPT ALL
      (
        SELECT *
        FROM `df`
      )

---

    Code
      intersect(lf, lf, all = TRUE)
    Output
      <SQL>
      (
        SELECT *
        FROM `df`
      )
      INTERSECT ALL
      (
        SELECT *
        FROM `df`
      )

