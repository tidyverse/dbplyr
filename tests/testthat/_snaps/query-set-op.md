# print method doesn't change unexpectedly

    Code
      sql_build(union(lf1, lf2))
    Output
      <SQL UNION>
      X:
        <SQL SELECT>
        From:
          <IDENT> df
        Select:   `x`, `y`, NULL
      Y:
        <SQL SELECT>
        From:
          <IDENT> df
        Select:   `x`, NULL, `z`

# generated sql doesn't change unexpectedly

    Code
      union(lf, lf)
    Output
      <SQL>
      (SELECT *
      FROM `df`)
      UNION
      (SELECT *
      FROM `df`)

---

    Code
      setdiff(lf, lf)
    Output
      <SQL>
      (SELECT *
      FROM `df`)
      EXCEPT
      (SELECT *
      FROM `df`)

---

    Code
      intersect(lf, lf)
    Output
      <SQL>
      (SELECT *
      FROM `df`)
      INTERSECT
      (SELECT *
      FROM `df`)

---

    Code
      union(lf, lf, all = TRUE)
    Output
      <SQL>
      (SELECT *
      FROM `df`)
      UNION ALL
      (SELECT *
      FROM `df`)

---

    Code
      setdiff(lf, lf, all = TRUE)
    Output
      <SQL>
      (SELECT *
      FROM `df`)
      EXCEPT ALL
      (SELECT *
      FROM `df`)

---

    Code
      intersect(lf, lf, all = TRUE)
    Output
      <SQL>
      (SELECT *
      FROM `df`)
      INTERSECT ALL
      (SELECT *
      FROM `df`)

# CTEs work with set ops

    Code
      union(lf1, lf2) %>% remote_query(cte = TRUE)
    Output
      <SQL> WITH `q01` AS (
      SELECT `x`, `y`, NULL AS `z`
      FROM `df1`
      ),
      `q02` AS (
      SELECT `x`, NULL AS `y`, `z`
      FROM `df2`
      )
      (SELECT *
      FROM `q01`)
      UNION
      (SELECT *
      FROM `q02`)

