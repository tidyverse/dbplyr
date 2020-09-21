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

