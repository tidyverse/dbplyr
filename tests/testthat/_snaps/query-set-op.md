# print method doesn't change unexpectedly

    Code
      sql_build(union_all(union(lf1, lf2), lf3))
    Output
        SELECT "lf1".*, NULL AS "z"
        FROM "lf1"
      
        UNION
      
        SELECT "x", NULL AS "y", "z"
        FROM "lf2"
      
        UNION ALL
      
        SELECT "x", NULL AS "y", "z"
        FROM "lf3"

# generated sql doesn't change unexpectedly

    Code
      union(lf, lf)
    Output
      <SQL>
      SELECT *
      FROM "df"
      
      UNION
      
      SELECT *
      FROM "df"

---

    Code
      setdiff(lf, lf)
    Output
      <SQL>
      (
        SELECT *
        FROM "df"
      )
      EXCEPT
      (
        SELECT *
        FROM "df"
      )

---

    Code
      intersect(lf, lf)
    Output
      <SQL>
      (
        SELECT *
        FROM "df"
      )
      INTERSECT
      (
        SELECT *
        FROM "df"
      )

---

    Code
      union(lf, lf, all = TRUE)
    Output
      <SQL>
      SELECT *
      FROM "df"
      
      UNION ALL
      
      SELECT *
      FROM "df"

---

    Code
      setdiff(lf, lf, all = TRUE)
    Output
      <SQL>
      (
        SELECT *
        FROM "df"
      )
      EXCEPT ALL
      (
        SELECT *
        FROM "df"
      )

---

    Code
      intersect(lf, lf, all = TRUE)
    Output
      <SQL>
      (
        SELECT *
        FROM "df"
      )
      INTERSECT ALL
      (
        SELECT *
        FROM "df"
      )

