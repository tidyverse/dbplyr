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
      union_all(lf, lf)
    Output
      <SQL>
      SELECT *
      FROM "df"
      
      UNION ALL
      
      SELECT *
      FROM "df"

