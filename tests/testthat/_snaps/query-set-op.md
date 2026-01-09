# generated sql doesn't change unexpectedly

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

