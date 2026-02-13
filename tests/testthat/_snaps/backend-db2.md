# uses FETCH FIRST instead of LIMIT

    Code
      head(mf)
    Output
      <SQL>
      SELECT *
      FROM "df"
      FETCH FIRST 6 ROWS ONLY

---

    Code
      head(mf, 10)
    Output
      <SQL>
      SELECT *
      FROM "df"
      FETCH FIRST 10 ROWS ONLY

# queries with WHERE and ORDER BY work

    Code
      head(arrange(filter(mf, x > 0), y), 5)
    Output
      <SQL>
      SELECT *
      FROM "df"
      WHERE ("x" > 0.0)
      ORDER BY "y"
      FETCH FIRST 5 ROWS ONLY

