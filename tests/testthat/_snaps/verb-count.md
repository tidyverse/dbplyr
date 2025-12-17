# generates expected SQL for common situations

    Code
      count(db, g)
    Output
      <SQL>
      SELECT "g", COUNT(*) AS "n"
      FROM "df"
      GROUP BY "g"

---

    Code
      count(db, g, wt = x)
    Output
      <SQL>
      SELECT "g", SUM("x") AS "n"
      FROM "df"
      GROUP BY "g"

---

    Code
      count(db, g, sort = TRUE)
    Output
      <SQL>
      SELECT "g", COUNT(*) AS "n"
      FROM "df"
      GROUP BY "g"
      ORDER BY "n" DESC

---

    Code
      add_count(db, g, sort = TRUE)
    Output
      <SQL>
      SELECT "df".*, COUNT(*) OVER (PARTITION BY "g") AS "n"
      FROM "df"
      ORDER BY "n" DESC

---

    Code
      add_count(group_by(db, g))
    Output
      <SQL>
      SELECT "df".*, COUNT(*) OVER (PARTITION BY "g") AS "n"
      FROM "df"

# .drop is not supported

    Code
      add_count(lazy_frame(g = 1), .drop = TRUE)
    Condition
      Error in `add_count()`:
      ! Argument `.drop` isn't supported on database backends.

