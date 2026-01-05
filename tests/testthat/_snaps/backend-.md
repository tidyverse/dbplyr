# basic arithmetic is correct

    Code
      translate_sql(100L %/% 3L, con = con)
    Condition
      Error in `100L %/% 3L`:
      ! `%/%()` is not available in this SQL variant.

# can translate subsetting

    Code
      translate_sql(a[[x]], con = con)
    Condition
      Error in `a[[x]]`:
      ! Can only index with strings and numbers

---

    Code
      translate_sql(a[[TRUE]], con = con)
    Condition
      Error in `a[[TRUE]]`:
      ! Can only index with strings and numbers

# $ doesn't evaluate second argument

    Code
      filter(lazy_frame(x = 1, y = 1), x == y$id)
    Output
      <SQL>
      SELECT "df".*
      FROM "df"
      WHERE ("x" = "y"."id")

---

    Code
      filter(lazy_frame(x = 1), x == y$id)
    Output
      <SQL>
      SELECT "df".*
      FROM "df"
      WHERE ("x" = 1.0)

# useful error if $ used with inlined value

    Code
      filter(lazy_frame(x = 1), x == y$id)
    Condition
      Error in `x$id`:
      ! $ operator is invalid for atomic vectors

# can only translate case sensitive str_like

    Code
      translate_sql(str_like(x, "abc", ignore_case = FALSE), con = con)
    Condition
      Warning:
      The `ignore_case` argument of `str_like()` is deprecated as of dbplyr 2.6.0.
      i `str_like()` is always case sensitive.
      i Use `str_ilike()` for case insensitive string matching.
    Output
      <SQL> "x" LIKE 'abc'

---

    Code
      translate_sql(str_like(x, "abc", ignore_case = TRUE), con = con)
    Condition
      Warning:
      The `ignore_case` argument of `str_like()` is deprecated as of dbplyr 2.6.0.
      i `str_like()` is always case sensitive.
      i Use `str_ilike()` for case insensitive string matching.
      Error in `str_like()`:
      ! Backend does not support case insensitive `str_like()`.
      i Use `tolower()` on both arguments to achieve a case insensitive match.

---

    Code
      translate_sql(str_ilike(x, "abc"), con = con)
    Condition
      Error in `str_ilike()`:
      ! `str_ilike()` is not available in this SQL variant.

# default raw escapes translated correctly

    Code
      filter(mf, x == a)
    Output
      <SQL>
      SELECT `df`.*
      FROM `df`
      WHERE (`x` = X'616263')

---

    Code
      filter(mf, x %in% L)
    Output
      <SQL>
      SELECT `df`.*
      FROM `df`
      WHERE (`x` IN (X'616263', X'0102'))

---

    Code
      qry
    Output
      <SQL>
      SELECT `df`.*
      FROM `df`
      WHERE (`x` IN (X'616263', X'0102'))

