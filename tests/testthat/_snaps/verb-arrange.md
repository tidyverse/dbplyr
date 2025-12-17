# quoting for rendering ordered grouped table

    Code
      sql_render(out)
    Output
      <SQL> SELECT `test-verb-arrange`.*
      FROM `test-verb-arrange`
      ORDER BY `y`

# arrange renders correctly (#373)

    Code
      # # arrange renders correctly
      lf <- lazy_frame(a = 1:3, b = 3:1)
      # basic
      arrange(lf, a)
    Output
      <SQL>
      SELECT "df".*
      FROM "df"
      ORDER BY "a"
    Code
      # double arrange
      arrange(arrange(lf, a), b)
    Output
      <SQL>
      SELECT "df".*
      FROM "df"
      ORDER BY "b"
    Code
      # remove ordered by
      select(arrange(lf, a), -a)
    Output
      <SQL>
      SELECT "b"
      FROM "df"
      ORDER BY "a"
    Code
      arrange(select(arrange(lf, a), -a), b)
    Output
      <SQL>
      SELECT "b"
      FROM "df"
      ORDER BY "b"
    Code
      # un-arrange
      arrange(arrange(lf, a))
    Output
      <SQL>
      SELECT "df".*
      FROM "df"
    Code
      arrange(select(arrange(lf, a), -a))
    Output
      <SQL>
      SELECT "b"
      FROM "df"
    Code
      # use order
      mutate(select(arrange(lf, a), -a), c = lag(b))
    Output
      <SQL>
      SELECT "b", LAG("b", 1, NULL) OVER () AS "c"
      FROM "df"
      ORDER BY "a"

# arrange renders correctly for single-table verbs (#373)

    Code
      lf <- lazy_frame(a = 1:3, b = 3:1)
      # head
      arrange(head(lf, 1), a)
    Output
      <SQL>
      SELECT "q01".*
      FROM (
        SELECT "df".*
        FROM "df"
        LIMIT 1
      ) AS "q01"
      ORDER BY "a"
    Code
      head(arrange(lf, a), 1)
    Output
      <SQL>
      SELECT "df".*
      FROM "df"
      ORDER BY "a"
      LIMIT 1
    Code
      arrange(head(arrange(lf, a), 1), b)
    Output
      <SQL>
      SELECT "q01".*
      FROM (
        SELECT "df".*
        FROM "df"
        ORDER BY "a"
        LIMIT 1
      ) AS "q01"
      ORDER BY "b"
    Code
      # mutate
      arrange(mutate(lf, a = b), a)
    Output
      <SQL>
      SELECT "b" AS "a", "b"
      FROM "df"
      ORDER BY "a"
    Code
      # complex mutate
      arrange(mutate(arrange(lf, a), a = b), a)
    Output
      <SQL>
      SELECT "b" AS "a", "b"
      FROM "df"
      ORDER BY "a"
    Code
      arrange(mutate(arrange(lf, a), a = 1), b)
    Output
      <SQL>
      SELECT 1.0 AS "a", "b"
      FROM "df"
      ORDER BY "b"
    Code
      mutate(arrange(mutate(lf, a = -a), a), a = -a)
    Condition
      Warning:
      ORDER BY is ignored in subqueries without LIMIT
      i Do you need to move arrange() later in the pipeline or use window_order() instead?
    Output
      <SQL>
      SELECT -"a" AS "a", "b"
      FROM (
        SELECT -"a" AS "a", "b"
        FROM "df"
      ) AS "q01"

# can combine arrange with dual table verbs

    Code
      lf <- lazy_frame(a = 1:3, b = 3:1)
      rf <- lazy_frame(a = 1:3, c = 4:6)
      # warn if arrange before join
      left_join(arrange(lf, a), rf)
    Message
      Joining with `by = join_by(a)`
    Condition
      Warning:
      ORDER BY is ignored in subqueries without LIMIT
      i Do you need to move arrange() later in the pipeline or use window_order() instead?
    Output
      <SQL>
      SELECT "LHS".*, "c"
      FROM (
        SELECT "df".*
        FROM "df"
      ) AS "LHS"
      LEFT JOIN "df"
        ON ("LHS"."a" = "df"."a")
    Code
      semi_join(arrange(lf, a), rf)
    Message
      Joining with `by = join_by(a)`
    Condition
      Warning:
      ORDER BY is ignored in subqueries without LIMIT
      i Do you need to move arrange() later in the pipeline or use window_order() instead?
    Output
      <SQL>
      SELECT "LHS".*
      FROM (
        SELECT "df".*
        FROM "df"
      ) AS "LHS"
      WHERE EXISTS (
        SELECT 1 FROM "df"
        WHERE ("LHS"."a" = "df"."a")
      )
    Code
      union(arrange(lf, a), rf)
    Output
      <SQL>
      SELECT "df".*, NULL AS "c"
      FROM "df"
      ORDER BY "a"
      
      UNION
      
      SELECT "a", NULL AS "b", "c"
      FROM "df"
    Code
      # can arrange after join
      arrange(left_join(lf, rf), a)
    Message
      Joining with `by = join_by(a)`
    Output
      <SQL>
      SELECT "df_LHS".*, "c"
      FROM "df" AS "df_LHS"
      LEFT JOIN "df" AS "df_RHS"
        ON ("df_LHS"."a" = "df_RHS"."a")
      ORDER BY "df_LHS"."a"
    Code
      arrange(semi_join(lf, rf), a)
    Message
      Joining with `by = join_by(a)`
    Output
      <SQL>
      SELECT "q01".*
      FROM (
        SELECT "df_LHS".*
        FROM "df" AS "df_LHS"
        WHERE EXISTS (
          SELECT 1 FROM "df" AS "df_RHS"
          WHERE ("df_LHS"."a" = "df_RHS"."a")
        )
      ) AS "q01"
      ORDER BY "a"
    Code
      arrange(union(lf, rf), a)
    Output
      <SQL>
      SELECT "q01".*
      FROM (
        SELECT "df".*, NULL AS "c"
        FROM "df"
      
        UNION
      
        SELECT "a", NULL AS "b", "c"
        FROM "df"
      ) AS "q01"
      ORDER BY "a"

