# sql_clause_select generates expected SQL

    Code
      sql_clause_select(sql("x", "y"))
    Output
      <sql clause> SELECT x, y

---

    Code
      sql_clause_select(sql("x"), distinct = TRUE)
    Output
      <sql clause> SELECT DISTINCT x

---

    Code
      sql_clause_select(sql("x"), top = 10)
    Output
      <sql clause> SELECT TOP 10 x

---

    Code
      sql_clause_select(sql("x"), distinct = TRUE, top = 5)
    Output
      <sql clause> SELECT DISTINCT TOP 5 x

# clauses wrap long inputs nicely

    Code
      sql_clause_select(sql(vars))
    Output
      <sql clause> SELECT
        variable1,
        variable2,
        variable3,
        variable4,
        variable5,
        variable6,
        variable7,
        variable8

# sql_clause_select requires non-empty input

    Code
      sql_clause_select(sql())
    Condition
      Error in `sql_clause_select()`:
      ! Query contains no columns

# sql_clause_where wraps in parens and combines conditions with AND

    Code
      sql_clause_where(sql("x > 1"))
    Output
      <sql clause> WHERE (x > 1)

---

    Code
      sql_clause_where(sql("x > 1", "y < 2"))
    Output
      <sql clause> WHERE (x > 1) AND (y < 2)

# sql_clause_order_by warns when dropping in subquery

    Code
      sql_clause_order_by(sql("x"), subquery = TRUE)
    Condition
      Warning:
      ORDER BY is ignored in subqueries without LIMIT
      i Do you need to move arrange() later in the pipeline or use window_order() instead?
    Output
      NULL

# sql_select_clauses generates expected SQL

    Code
      sql_select_clauses(select = sql_clause_select(sql("x", "y")), from = sql_clause_from(
        sql("foo")), where = sql_clause_where(sql("x > 1")), group_by = sql_clause_group_by(
        sql("x")), having = sql_clause_having(sql("COUNT(*) > 1")), window = sql_clause_window(
        sql("win1 AS (PARTITION BY x)")), order_by = sql_clause_order_by(sql("x")),
      limit = sql_clause_limit(NULL, 10))
    Output
      <SQL> SELECT x, y
      FROM foo
      WHERE (x > 1)
      GROUP BY x
      HAVING (COUNT(*) > 1)
      WINDOW win1 AS (PARTITION BY x)
      ORDER BY x
      LIMIT 10

# update clauses generate expected SQL

    Code
      sql_clause_update(sql("foo"))
    Output
      <sql clause> UPDATE foo

---

    Code
      sql_clause_set(sql("x", "y"), sql("1", "2"))
    Output
      <sql clause> SET x = 1, y = 2

# insert clauses generate expected SQL

    Code
      sql_clause_insert(sql("x", "y"))
    Output
      <sql clause> INSERT (x, y)

---

    Code
      sql_clause_insert(sql("x", "y"), into = sql("foo"))
    Output
      <sql clause> INSERT INTO foo (x, y)

# sql_clause_where_exists generates expected SQL

    Code
      sql_format_clauses(clause_true)
    Output
      <SQL> WHERE EXISTS (
        SELECT 1 FROM bar
        WHERE (x = y)
      )
    Code
      sql_format_clauses(clause_false)
    Output
      <SQL> WHERE NOT EXISTS (
        SELECT 1 FROM bar
        WHERE (x = y)
      )

