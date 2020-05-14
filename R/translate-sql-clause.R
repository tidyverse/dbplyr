
sql_clause_generic <- function(clause, fields, con){
  if (length(fields) > 0L) {
    assert_that(is.character(fields))
    build_sql(
      sql(clause), " ",
      escape(fields, collapse = ", ", con = con),
      con = con
    )
  }
}

sql_clause_select <- function(select, con, distinct = FALSE){
  assert_that(is.character(select))
  if (is_empty(select)) {
    abort("Query contains no columns")
  }

  build_sql(
    "SELECT ",
    if (distinct) sql("DISTINCT "),
    escape(select, collapse = ", ", con = con),
    con = con
  )
}

sql_clause_where <- function(where, con){
  if (length(where) > 0L) {
    assert_that(is.character(where))
    where_paren <- escape(where, parens = TRUE, con = con)
    build_sql(
      "WHERE ", sql_vector(where_paren, collapse = " AND ", con = con),
      con = con
    )
  }
}

sql_clause_limit <- function(limit, con){
  if (!is.null(limit) && !identical(limit, Inf)) {
    build_sql(
      "LIMIT ", sql(format(limit, scientific = FALSE)),
      con = con
    )
  }
}

sql_clause_from  <- function(from, con) sql_clause_generic("FROM", from, con)

sql_clause_group_by <- function(group_by, con) sql_clause_generic("GROUP BY", group_by, con)

sql_clause_having <- function(having, con) sql_clause_generic("HAVING", having, con)

sql_clause_order_by <- function(order_by, con) sql_clause_generic("ORDER BY", order_by, con)


