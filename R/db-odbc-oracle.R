#' @export
sql_select.Oracle<- function(con, select, from, where = NULL,
                             group_by = NULL, having = NULL,
                             order_by = NULL,
                             limit = NULL,
                             distinct = FALSE,
                             ...) {
  out <- vector("list", 7)
  names(out) <- c("select", "from", "where", "group_by", "having", "order_by",
                  "limit")

  assertthat::assert_that(is.character(select), length(select) > 0L)
  out$select <- build_sql(
    "SELECT ",
    if (distinct) sql("DISTINCT "),
    escape(toupper(select), collapse = ", ", con = con)
  )

  assertthat::assert_that(is.character(from), length(from) == 1L)
  out$from <- build_sql("FROM ", from, con = con)

  if (length(where) > 0L) {
    assertthat::assert_that(is.character(where))

    where_paren <- escape(where, parens = TRUE, con = con)
    out$where <- build_sql("WHERE ", sql_vector(where_paren, collapse = " AND "))
  }

  if (length(group_by) > 0L) {
    assertthat::assert_that(is.character(group_by))
    out$group_by <- build_sql(
      "GROUP BY ",
      escape(group_by, collapse = ", ", con = con)
    )
  }

  if (length(having) > 0L) {
    assertthat::assert_that(is.character(having))
    out$having <- build_sql(
      "HAVING ",
      escape(having, collapse = ", ", con = con)
    )
  }

  if (length(order_by) > 0L) {
    assertthat::assert_that(is.character(order_by))
    out$order_by <- build_sql(
      "ORDER BY ",
      escape(order_by, collapse = ", ", con = con)
    )
  }

  # Using Oracle's FETCH FIRST SQL command instead of LIMIT
  # https://oracle-base.com/articles/12c/row-limiting-clause-for-top-n-queries-12cr1

  if (!is.null(limit) && !identical(limit, Inf)) {
    assertthat::assert_that(is.numeric(limit), length(limit) == 1L, limit > 0)
    out$limit <- build_sql(
      "FETCH FIRST ", sql(format(trunc(limit), scientific = FALSE)), " ROWS ONLY ",
      con = con
    )
  }

  escape(unname(plyr::compact(out)), collapse = "\n", parens = FALSE, con = con)
}


#' @export
sql_translate_env.Oracle <- function(con) {
  sql_variant(
    scalar = sql_translator(.parent = base_odbc_scalar,
                            as.character  = sql_cast("VARCHAR(255)"),
                            as.numeric = sql_cast("NUMERIC"),
                            as.double = sql_cast("NUMERIC")
    ) ,
    base_odbc_agg,
    base_odbc_win
  )
}


#' @export
sql_subquery.Oracle <- function(con, from, name = dbplyr:::unique_name(), ...) {
  # Query fails if a quoted table is used.  Using sql() to remove quotes from an
  # already 'idented' argument.
  if (is.ident(from)) {
    if (is.null(name)) {
      build_sql("(", sql(from), ") ", con = con)
    } else {
      build_sql("(", sql(from), ") ", ident(name), con = con)
    }
  } else {
    if (is.null(name)) {
      build_sql("(", from, ")", con = con)
    } else {
      build_sql("(", from, ") ", ident(name), con = con)
    }
  }
}

