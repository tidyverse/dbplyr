#' @export
`db_desc.Microsoft SQL Server` <- function(x) {
  info <- DBI::dbGetInfo(x)
  host <- if (info$host == "") "localhost" else info$host

  paste0("Microsoft SQL Server ", info$serverVersion, " [", info$user, "@",
         host, ":", info$port, "/", info$dbname, "]")
}

#' @export
`sql_select.Microsoft SQL Server`<- function(con, select, from, where = NULL,
                                             group_by = NULL, having = NULL,
                                             order_by = NULL,
                                             limit = NULL,
                                             distinct = FALSE,
                                             ...) {
  out <- vector("list", 7)
  names(out) <- c("select", "from", "where", "group_by",
                  "having", "order_by","limit")

  assert_that(is.character(select), length(select) > 0L)
  out$select <- build_sql(
    "SELECT ",

    if (!is.null(limit) && !identical(limit, Inf)) {
      assert_that(is.numeric(limit), length(limit) == 1L, limit > 0)
      sql("TOP ")
      sql(format(trunc(limit), scientific = FALSE))
      sql(" ")},

    if (distinct) sql("DISTINCT "),
    escape(select, collapse = ", ", con = con)
  )

  assert_that(is.character(from), length(from) == 1L)
  out$from <- build_sql("FROM ", from, con = con)

  if (length(where) > 0L) {
    assert_that(is.character(where))
    where_paren <- escape(where, parens = TRUE, con = con)
    out$where <- build_sql("WHERE ", sql_vector(where_paren, collapse = " AND "))
  }

  if (length(group_by) > 0L) {
    assert_that(is.character(group_by))
    out$group_by <- build_sql(
      "GROUP BY ",
      escape(group_by, collapse = ", ", con = con)
    )
  }

  if (length(having) > 0L) {
    assert_that(is.character(having))
    out$having <- build_sql(
      "HAVING ",
      escape(having, collapse = ", ", con = con)
    )
  }

  if (length(order_by) > 0L) {
    assert_that(is.character(order_by))
    out$order_by <- build_sql(
      "ORDER BY ",
      escape(order_by, collapse = ", ", con = con)
    )
  }

  escape(unname(compact(out)), collapse = "\n", parens = FALSE, con = con)
}


#' @export
`sql_subquery.Microsoft SQL Server` <- function(con, from, name = dbplyr:::unique_name(), ...) {
  if (is.ident(from)) {
    setNames(from, name)
  } else {
    build_sql("(", from, ") ", ident(name %||% dbplyr:::random_table_name()), con = con)
  }
}


#' @export
`sql_translate_env.Microsoft SQL Server` <- function(con) {
  dbplyr::sql_variant(
    scalar = dbplyr::sql_translator(
      .parent = dbplyr::base_scalar,
      as.numeric = function(x) build_sql("CAST(", x, " AS NUMERIC)"),
      as.double  = function(x) build_sql("CAST(", x, " AS NUMERIC)"),
      as.integer  = function(x) build_sql("CAST(", x, " AS INT)"),
      as.logical = function(x) build_sql("CAST(", x, " AS BOOLEAN)"),
      as.character  = function(x) build_sql("CAST(", x, " AS VARCHAR(MAX))"),
      as.date  = function(x) build_sql("CAST(", x, " AS DATE)"),
      as.Date  = function(x) build_sql("CAST(", x, " AS DATE)"),
      paste = function(..., sep = " ") build_sql("CONCAT_WS", list(sep, ...)),
      paste0 = function(...) build_sql("CONCAT", list(...)),
      xor = function(x, y) build_sql(x, " ^ ", y),
      or = function(x, y) build_sql(x, " or ", y),
      and = function(x, y) build_sql(x, " and ", y),
      pmin = function(...) build_sql_if_compare(..., con = con, compare = "<="),
      pmax = function(...) build_sql_if_compare(..., con = con, compare = ">=")
    ) ,
    aggregate = dbplyr::sql_translator(
      # T-SQL does not have function for: cor and cov
      .parent = dbplyr::base_agg,
      n = function() dbplyr::sql("COUNT(*)"),
      count = function() dbplyr::sql("COUNT(*)"),
      n_distinct = function(...) dbplyr::build_sql("COUNT(DISTINCT", list(...), ")"),
      sd =  function(...) dbplyr::build_sql("STDEV(", list(...), ")"),
      var = function(...) dbplyr::build_sql("VAR(", list(...), ")")
    )
  )}



#' @export
`sql_join.Microsoft SQL Server` <- function(con, x, y, vars, type = "inner", by = NULL, ...) {
  JOIN <- switch(
    type,
    left = sql("LEFT JOIN"),
    inner = sql("INNER JOIN"),
    right = sql("RIGHT JOIN"),
    full = sql("FULL JOIN"),
    stop("Unknown join type:", type, call. = FALSE)
  )

  select <- sql_vector(c(
    dbplyr:::sql_as(con, names(vars$x), vars$x, table = "TBL_LEFT"),
    dbplyr:::sql_as(con, names(vars$y), vars$y, table = "TBL_RIGHT")
  ), collapse = ", ", parens = FALSE)

  on <- sql_vector(
    paste0(
      dbplyr:::sql_table_prefix(con, by$x, "TBL_LEFT"),
      " = ",
      dbplyr:::sql_table_prefix(con, by$y, "TBL_RIGHT")
    ),
    collapse = " AND ",
    parens = TRUE
  )

  # Wrap with SELECT since callers assume a valid query is returned
  build_sql(
    "SELECT ", select, "\n",
    "  FROM ", x, "\n",
    "  ", JOIN, " ", y, "\n",
    "  ON ", on, "\n",
    con = con
  )
}



