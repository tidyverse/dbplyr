#' @export
#' @rdname sql_build
lazy_rf_join_query <- function(
  x,
  y,
  type,
  by,
  table_names,
  select,
  group_vars = op_grps(x),
  order_vars = op_sort(x),
  frame = op_frame(x),
  call = caller_env()
) {
  check_lazy_query(x, call = call)
  check_lazy_query(y, call = call)

  check_character(type, call = call)

  check_has_names(table_names, c("name", "from"), call = call)
  check_character(table_names$name, call = call)
  check_character(table_names$from, call = call)

  check_has_names(
    select,
    c("name", "expr", "group_vars", "order_vars", "frame"),
    call = call
  )
  check_character(select$name, call = call)

  lazy_query(
    query_type = "rf_join",
    x = x,
    y = y,
    type = type,
    by = by,
    table_names = table_names,
    select = select,
    group_vars = group_vars,
    order_vars = order_vars,
    frame = frame
  )
}

#' @export
op_vars.lazy_rf_join_query <- function(op) {
  op$select$name
}

#' @export
sql_build.lazy_rf_join_query <- function(op, con, ..., sql_options = NULL) {
  table_names_out <- generate_join_table_names(op$table_names, con)

  by <- op$by
  by$x_as <- table_names_out[[1]]
  by$y_as <- table_names_out[[2]]

  # Build tables mapping for expression translation
  # Use indexing to preserve table_path class (as.list strips it)
  tables <- set_names(
    list(table_names_out[1], table_names_out[2]),
    c(".table1", ".table2")
  )
  table_vars <- list(op_vars(op$x), op_vars(op$y))
  table_vars <- set_names(table_vars, table_names_out)

  select <- translate_join_select(
    con = con,
    select = op$select,
    tables = tables,
    table_vars = table_vars,
    use_star = sql_options$use_star,
    qualify_all_columns = sql_options$qualify_all_columns
  )

  rf_join_query(
    sql_build(op$x, con, sql_options = sql_options),
    sql_build(op$y, con, sql_options = sql_options),
    select = select,
    type = op$type,
    by = by,
    na_matches = by$na_matches
  )
}

# Built query -------------------------------------------------------------

#' @export
#' @rdname sql_build
rf_join_query <- function(
  x,
  y,
  select,
  ...,
  type = "inner",
  by = NULL,
  suffix = c(".x", ".y"),
  na_matches = FALSE
) {
  query(
    "rf_join",
    x = x,
    y = y,
    select = select,
    type = type,
    by = by,
    na_matches = na_matches
  )
}

#' @export
sql_render.rf_join_query <- function(
  query,
  con = NULL,
  ...,
  sql_options = NULL,
  subquery = FALSE,
  lvl = 0
) {
  from_x <- sql_render(query$x, con, ..., subquery = TRUE, lvl = lvl + 1)
  from_y <- sql_render(query$y, con, ..., subquery = TRUE, lvl = lvl + 1)

  dbplyr_query_join(
    con,
    from_x,
    from_y,
    vars = query$vars,
    type = query$type,
    by = query$by,
    na_matches = query$na_matches,
    select = query$select,
    lvl = lvl
  )
}

#' @export
flatten_query.rf_join_query <- function(qry, query_list, con) {
  flatten_query_2_tables(qry, query_list, con)
}

# SQL generation ----------------------------------------------------------

#' @rdname db-sql
#' @export
sql_query_join <- function(
  con,
  x,
  y,
  select,
  type = "inner",
  by = NULL,
  na_matches = FALSE,
  ...,
  lvl = 0
) {
  check_dots_used()
  UseMethod("sql_query_join")
}
#' @export
sql_query_join.DBIConnection <- function(
  con,
  x,
  y,
  select,
  type = "inner",
  by = NULL,
  na_matches = FALSE,
  ...,
  lvl = 0
) {
  JOIN <- switch(
    type,
    left = sql("LEFT JOIN"),
    inner = sql("INNER JOIN"),
    right = sql("RIGHT JOIN"),
    full = sql("FULL JOIN"),
    cross = sql("CROSS JOIN"),
    cli_abort("Unknown join type: {.val {type}}")
  )

  x <- dbplyr_sql_subquery(con, x, name = by$x_as, lvl = lvl)
  y <- dbplyr_sql_subquery(con, y, name = by$y_as, lvl = lvl)

  on <- sql_join_tbls(con, by, na_matches = na_matches)

  # Wrap with SELECT since callers assume a valid query is returned
  clauses <- list(
    sql_clause_select(select),
    sql_clause_from(x),
    sql_clause(JOIN, y),
    sql_clause("ON", on, sep = " AND", parens = TRUE, lvl = 1)
  )
  sql_format_clauses(clauses, lvl)
}
dbplyr_query_join <- function(
  con,
  x,
  y,
  vars,
  type = "inner",
  by = NULL,
  na_matches = FALSE,
  ...,
  select = NULL,
  lvl = 0
) {
  check_2ed(con)
  sql_query_join(
    con,
    x,
    y,
    select,
    type = type,
    by = by,
    na_matches = na_matches,
    ...,
    lvl = lvl
  )
}

# Helpers ----------------------------------------------------------------------

sql_rf_join_vars <- function(
  con,
  type,
  vars,
  x_as = "LHS",
  y_as = "RHS",
  use_star,
  qualify_all_columns
) {
  type <- arg_match0(type, c("right", "full"))

  check_table_path(x_as)
  check_table_path(y_as)
  table_names <- c(x_as, y_as)

  if (type == "full") {
    duplicated_vars <- intersect(tolower(vars$all_x), tolower(vars$all_y))
    out <- purrr::map2(
      vars$x,
      vars$y,
      ~ {
        if (!is.na(.x) && !is.na(.y)) {
          x_prefix <- sql_table_prefix(con, x_as, .x)
          y_prefix <- sql_table_prefix(con, y_as, .y)
          sql_glue2(con, "COALESCE({x_prefix}, {y_prefix})")
        } else if (!is.na(.x)) {
          sql_multi_join_var(con, .x, table_names[[1L]], duplicated_vars)
        } else {
          sql_multi_join_var(con, .y, table_names[[2L]], duplicated_vars)
        }
      }
    )

    out <- set_names(out, vars$name)
    return(names_to_as(con, unlist(out)))
  }

  multi_join_vars <- purrr::map2_dfr(
    vars$x,
    vars$y,
    ~ {
      if (!is.na(.x)) {
        table <- 1L
        var <- .x
      } else {
        table <- 2L
        var <- .y
      }

      vctrs::new_data_frame(list(table = table, var = var), n = 1L)
    }
  )

  multi_join_vars <- vctrs::vec_cbind(name = vars$name, multi_join_vars)
  table_vars <- set_names(vars[c("all_x", "all_y")], table_names)

  sql_multi_join_select(
    con = con,
    vars = multi_join_vars,
    table_vars = table_vars,
    use_star = use_star,
    qualify_all_columns = qualify_all_columns
  )
}
