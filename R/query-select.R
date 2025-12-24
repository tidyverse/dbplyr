#' @export
#' @rdname sql_build
lazy_select_query <- function(
  x,
  select = NULL,
  where = NULL,
  group_by = NULL,
  having = NULL,
  order_by = NULL,
  limit = NULL,
  distinct = FALSE,
  group_vars = NULL,
  order_vars = NULL,
  frame = NULL,
  select_operation = c("select", "mutate", "summarise")
) {
  check_lazy_query(x, call = call)
  stopifnot(is.null(select) || is_lazy_sql_part(select))
  stopifnot(is_lazy_sql_part(where))
  # stopifnot(is.character(group_by))
  stopifnot(is_lazy_sql_part(order_by))
  check_number_whole(limit, allow_infinite = TRUE, allow_null = TRUE)
  check_bool(distinct)

  select <- select %||% syms(set_names(op_vars(x)))
  select_operation <- arg_match0(
    select_operation,
    c("select", "mutate", "summarise")
  )

  # inherit `group_vars`, `order_vars`, and `frame` from `from`
  group_vars <- group_vars %||% op_grps(x)
  order_vars <- order_vars %||% op_sort(x)
  frame <- frame %||% op_frame(x)

  if (select_operation == "mutate") {
    select <- new_lazy_select(
      select,
      group_vars = group_vars,
      order_vars = order_vars,
      frame = frame
    )
  } else {
    select <- new_lazy_select(select)
  }

  lazy_query(
    query_type = "select",
    x = x,
    select = select,
    where = where,
    group_by = group_by,
    order_by = order_by,
    distinct = distinct,
    limit = limit,
    select_operation = select_operation,
    group_vars = group_vars,
    order_vars = order_vars,
    frame = frame
  )
}

is_lazy_select_query <- function(x) {
  inherits(x, "lazy_select_query")
}

is_lazy_sql_part <- function(x) {
  if (is.null(x)) {
    return(TRUE)
  }
  if (is_quosures(x)) {
    return(TRUE)
  }

  if (!is.list(x)) {
    return(FALSE)
  }
  purrr::every(x, \(item) is_quosure(item) || is_symbol(item) || is_call(item))
}

new_lazy_select <- function(
  vars,
  group_vars = character(),
  order_vars = NULL,
  frame = NULL
) {
  vctrs::vec_as_names(names2(vars), repair = "check_unique")

  var_names <- names(vars)
  vars <- unname(vars)

  tibble(
    name = var_names %||% character(),
    expr = vars %||% list(),
    group_vars = rep_along(vars, list(group_vars)),
    order_vars = rep_along(vars, list(order_vars)),
    frame = rep_along(vars, list(frame))
  )
}

#' @export
op_vars.lazy_select_query <- function(op) {
  op$select$name
}

#' @export
sql_build.lazy_select_query <- function(op, con, ..., sql_options = NULL) {
  alias <- remote_name(op$x, null_if_local = FALSE) %||% unique_subquery_name()
  from <- sql_build(op$x, con, sql_options = sql_options)
  select_sql_list <- get_select_sql(
    select = op$select,
    select_operation = op$select_operation,
    in_vars = op_vars(op$x),
    table_alias = alias,
    con = con,
    use_star = sql_options$use_star
  )
  where_sql <- translate_sql_(
    op$where,
    con = con,
    context = list(clause = "WHERE")
  )

  select_query(
    from = from,
    select = select_sql_list$select_sql,
    where = where_sql,
    group_by = translate_sql_(op$group_by, con = con),
    having = translate_sql_(op$having, con = con, window = FALSE),
    window = select_sql_list$window_sql,
    order_by = translate_sql_(op$order_by, con = con),
    distinct = op$distinct,
    limit = op$limit,
    from_alias = alias
  )
}

get_select_sql <- function(
  select,
  select_operation,
  in_vars,
  table_alias,
  con,
  use_star
) {
  if (select_operation == "summarise") {
    select_expr <- set_names(select$expr, select$name)
    select_sql <- translate_sql_(
      select_expr,
      con,
      window = FALSE,
      context = list(clause = "SELECT")
    )
    return(list(select_sql = select_sql, window_sql = character()))
  }

  if (use_star) {
    if (is_identity(select$expr, select$name, in_vars)) {
      out <- list(
        select_sql = sql_star(con, table_alias),
        window_sql = character()
      )
      return(out)
    } else {
      select <- select_use_star(select, in_vars, table_alias, con)
    }
  }

  # translate once just to register windows
  win_register_activate()
  # Remove known windows before building the next query
  on.exit(win_reset(), add = TRUE)
  on.exit(win_register_deactivate(), add = TRUE)
  select_sql <- translate_select_sql(con, select)
  win_register_deactivate()

  named_windows <- win_register_names()
  if (nrow(named_windows) > 0 && supports_window_clause(con)) {
    # need to translate again and use registered windows names
    select_sql <- translate_select_sql(con, select)

    # build window sql
    names_esc <- sql_escape_ident(con, named_windows$name)
    window_sql <- sql(paste0(names_esc, " AS ", named_windows$key))
  } else {
    window_sql <- character()
  }

  list(
    select_sql = select_sql,
    window_sql = window_sql
  )
}

select_use_star <- function(select, vars_prev, table_alias, con) {
  first_match <- vctrs::vec_match(vars_prev[[1]], select$name)
  if (is.na(first_match)) {
    return(select)
  }

  last <- first_match + length(vars_prev) - 1
  n <- vctrs::vec_size(select)

  if (n < last) {
    return(select)
  }

  test_cols <- vctrs::vec_slice(select, seq2(first_match, last))

  if (is_identity(test_cols$expr, test_cols$name, vars_prev)) {
    idx_start <- seq2(1, first_match - 1)
    idx_end <- seq2(last + 1, n)
    vctrs::vec_rbind(
      vctrs::vec_slice(select, idx_start),
      tibble(name = "", expr = list(sql_star(con, table_alias))),
      vctrs::vec_slice(select, idx_end)
    )
  } else {
    select
  }
}

translate_select_sql <- function(con, select_df) {
  n <- vctrs::vec_size(select_df)
  out <- vctrs::vec_init(sql(), n)
  out <- set_names(out, select_df$name)
  for (i in seq2(1, n)) {
    out[[i]] <- translate_sql_(
      dots = select_df$expr[i],
      con = con,
      vars_group = translate_sql_(syms(select_df$group_vars[[i]]), con),
      vars_order = translate_sql_(
        select_df$order_vars[[i]],
        con,
        context = list(clause = "ORDER")
      ),
      vars_frame = select_df$frame[[i]][[1]],
      context = list(clause = "SELECT")
    )
  }

  out
}

# Built query -------------------------------------------------------------

#' @export
#' @rdname sql_build
select_query <- function(
  from,
  select = sql("*"),
  where = character(),
  group_by = character(),
  having = character(),
  window = character(),
  order_by = character(),
  limit = NULL,
  distinct = FALSE,
  from_alias = NULL
) {
  check_character(select)
  check_character(where)
  check_character(group_by)
  check_character(having)
  check_character(window)
  check_character(order_by)
  check_number_whole(limit, allow_infinite = TRUE, allow_null = TRUE)
  check_bool(distinct)
  check_string(from_alias, allow_null = TRUE)

  query(
    "select",
    from = from,
    select = select,
    where = where,
    group_by = group_by,
    having = having,
    window = window,
    order_by = order_by,
    distinct = distinct,
    limit = limit,
    from_alias = from_alias
  )
}

#' @export
sql_render.select_query <- function(
  query,
  con,
  ...,
  sql_options = NULL,
  subquery = FALSE,
  lvl = 0
) {
  from <- dbplyr_sql_subquery(
    con,
    sql_render(query$from, con, ..., subquery = TRUE, lvl = lvl + 1),
    name = query$from_alias,
    lvl = lvl
  )

  sql_query_select(
    con,
    query$select,
    from,
    where = query$where,
    group_by = query$group_by,
    having = query$having,
    window = query$window,
    order_by = query$order_by,
    limit = query$limit,
    distinct = query$distinct,
    ...,
    subquery = subquery,
    lvl = lvl
  )
}

warn_drop_order_by <- function() {
  # Rules according to SQLite: https://sqlite.org/forum/forumpost/878ca7a9be0862af?t=h
  # 1. There is no LIMIT clause in the subquery
  # 3. The subquery is not part of the FROM clause in an UPDATE-FROM statement
  # 4. The outer query does not use any aggregate functions other than the built-in count(), min(), and/or max() functions.
  # 5. Either the outer query has its own ORDER BY clause or else the subquery is one term of a join.
  #
  # (2. and 6. are left out as they are SQLite internal)
  warn(c(
    "ORDER BY is ignored in subqueries without LIMIT",
    i = "Do you need to move arrange() later in the pipeline or use window_order() instead?"
  ))
}

#' @export
flatten_query.select_query <- function(qry, query_list, con) {
  from <- qry$from
  query_list <- flatten_query(from, query_list, con)

  qry$from <- get_subquery_name(from, query_list)
  querylist_reuse_query(qry, query_list, con)
}

# SQL generation ----------------------------------------------------------

#' @rdname db-sql
#' @export
sql_query_select <- function(
  con,
  select,
  from,
  where = NULL,
  group_by = NULL,
  having = NULL,
  window = NULL,
  order_by = NULL,
  limit = NULL,
  distinct = FALSE,
  ...,
  subquery = FALSE,
  lvl = 0
) {
  check_dots_used()
  UseMethod("sql_query_select")
}

#' @export
sql_query_select.DBIConnection <- function(
  con,
  select,
  from,
  where = NULL,
  group_by = NULL,
  having = NULL,
  window = NULL,
  order_by = NULL,
  limit = NULL,
  distinct = FALSE,
  ...,
  subquery = FALSE,
  lvl = 0
) {
  select <- sql(names_to_as(con, select))

  sql_select_clauses(
    select = sql_clause_select(select, distinct),
    from = sql_clause_from(sql_escape_table_source(con, from)),
    where = sql_clause_where(where),
    group_by = sql_clause_group_by(group_by),
    having = sql_clause_having(having),
    window = sql_clause_window(window),
    order_by = sql_clause_order_by(order_by, subquery, limit),
    limit = sql_clause_limit(limit),
    lvl = lvl
  )
}
