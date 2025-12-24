#' @export
#' @rdname sql_build
lazy_semi_join_query <- function(
  x,
  y,
  vars,
  anti,
  by,
  where,
  group_vars = op_grps(x),
  order_vars = op_sort(x),
  frame = op_frame(x),
  call = caller_env()
) {
  check_lazy_query(x, call = call)
  check_lazy_query(y, call = call)
  check_bool(anti, call = call)
  check_semi_join_by(by, call)

  lazy_query(
    query_type = "semi_join",
    x = x,
    y = y,
    anti = anti,
    by = by,
    vars = vars,
    where = where,
    group_vars = group_vars,
    order_vars = order_vars,
    frame = frame
  )
}

check_semi_join_by <- function(by, call) {
  if (!vctrs::vec_is_list(by) && !inherits(by, "dplyr_join_by")) {
    cli_abort("{.arg by} must be a list", .internal = TRUE)
  }
  check_character(by$x, call = call)
  check_character(by$y, call = call)
  if (vctrs::vec_size(by$x) != vctrs::vec_size(by$y)) {
    cli_abort(
      "{.arg by$x} and {.arg by$y} must have the same size",
      .internal = TRUE
    )
  }
  check_string(by$x_as, arg = "by$x_as", call = call)
  check_string(by$y_as, arg = "by$y_as", call = call)
}


#' @export
op_vars.lazy_semi_join_query <- function(op) {
  op$vars$name
}

#' @export
sql_build.lazy_semi_join_query <- function(op, con, ..., sql_options = NULL) {
  vars_prev <- op_vars(op$x)
  if (
    sql_options$use_star &&
      identical(op$vars$var, op$vars$name) &&
      identical(op$vars$var, vars_prev)
  ) {
    vars <- sql_star(con, op$by$x_as)
  } else {
    vars <- ident(set_names(op$vars$var, op$vars$name))
  }

  # We've introduced aliases to disambiguate the internal and external tables
  # so need to update the existing WHERE clause
  y_vars <- op_vars(op$y)
  y_as <- op$by$y_as
  replacements <- lapply(y_vars, \(var) sql_glue2(con, "{y_as}.{.id var}"))
  where <- replace_sym(op$where, y_vars, replacements)
  where_sql <- translate_sql_(
    where,
    con = con,
    context = list(clause = "WHERE")
  )

  semi_join_query(
    sql_build(op$x, con, sql_options = sql_options),
    sql_build(op$y, con, sql_options = sql_options),
    vars = vars,
    anti = op$anti,
    by = op$by,
    where = where_sql,
    na_matches = op$by$na_matches
  )
}

# Built query -------------------------------------------------------------

#' @export
#' @rdname sql_build
semi_join_query <- function(
  x,
  y,
  vars,
  anti = FALSE,
  by = NULL,
  where = NULL,
  na_matches = FALSE
) {
  query(
    "semi_join",
    x = x,
    y = y,
    vars = vars,
    anti = anti,
    by = by,
    where = where,
    na_matches = na_matches
  )
}

#' @export
sql_render.semi_join_query <- function(
  query,
  con = NULL,
  ...,
  sql_options = NULL,
  subquery = FALSE,
  lvl = 0
) {
  from_x <- sql_render(query$x, con, ..., subquery = TRUE, lvl = lvl + 1)
  from_y <- sql_render(query$y, con, ..., subquery = TRUE, lvl = lvl + 1)

  dbplyr_query_semi_join(
    con,
    from_x,
    from_y,
    vars = query$vars,
    anti = query$anti,
    by = query$by,
    where = query$where,
    lvl = lvl
  )
}

#' @export
flatten_query.semi_join_query <- function(qry, query_list, con) {
  flatten_query_2_tables(qry, query_list, con)
}

# SQL generation ----------------------------------------------------------

#' @rdname db-sql
#' @export
sql_query_semi_join <- function(
  con,
  x,
  y,
  anti,
  by,
  where,
  vars,
  ...,
  lvl = 0
) {
  check_dots_used()
  UseMethod("sql_query_semi_join")
}
#' @export
sql_query_semi_join.DBIConnection <- function(
  con,
  x,
  y,
  anti,
  by,
  where,
  vars,
  ...,
  lvl = 0
) {
  x <- dbplyr_sql_subquery(con, x, name = by$x_as)
  y <- dbplyr_sql_subquery(con, y, name = by$y_as)

  on <- sql_join_tbls(con, by, na_matches = by$na_matches)

  exists <- if (anti) "NOT EXISTS" else "EXISTS"

  lines <- list(
    sql_clause_select(escape(vars, con = con)),
    sql_clause_from(x),
    sql_glue2(con, "WHERE {.sql exists} ("),
    # lvl = 1 because they are basically in a subquery
    sql_clause("SELECT 1 FROM", y, lvl = 1),
    sql_clause_where(c(on, where), lvl = 1),
    sql(")")
  )
  sql_format_clauses(lines, lvl, con)
}

dbplyr_query_semi_join <- function(
  con,
  x,
  y,
  anti = FALSE,
  by = NULL,
  ...,
  lvl = 0
) {
  check_2ed(con)
  sql_query_semi_join(con, x, y, anti = anti, by = by, ..., lvl = lvl)
}
