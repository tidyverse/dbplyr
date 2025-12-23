#' @export
#' @rdname sql_build
lazy_rf_join_query <- function(
  x,
  y,
  type,
  by,
  table_names,
  vars,
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

  check_has_names(vars, c("name", "x", "y"), call = call)
  check_character(vars$name, call = call)
  check_character(vars$x, call = call)
  check_character(vars$y, call = call)

  lazy_query(
    query_type = "rf_join",
    x = x,
    y = y,
    type = type,
    by = by,
    table_names = table_names,
    vars = vars,
    group_vars = group_vars,
    order_vars = order_vars,
    frame = frame
  )
}

#' @export
op_vars.lazy_rf_join_query <- function(op) {
  op$vars$name
}

#' @export
sql_build.lazy_rf_join_query <- function(op, con, ..., sql_options = NULL) {
  table_names_out <- generate_join_table_names(op$table_names, con)

  vars_classic <- as.list(op$vars)
  vars_classic$all_x <- op_vars(op$x)
  vars_classic$all_y <- op_vars(op$y)

  by <- op$by
  by$x_as <- table_names_out[[1]]
  by$y_as <- table_names_out[[2]]

  select <- sql_rf_join_vars(
    con,
    type = op$type,
    vars = vars_classic,
    x_as = by$x_as,
    y_as = by$y_as,
    use_star = sql_options$use_star,
    qualify_all_columns = sql_options$qualify_all_columns
  )

  join_query(
    sql_build(op$x, con, sql_options = sql_options),
    sql_build(op$y, con, sql_options = sql_options),
    select = select,
    type = op$type,
    by = by,
    suffix = NULL, # it seems like the suffix is not used for rendering
    na_matches = by$na_matches
  )
}
