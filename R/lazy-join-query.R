#' @export
#' @rdname sql_build
lazy_multi_join_query <- function(
  x,
  joins,
  table_names,
  vars,
  distinct = FALSE,
  group_vars = op_grps(x),
  order_vars = op_sort(x),
  frame = op_frame(x),
  call = caller_env()
) {
  check_lazy_query(x, call = call)

  check_has_names(joins, c("table", "type", "by_x_table_id", "by"))
  check_character(joins$type, call = call)

  check_has_names(table_names, c("name", "from"), call = call)
  check_character(table_names$name, call = call)
  check_character(table_names$from, call = call)

  check_has_names(vars, c("name", "table", "var"), call = call)
  check_character(vars$name, call = call)
  check_integer(vars$table, call = call)
  check_character(vars$var, call = call)

  lazy_query(
    query_type = "multi_join",
    x = x,
    joins = joins,
    table_names = table_names,
    vars = vars,
    distinct = distinct,
    group_vars = group_vars,
    order_vars = order_vars,
    frame = frame
  )
}

join_check_by <- function(by, call) {
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
  join_check_by(by, call)

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

#' @export
print.lazy_semi_join_query <- function(x, ...) {
  cat_line("<SQL ", if (x$anti) "ANTI" else "SEMI", " JOIN>")

  cat_line("By:")
  cat_line(indent(paste0(x$by$x, "-", x$by$y)))

  if (length(x$where)) {
    cat_line("Where:")
    where <- purrr::map_chr(x$where, as_label)
    cat_line(indent(paste0(where)))
  }

  cat_line("X:")
  cat_line(indent_print(sql_build(x$x, simulate_dbi())))

  cat_line("Y:")
  cat_line(indent_print(sql_build(x$y, simulate_dbi())))
}

#' @export
op_vars.lazy_multi_join_query <- function(op) {
  op$vars$name
}
#' @export
op_vars.lazy_semi_join_query <- function(op) {
  op$vars$name
}

#' @export
sql_build.lazy_multi_join_query <- function(op, con, ..., sql_options = NULL) {
  table_names_out <- generate_join_table_names(op$table_names, con)

  tables <- set_names(c(list(op$x), op$joins$table), table_names_out)
  table_vars <- purrr::map(tables, op_vars)
  select_sql <- sql_multi_join_vars(
    con,
    op$vars,
    table_vars,
    use_star = sql_options$use_star,
    qualify_all_columns = sql_options$qualify_all_columns
  )

  op$joins$table <- purrr::map(
    op$joins$table,
    \(table) sql_build(table, con, sql_options = sql_options)
  )
  op$joins$by <- purrr::map2(
    op$joins$by,
    seq_along(op$joins$by),
    function(by, i) {
      by$x_as <- table_names_out[op$joins$by_x_table_id[[i]]]
      by$y_as <- table_names_out[i + 1L]
      by
    }
  )

  multi_join_query(
    x = sql_build(op$x, con, sql_options = sql_options),
    joins = op$joins,
    table_names = table_names_out,
    select = select_sql,
    distinct = op$distinct
  )
}

generate_join_table_names <- function(table_names, con) {
  names <- table_path_name(table_names$name, con)
  table_name_length_max <- max(nchar(names))

  if (length(table_names$name) != 2) {
    table_names_repaired <- vctrs::vec_as_names(
      names,
      repair = "unique",
      quiet = TRUE
    )
    may_repair_name <- table_names$from != "as"
    names[may_repair_name] <- table_names_repaired[may_repair_name]
  } else {
    names <- join_two_table_alias(names, table_names$from)
  }

  # avoid database aliases exceeding the database-specific maximum length
  abbr_names <- abbreviate(
    names,
    # arbitrarily floor at identifier limit for Postgres backend to avoid unnecessarily truncating reasonable lengths
    # Source: https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS
    # "By default, NAMEDATALEN is 64 so the maximum identifier length is 63 bytes."
    minlength = max(table_name_length_max, 63),
    # Explicitly set `strict = FALSE` (the default) to ensure table names are unique;
    # NB: non-zero (but low) chance that name is longer than DB limit
    strict = FALSE,
    named = FALSE,
    # Mitigation for non-zero chance of strings over limit:
    # don't over anchor to the start of the string,
    # since we opt to add qualifiers (...1, _{R/L}HS, etc.) to end of table name
    method = "both.sides"
  )

  as_table_paths(abbr_names, con)
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
