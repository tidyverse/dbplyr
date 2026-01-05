#' @export
#' @rdname sql_build
lazy_multi_join_query <- function(
  x,
  joins,
  table_names,
  select,
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

  check_has_names(
    select,
    c("name", "expr", "group_vars", "order_vars", "frame"),
    call = call
  )
  check_character(select$name, call = call)
  check_list(select$expr, call = call)

  lazy_query(
    query_type = "multi_join",
    x = x,
    joins = joins,
    table_names = table_names,
    select = select,
    distinct = distinct,
    group_vars = group_vars,
    order_vars = order_vars,
    frame = frame
  )
}

#' @export
op_vars.lazy_multi_join_query <- function(op) {
  op$select$name
}

#' @export
sql_build.lazy_multi_join_query <- function(op, con, ..., sql_options = NULL) {
  table_names_out <- generate_join_table_names(op$table_names, con)

  # Build tables mapping: .table1 -> alias1, .table2 -> alias2
  # Use lapply with indexing to preserve table_path class (as.list strips it)
  tables_context <- set_names(
    lapply(seq_along(table_names_out), function(i) table_names_out[i]),
    paste0(".table", seq_along(table_names_out))
  )

  tables <- set_names(c(list(op$x), op$joins$table), table_names_out)
  table_vars <- purrr::map(tables, op_vars)

  select_sql <- translate_join_select(
    con = con,
    select = op$select,
    tables = tables_context,
    table_vars = table_vars,
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

# Built query -------------------------------------------------------------

multi_join_query <- function(x, joins, table_names, select, distinct = FALSE) {
  query(
    "multi_join",
    x = x,
    joins = joins,
    table_names = table_names,
    select = select,
    distinct = distinct
  )
}

#' @export
sql_render.multi_join_query <- function(
  query,
  con = NULL,
  ...,
  sql_options = NULL,
  subquery = FALSE,
  lvl = 0
) {
  x <- sql_render(query$x, con, ..., subquery = TRUE, lvl = lvl + 1)
  query$joins$table <- purrr::map(
    query$joins$table,
    \(table) sql_render(table, con, ..., subquery = TRUE, lvl = lvl + 1)
  )

  sql_query_multi_join(
    con = con,
    x = x,
    joins = query$joins,
    table_names = query$table_names,
    by_list = query$by_list,
    select = query$select,
    distinct = query$distinct,
    lvl = lvl
  )
}

#' @export
flatten_query.multi_join_query <- function(qry, query_list, con) {
  x <- qry$x
  query_list_new <- flatten_query(x, query_list, con)
  qry$x <- get_subquery_name(x, query_list_new)

  for (i in vctrs::vec_seq_along(qry$joins)) {
    y <- qry$joins$table[[i]]
    query_list_new <- flatten_query(y, query_list_new, con)
    qry$joins$table[[i]] <- get_subquery_name(y, query_list_new)
  }

  # TODO reuse query
  name <- as_table_path(unique_subquery_name(), con)
  wrapped_query <- set_names(list(qry), name)

  query_list$queries <- c(query_list_new$queries, wrapped_query)
  query_list$name <- name
  query_list
}


# SQL generation ----------------------------------------------------------

#' @rdname db-sql
#' @export
sql_query_multi_join <- function(
  con,
  x,
  joins,
  table_names,
  by_list,
  select,
  ...,
  distinct = FALSE,
  lvl = 0
) {
  check_dots_used()
  UseMethod("sql_query_multi_join")
}

#' @export
#' @param vars tibble with six columns:
#'   * `table` `<tbl_lazy>`: the tables to join with.
#'   * `type` `<character>`: the join type (left, right, inner, full).
#'   * `by_x`, `by_y` `<list_of<character>>`: The columns to join by
#'   * `by_x_table_id` `<list_of<integer>>`: The table index where the join column
#'     comes from. This needs to be a list because a the join columns might come
#'     from different tables
#'   * `on` `<character>`
#'   * `na_matches` `<character>`: Either `"na"` or `"never"`.
#' @param select A named SQL vector.
#' @param table_names `<character>` The names of the tables.
#' @noRd
#' @examples
#' # Left join with *
#' df1 <- lazy_frame(x = 1, y = 1)
#' df2 <- lazy_frame(x = 1, z = 1)
#' df3 <- lazy_frame(x = 1, z2 = 1)
#'
#' tmp <- left_join(df1, df2, by = "x") |>
#'   left_join(df3, by = c("x", z = "z2"))
#' tibble(
#'   table = list(df1, df2),
#'   type = c("left", "left"),
#'   by_x = list("x", c("x", "z")),
#'   by_y = list("x", c("x", "z2")),
#'   by_x_table_id = list(1L, c(1L, 2L)),
#'   on = c(NA, NA),
#'   na_matches = c("never", "never")
#' )
sql_query_multi_join.DBIConnection <- function(
  con,
  x,
  joins,
  table_names,
  by_list,
  select,
  ...,
  distinct = FALSE,
  lvl = 0
) {
  if (vctrs::vec_duplicate_any(table_names)) {
    cli_abort("{.arg table_names} must be unique.")
  }

  from <- dbplyr_sql_subquery(con, x, name = table_names[[1]], lvl = lvl)
  names <- table_names[-1]
  tables <- joins$table
  types <- toupper(paste0(joins$type, " JOIN"))

  n_joins <- length(types)
  out <- vector("list", n_joins * 2)

  for (i in seq_len(n_joins)) {
    table <- dbplyr_sql_subquery(con, tables[[i]], name = names[[i]], lvl = lvl)
    out[[2 * i - 1]] <- sql_clause(types[[i]], table)

    by <- joins$by[[i]]
    on <- sql_join_tbls(con, by = by, na_matches = by$na_matches)
    out[[2 * i]] <- sql_clause("ON", on, sep = " AND", parens = TRUE, lvl = 1)
  }

  clauses <- list2(
    sql_clause_select(select, distinct),
    sql_clause_from(from),
    !!!out
  )
  sql_format_clauses(clauses, lvl = lvl)
}

# Helpers ----------------------------------------------------------------------

#' @param vars tibble with three columns:
#'   * `name` `<character>`: variable name in output.
#'   * `table` `<list_of<integer>>`: tables index in database.
#'   * `var` `<list_of<character>>`: variable names in database.
#'     If more than one, need to coalesce in output.
#' @param table_names `<character>`: table names indexed by `table`.
#' @param all_vars_list `<list_of<character>>` All variables in each table.
#' @noRd
sql_multi_join_select <- function(
  con,
  vars,
  table_vars,
  use_star = TRUE,
  qualify_all_columns = FALSE
) {
  all_vars <- tolower(unlist(table_vars, use.names = FALSE))
  if (qualify_all_columns) {
    duplicated_vars <- unique(all_vars)
  } else {
    duplicated_vars <- all_vars[vctrs::vec_duplicate_detect(all_vars)]
    duplicated_vars <- unique(duplicated_vars)
  }
  table_paths <- table_path(names(table_vars))

  out <- rep_named(vars$name, list())

  for (i in seq_along(table_paths)) {
    all_vars_i <- table_vars[[i]]
    vars_idx_i <- which(vars$table == i)
    used_vars_i <- vars$var[vars_idx_i]
    out_vars_i <- vars$name[vars_idx_i]

    if (
      use_star &&
        join_can_use_star(all_vars_i, used_vars_i, out_vars_i, vars_idx_i)
    ) {
      id <- vars_idx_i[[1]]
      out[[id]] <- sql_star(con, table_paths[i])
      names(out)[id] <- ""
    } else {
      out[vars_idx_i] <- purrr::map2(used_vars_i, i, function(var, table_idx) {
        sql_multi_join_var(
          con,
          var,
          table_paths[[table_idx]],
          duplicated_vars
        )
      })
    }
  }

  names_to_as(con, unlist(out))
}

join_can_use_star <- function(all_vars, used_vars, out_vars, idx) {
  # using `tbl.*` for a single variable is silly
  if (length(all_vars) <= 1) {
    return(FALSE)
  }

  # all variables need to be used in same order
  if (!identical(used_vars, all_vars)) {
    return(FALSE)
  }

  # they must not be renamed
  if (!identical(used_vars, out_vars)) {
    return(FALSE)
  }

  # the variables must form a sequence
  all(diff(idx) == 1)
}

sql_multi_join_var <- function(con, var, table_name, duplicated_vars) {
  if (tolower(var) %in% duplicated_vars) {
    sql_table_prefix(con, table_name, var)
  } else {
    sql_escape_ident(con, var)
  }
}

sql_join_tbls <- function(con, by, na_matches) {
  na_matches <- arg_match(na_matches, c("na", "never"))

  if (na_matches == "na" || length(by$x) + length(by$y) > 0) {
    lhs <- sql_table_prefix(con, by$x_as %||% "LHS", by$x)
    rhs <- sql_table_prefix(con, by$y_as %||% "RHS", by$y)

    if (na_matches == "na") {
      compare <- purrr::map_chr(seq_along(lhs), function(i) {
        op <- by$condition[[i]]
        if (op == "==") {
          sql_expr_matches(con, lhs[[i]], rhs[[i]])
        } else {
          sql_glue2(
            con,
            "({lhs[[i]]} {.sql op} {rhs[[i]]} OR ({lhs[[i]]} IS NULL AND {rhs[[i]]} IS NULL))"
          )
        }
      })
    } else {
      by$condition[by$condition == "=="] <- "="
      compare <- purrr::map_chr(seq_along(lhs), function(i) {
        sql_glue2(con, "{lhs[i]} {.sql by$condition[[i]]} {rhs[i]}")
      })
    }

    sql(compare)
  } else if (length(by$on) > 0) {
    by$on
  }
}

sql_table_prefix <- function(con, table, var) {
  if (!is_bare_character(var)) {
    cli_abort("{.arg var} must be a bare character.", .internal = TRUE)
  }

  var <- sql_escape_ident(con, var)
  table <- sql_escape_ident(con, table_path_name(table, con))
  sql(paste0(table, ".", var))
}

sql_star <- function(con, table) {
  table <- table_path_name(table, con)
  sql_glue2(con, "{.id table}.*")
}

# Translate join select expressions with table context --------------------------

# Try to extract table index and variable from a simple .tableN$var expression
# Returns list(table = N, var = "varname") or NULL if not a simple reference
parse_join_expr <- function(expr) {
  if (is_quosure(expr)) {
    expr <- quo_get_expr(expr)
  }

  # Check for .tableN$var pattern
  if (!is_call(expr, "$")) {
    return(NULL)
  }

  table_sym <- expr[[2]]
  var_sym <- expr[[3]]

  if (!is_symbol(table_sym) || !is_symbol(var_sym)) {
    return(NULL)
  }

  table_name <- as_string(table_sym)
  if (!grepl("^\\.table[0-9]+$", table_name)) {
    return(NULL)
  }

  table_idx <- as.integer(sub("^\\.table", "", table_name))
  var_name <- as_string(var_sym)

  list(table = table_idx, var = var_name)
}

# Translate select expressions for joins, supporting star optimization
translate_join_select <- function(
  con,
  select,
  tables,
  table_vars,
  use_star = TRUE,
  qualify_all_columns = FALSE
) {
  n_tables <- length(tables)
  table_paths <- table_path(names(table_vars))

  # Parse all expressions to extract table/var info
  parsed <- purrr::map(select$expr, parse_join_expr)

  # Check which expressions are simple table references
  is_simple <- !purrr::map_lgl(parsed, is.null)

  # For star optimization, we need to know which table each simple expr comes from
  all_vars <- tolower(unlist(table_vars, use.names = FALSE))
  if (qualify_all_columns) {
    duplicated_vars <- unique(all_vars)
  } else {
    duplicated_vars <- all_vars[vctrs::vec_duplicate_detect(all_vars)]
    duplicated_vars <- unique(duplicated_vars)
  }

  out <- rep_named(select$name, list())

  # Process table by table for star optimization
  for (i in seq_len(n_tables)) {
    all_vars_i <- table_vars[[i]]

    # Find expressions from this table
    expr_idxs <- which(purrr::map_lgl(parsed, \(p) !is.null(p) && p$table == i))

    if (length(expr_idxs) == 0) {
      next
    }

    used_vars <- purrr::map_chr(parsed[expr_idxs], "var")
    out_vars <- select$name[expr_idxs]

    if (
      use_star &&
        all(is_simple[expr_idxs]) &&
        join_can_use_star(all_vars_i, used_vars, out_vars, expr_idxs)
    ) {
      # Use star optimization
      id <- expr_idxs[[1]]
      out[[id]] <- sql_star(con, table_paths[i])
      names(out)[id] <- ""
    } else {
      # Translate each expression individually
      for (idx in expr_idxs) {
        p <- parsed[[idx]]
        out[[idx]] <- sql_multi_join_var(
          con,
          p$var,
          table_paths[[i]],
          duplicated_vars
        )
      }
    }
  }

  # Handle complex expressions (not simple table references)
  complex_idxs <- which(!is_simple)
  if (length(complex_idxs) > 0) {
    for (idx in complex_idxs) {
      out[[idx]] <- translate_sql_(
        dots = select$expr[idx],
        con = con,
        tables = tables,
        context = list(clause = "SELECT")
      )
    }
  }

  names_to_as(con, unlist(out))
}


# Join select helpers ------------------------------------------------------

# Create a table-qualified column expression like .table1$x
join_select_expr <- function(table_idx, var) {
  tbl_sym <- sym(paste0(".table", table_idx))
  var_sym <- sym(var)
  call("$", tbl_sym, var_sym)
}

# Create a COALESCE expression for full joins: coalesce(.table1$x, .table2$y)
join_select_coalesce <- function(var_x, var_y) {
  x_expr <- call("$", sym(".table1"), sym(var_x))
  y_expr <- call("$", sym(".table2"), sym(var_y))
  call("coalesce", x_expr, y_expr)
}

# Create a lazy select tibble from table indices and variable names
new_lazy_join_select <- function(names, table_idxs, vars) {
  exprs <- purrr::map2(table_idxs, vars, join_select_expr)
  tibble(
    name = names,
    expr = exprs,
    group_vars = rep_along(exprs, list(character())),
    order_vars = rep_along(exprs, list(NULL)),
    frame = rep_along(exprs, list(NULL))
  )
}
