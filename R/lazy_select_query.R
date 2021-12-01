#' @export
#' @rdname sql_build
lazy_select_query <- function(from,
                              last_op,
                              select = NULL,
                              where = NULL,
                              group_by = NULL,
                              order_by = NULL,
                              limit = NULL,
                              distinct = FALSE,
                              group_vars = NULL,
                              order_vars = NULL,
                              frame = NULL,
                              select_operation = c("mutate", "summarise")) {

  stopifnot(is_string(last_op))
  # TODO check arguments
  # stopifnot(is.character(select))
  # stopifnot(is.character(where))
  # stopifnot(is.character(group_by))
  # stopifnot(is.character(order_by))
  stopifnot(is.null(limit) || (is.numeric(limit) && length(limit) == 1L))
  stopifnot(is.logical(distinct), length(distinct) == 1L)
  # stopifnot(is.character(group_vars))
  # stopifnot(is.character(order_vars))
  # frame
  select_operation <- arg_match0(select_operation, c("mutate", "summarise"))

  if (is_null(group_vars)) {
    group_vars <- op_grps(from)
  }
  if (is_null(order_vars)) {
    order_vars <- op_sort(from)
  }
  if (is_null(frame)) {
    frame <- op_frame(from)
  }

  if (last_op == "mutate") {
    select <- new_lazy_select(
      select,
      group_vars = group_vars,
      order_vars = order_vars,
      frame = frame
    )
  } else {
    select <- new_lazy_select(select)
  }

  structure(
    list(
      from = from,
      select = select,
      where = where,
      group_by = group_by,
      order_by = order_by,
      distinct = distinct,
      limit = limit,
      group_vars = group_vars,
      order_vars = order_vars,
      frame = frame,
      select_operation = select_operation,
      last_op = last_op
    ),
    class = c("lazy_select_query", "lazy_query")
  )
}

new_lazy_select <- function(vars, group_vars = NULL, order_vars = NULL, frame = NULL) {
  if (!is_null(vars)) {
    vctrs::vec_as_names(names2(vars), repair = "check_unique")
  }

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

update_lazy_select <- function(lazy_query, vars) {
  vctrs::vec_as_names(names(vars), repair = "check_unique")
  select <- lazy_query$select

  sel_vars <- purrr::map_chr(vars, as_string)
  idx <- vctrs::vec_match(sel_vars, select$name)
  select <- vctrs::vec_slice(select, idx)
  select$name <- names(vars)
  lazy_query$select <- select


  group_vars <- lazy_query$group_vars
  group_vars_out <- syms(names(group_vars))
  idx <- vctrs::vec_match(group_vars_out, vars)
  names(group_vars)[group_vars_out %in% vars] <- names(vars[idx])
  lazy_query$group_vars <- group_vars

  order_vars <- lazy_query$order_vars
  order_vars_out <- syms(names(order_vars))
  idx <- vctrs::vec_match(order_vars_out, vars)
  names(order_vars)[order_vars_out %in% vars] <- names(vars[idx])
  lazy_query$order_vars <- order_vars

  lazy_query
}

#' @export
print.lazy_select_query <- function(x, ...) {
  cat(
    "<SQL SELECT",
    if (x$distinct) " DISTINCT", ">\n",
    sep = ""
  )
  cat_line("From:")
  cat_line(indent_print(sql_build(x$from, simulate_dbi())))

  select <- purrr::set_names(x$select$expr, x$select$name)
  if (length(select))   cat("Select:   ", named_commas2(!!!select), "\n", sep = "")
  if (length(x$where))    cat("Where:    ", named_commas(x$where), "\n", sep = "")
  if (length(x$group_by)) cat("Group by: ", named_commas(x$group_by), "\n", sep = "")
  if (length(x$order_by)) cat("Order by: ", named_commas(x$order_by), "\n", sep = "")
  if (length(x$limit))    cat("Limit:    ", x$limit, "\n", sep = "")

  if (length(x$group_vars)) cat("group_vars: ", named_commas(!!!x$group_vars), "\n", sep = "")
  if (length(x$order_vars)) cat("order_vars: ", named_commas(x$order_vars), "\n", sep = "")
  if (length(x$frame))    cat("frame:    ", x$frame, "\n", sep = "")
}

named_commas2 <- function(...) {
  x <- unlist(purrr::map(list2(...), as_label))
  if (is_null(names(x))) {
    paste0(x, collapse = ", ")
  } else {
    paste0(names(x), " = ", x, collapse = ", ")
  }
}

#' @export
op_vars.lazy_query <- function(x) {
  x$select$name
}

#' @export
op_grps.lazy_query <- function(x) {
  syms(set_names(names(x$group_vars)))
}

#' @export
op_sort.lazy_query <- function(x) {
  x$order_vars
}

#' @export
op_frame.lazy_query <- function(x) {
  x$frame
}

#' @export
op_desc.lazy_query <- function(x) {
  # TODO
}

#' @export
sql_build.lazy_query <- function(x, con, ...) {
  select_sql <- get_select_sql(x$select, x$select_operation, op_vars(x$from), con)
  where_sql <- translate_sql_(x$where, con = con, context = list(clause = "WHERE"))

  select_query(
    from = sql_build(x$from, con),
    select = select_sql,
    where = where_sql,
    group_by = translate_sql_(x$group_by, con = con),
    order_by = translate_sql_(x$order_by, con = con),
    distinct = x$distinct
  )
}

get_select_sql <- function(select, select_operation, in_vars, con) {
  if (select_operation == "summarise") {
    select_expr <- set_names(select$expr, select$name)
    select_sql <- translate_sql_(select_expr, con, window = FALSE, context = list(clause = "SELECT"))
    return(select_sql)
  }

  if (identical(select$name, in_vars) &&
      purrr::every(select$expr, is_symbol) &&
      identical(syms(select$name), select$expr)) {
    return(sql("*"))
  }

  select_sql <- purrr::pmap(
    select %>% transmute(
      dots = set_names(expr, name),
      vars_group = group_vars,
      vars_order = order_vars,
      vars_frame = frame
    ),
    function(dots, vars_group, vars_order, vars_frame) {
      translate_sql_(
        list(dots), con,
        vars_group = translate_sql_(unname(vars_group), con),
        vars_order = translate_sql_(vars_order, con, context = list(clause = "ORDER")),
        vars_frame = vars_frame,
        context = list(clause = "SELECT")
      )
    }
  )

  sql(unlist(select_sql))
}

#' @export
sql_build.select_query <- function(x, con, ...) {
  "select_query"
}
