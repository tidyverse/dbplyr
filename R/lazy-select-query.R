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
                              select_operation = c("mutate", "summarise"),
                              message_summarise = NULL) {
  stopifnot(inherits(from, "lazy_query"))
  stopifnot(is_string(last_op))
  stopifnot(is.null(select) || is_lazy_sql_part(select))
  stopifnot(is_lazy_sql_part(where))
  # stopifnot(is.character(group_by))
  stopifnot(is_lazy_sql_part(order_by))
  stopifnot(is.null(limit) || (is.numeric(limit) && length(limit) == 1L))
  stopifnot(is.logical(distinct), length(distinct) == 1L)

  # stopifnot(is.null(group_vars) || (is.character(group_vars) && is.null(names(group_vars))))
  stopifnot(is_lazy_sql_part(order_vars), is.null(names(order_vars)))
  stopifnot(is.null(frame) || is_integerish(frame, n = 2, finite = TRUE))

  select <- select %||% syms(set_names(op_vars(from)))
  select_operation <- arg_match0(select_operation, c("mutate", "summarise"))

  stopifnot(is.null(message_summarise) || is_string(message_summarise))

  # inherit `group_vars`, `order_vars`, and `frame` from `from`
  group_vars <- group_vars %||% op_grps(from)
  order_vars <- order_vars %||% op_sort(from)
  frame <- frame %||% op_frame(from)

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
      last_op = last_op,
      message_summarise = message_summarise
    ),
    class = c("lazy_select_query", "lazy_query")
  )
}

is_lazy_sql_part <- function(x) {
  if (is.null(x)) return(TRUE)
  if (is_quosures(x)) return(TRUE)

  if (!is.list(x)) return(FALSE)
  purrr::every(x, ~ is_quosure(.x) || is_symbol(.x) || is_expression(.x))
}

new_lazy_select <- function(vars, group_vars = NULL, order_vars = NULL, frame = NULL) {
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

update_lazy_select <- function(select, vars) {
  vctrs::vec_as_names(names(vars), repair = "check_unique")

  sel_vars <- purrr::map_chr(vars, as_string)
  idx <- vctrs::vec_match(sel_vars, select$name)
  select <- vctrs::vec_slice(select, idx)
  select$name <- names(vars)
  select
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
  if (length(select))   cat("Select:   ", named_commas2(select), "\n", sep = "")
  if (length(x$where))    cat("Where:    ", named_commas2(x$where), "\n", sep = "")
  if (length(x$group_by)) cat("Group by: ", named_commas2(x$group_by), "\n", sep = "")
  if (length(x$order_by)) cat("Order by: ", named_commas2(x$order_by), "\n", sep = "")
  if (length(x$limit))    cat("Limit:    ", x$limit, "\n", sep = "")

  if (length(x$group_vars)) cat("group_vars: ", named_commas2(x$group_vars), "\n", sep = "")
  if (length(x$order_vars)) cat("order_vars: ", named_commas2(x$order_vars), "\n", sep = "")
  if (length(x$frame))    cat("frame:    ", x$frame, "\n", sep = "")
}

named_commas2 <- function(x) {
  x <- purrr::map_chr(x, as_label)
  nms <- names2(x)
  out <- ifelse(nms == "", x, paste0(nms, " = ", x))
  paste0(out, collapse = ", ")
}

#' @export
op_vars.lazy_query <- function(op) {
  op$select$name
}

#' @export
op_grps.lazy_query <- function(op) {
  # Find renamed variables
  vars <- purrr::set_names(op$select$expr, op$select$name)
  symbols <- purrr::keep(vars, is_symbol)
  new2old <- purrr::map_chr(symbols, as_string)
  old2new <- set_names(names(new2old), new2old)

  grps <- op$group_vars
  renamed <- grps %in% names(old2new)
  grps[renamed] <- old2new[grps[renamed]]
  grps
}

#' @export
op_sort.lazy_query <- function(op) {
  # Renaming (like for groups) cannot be done because:
  # * `order_vars` is a list of quosures
  # * variables needed in sorting can be dropped
  op$order_vars
}

#' @export
op_frame.lazy_query <- function(op) {
  op$frame
}

#' @export
op_desc.lazy_query <- function(op) {
  # TODO
}

#' @export
sql_build.lazy_select_query <- function(op, con, ...) {
  if (!is.null(op$message_summarise)) {
    inform(op$message_summarise)
  }

  select_sql <- get_select_sql(op$select, op$select_operation, op_vars(op$from), con)
  where_sql <- translate_sql_(op$where, con = con, context = list(clause = "WHERE"))

  select_query(
    from = sql_build(op$from, con),
    select = select_sql,
    where = where_sql,
    group_by = translate_sql_(op$group_by, con = con),
    order_by = translate_sql_(op$order_by, con = con),
    distinct = op$distinct,
    limit = op$limit
  )
}

get_select_sql <- function(select, select_operation, in_vars, con) {
  if (select_operation == "summarise") {
    select_expr <- set_names(select$expr, select$name)
    select_sql_list <- translate_sql_(select_expr, con, window = FALSE, context = list(clause = "SELECT"))
    select_sql <- sql_vector(select_sql_list, parens = FALSE, collapse = NULL, con = con)
    return(select_sql)
  }

  if (identical(select$name, in_vars) &&
      purrr::every(select$expr, is_symbol) &&
      identical(syms(select$name), select$expr)) {
    return(new_sql("*"))
  }

  select_sql <- purrr::pmap(
    select %>% transmute(
      dots = set_names(expr, name),
      vars_group = .data$group_vars,
      vars_order = .data$order_vars,
      vars_frame = .data$frame
    ),
    function(dots, vars_group, vars_order, vars_frame) {
      translate_sql_(
        list(dots), con,
        vars_group = translate_sql_(syms(vars_group), con),
        vars_order = translate_sql_(vars_order, con, context = list(clause = "ORDER")),
        vars_frame = vars_frame[[1]],
        context = list(clause = "SELECT")
      )
    }
  )

  sql(unlist(select_sql))
}
