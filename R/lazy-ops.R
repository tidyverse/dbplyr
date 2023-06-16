#' Lazy operations
#'
#' This set of S3 classes describe the action of dplyr verbs. These are
#' currently used for SQL sources to separate the description of operations
#' in R from their computation in SQL. This API is very new so is likely
#' to evolve in the future.
#'
#' `op_vars()` and `op_grps()` compute the variables and groups from
#' a sequence of lazy operations. `op_sort()` and `op_frame()` tracks the
#' order and frame for use in window functions.
#'
#' @keywords internal
#' @name lazy_ops
NULL

# Base constructors -------------------------------------------------------

#' @export
#' @rdname lazy_ops
lazy_base_query <- function(x, vars, class = character(), ...) {
  check_character(vars)

  lazy_query(
    query_type = c(paste0("base_", class), "base"),
    x = x,
    vars = vars,
    ...,
    group_vars = character(),
    order_vars = NULL,
    frame = NULL
  )
}

lazy_query_local <- function(df, name) {
  name <- as_table_ident(name)
  lazy_base_query(df, names(df), class = "local", name = name)
}

lazy_query_remote <- function(x, vars) {
  lazy_base_query(x, vars, class = "remote")
}

base_query <- function(from) {
  from <- as_from(from)
  structure(
    list(from = from),
    class = c("base_query", "query")
  )
}

#' @export
print.lazy_base_remote_query <- function(x, ...) {
  if (inherits(x$x, "ident")) {
    cat_line("From: ", format(x$x))
  } else {
    cat_line("From: <derived table>")
  }

  cat("<Table: ", x$x, ">\n", sep = "")
}

#' @export
print.lazy_base_local_query <- function(x, ...) {
  cat_line("<Local data frame> ", dplyr::dim_desc(x$x))
}

#' @export
sql_build.lazy_base_remote_query <- function(op, con, ...) {
  base_query(op$x)
}

#' @export
sql_build.lazy_base_local_query <- function(op, con, ...) {
  base_query(op$name)
}

#' @export
sql_render.base_query <- function(query, con = NULL, ..., subquery = FALSE, lvl = 0, cte = FALSE) {
  from <- query$from
  if (subquery || is.sql(from)) {
    from
  } else {
    from <- escape(from, con = con)
    dbplyr_query_select(con, sql("*"), from, lvl = lvl)
  }
}

#' @export
print.base_query <- function(x, ...) {
  print(x$from)
}

#' @export
flatten_query.base_query <- function(qry, query_list) {
  query_list
}

# op_grps -----------------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_grps <- function(op) UseMethod("op_grps")
#' @export
op_grps.tbl_lazy <- function(op) op_grps(op$lazy_query)
#' @export
op_grps.lazy_query <- function(op) op$group_vars %||% character()

# op_vars -----------------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_vars <- function(op) UseMethod("op_vars")
#' @export
op_vars.tbl_lazy <- function(op) op_vars(op$lazy_query)
#' @export
op_vars.lazy_base_query <- function(op) op$vars

# op_sort -----------------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_sort <- function(op) UseMethod("op_sort")
#' @export
op_sort.tbl_lazy <- function(op) op_sort(op$lazy_query)
#' @export
op_sort.lazy_query <- function(op) {
  # Renaming (like for groups) cannot be done because:
  # * `order_vars` is a list of quosures
  # * variables needed in sorting can be dropped
  op$order_vars
}

# op_frame ----------------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_frame <- function(op) UseMethod("op_frame")
#' @export
op_frame.tbl_lazy <- function(op) op_frame(op$lazy_query)
#' @export
op_frame.lazy_query <- function(op) op$frame

# Description -------------------------------------------------------------

op_rows <- function(op, rows_total = NA_integer_) {
  if (is.na(rows_total)) {
    "??"
  } else {
    big_mark(rows_total)
  }
}
op_cols <- function(op) {
  length(op_vars(op))
}

op_desc <- function(op) UseMethod("op_desc")

#' @export
op_desc.lazy_base_remote_query <- function(op) {
  table <- remote_name(op)
  if (is.null(table)) {
    "SQL"
  } else {
    paste0("table<", table, ">")
  }
}
