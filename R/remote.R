#' Metadata about a remote table
#'
#' `remote_name()` gives the name remote table, or `NULL` if it's a query.
#' `remote_query()` gives the text of the query, and `remote_query_plan()`
#' the query plan (as computed by the remote database). `remote_src()` and
#' `remote_con()` give the dplyr source and DBI connection respectively.
#'
#' @param x Remote table, currently must be a [tbl_sql].
#' @param cte `r lifecycle::badge("experimental")`
#'   Use common table expressions in the generated SQL?
#' @param ... Additional arguments passed on to methods.
#' @return The value, or `NULL` if not remote table, or not applicable.
#'    For example, computed queries do not have a "name"
#' @export
#' @examples
#' mf <- memdb_frame(x = 1:5, y = 5:1, .name = "blorp")
#' remote_name(mf)
#' remote_src(mf)
#' remote_con(mf)
#' remote_query(mf)
#'
#' mf2 <- dplyr::filter(mf, x > 3)
#' remote_name(mf2)
#' remote_src(mf2)
#' remote_con(mf2)
#' remote_query(mf2)
remote_name <- function(x) {
  lq <- x$lazy_query
  if (inherits(lq, "lazy_base_remote_query")) {
    return(lq$x)
  }

  if (!is_lazy_select_query_simple(lq, ignore_group_by = TRUE, select = "identity")) {
    return()
  }

  lq$x$x
}

query_name <- function(x, ...) {
  UseMethod("query_name")
}

#' @export
query_name.tbl_lazy <- function(x, ...) {
  query_name(x$lazy_query)
}

#' @export
query_name.lazy_base_remote_query <- function(x, ...) {
  x$x
}

#' @export
query_name.lazy_base_local_query <- function(x, ...) {
  ident(x$name)
}

#' @export
query_name.lazy_query <- function(x, ...) {
  NULL
}

#' @export
#' @rdname remote_name
remote_src <- function(x) {
  x$src
}

#' @export
#' @rdname remote_name
remote_con <- function(x) {
  x$src$con
}

#' @export
#' @rdname remote_name
remote_query <- function(x, cte = FALSE) {
  db_sql_render(remote_con(x), x, cte = cte)
}

#' @export
#' @rdname remote_name
remote_query_plan <- function(x, ...) {
  dbplyr_explain(remote_con(x), db_sql_render(remote_con(x), x$lazy_query), ...)
}
