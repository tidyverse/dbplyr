#' @export
#' @rdname sql_build
lazy_query <- function(query_type,
                       x,
                       ...,
                       group_vars = op_grps(x),
                       order_vars = op_sort(x),
                       frame = op_frame(x)) {
  stopifnot(is.null(group_vars) || is.character(group_vars), is.null(names(group_vars)))
  stopifnot(is.null(order_vars) || is_expression_list(order_vars), is.null(names(order_vars)))
  stopifnot(is.null(frame) || is_integerish(frame$range, n = 2))

  structure(
    list(
      x = x,
      ...,
      group_vars = group_vars,
      order_vars = order_vars,
      frame = frame
    ),
    class = c(paste0("lazy_", query_type, "_query"), "lazy_query")
  )
}

is_expression_list <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }

  purrr::every(x, is_expression)
}
