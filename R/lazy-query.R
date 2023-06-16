#' @export
#' @rdname sql_build
lazy_query <- function(query_type,
                       x,
                       ...,
                       group_vars = op_grps(x),
                       order_vars = op_sort(x),
                       frame = op_frame(x)) {
  stopifnot(is.null(group_vars) || (is.character(group_vars) && is.null(names(group_vars))))
  stopifnot(is_lazy_sql_part(order_vars), is.null(names(order_vars)))
  check_frame(frame)

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
