select_query2 <- function(from,
                          select = list(),
                          where = list(),
                          group_by = list(),
                          order_by = list(),
                          limit = NULL,
                          distinct = FALSE) {
  vctrs::vec_assert(from, character(), 1L)
  assert_list_of_sql_parts(select)
  assert_list_of_sql_parts(where)
  assert_list_of_sql_parts(group_by)
  assert_list_of_sql_parts(order_by)
  stopifnot(is.null(limit) || (is.numeric(limit) && length(limit) == 1L))
  stopifnot(is.logical(distinct), length(distinct) == 1L)

  structure(
    list(
      from = from,
      select = select,
      where = where,
      group_by = group_by,
      order_by = order_by,
      distinct = distinct,
      limit = limit
    ),
    class = c("select_query2", "query2")
  )
}

assert_list_of_sql_parts <- function(x) {
  vctrs::vec_assert(x, list())

  all_valid <- purrr::every(x, is_sql_part)
  if (!all_valid) {
    abort("Every part of `x` must be a quosure, a symbol, or sql.")
  }
}

is_sql_part <- function(x) {
  if (rlang::is_quosure(x)) return(TRUE)
  if (rlang::is_symbol(x)) return(TRUE)
  if (is.sql(x)) return(TRUE)

  FALSE
}
