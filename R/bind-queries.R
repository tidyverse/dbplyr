#' Combine multiple lazy queries
#'
#' Combine multiple lazy queries into a single query using `UNION ALL`.
#' This is a convenient wrapper around `purrr::reduce(tables, union_all)`.
#' Like [dplyr::bind_rows()] (and unlike `UNION ALL`), `bind_queries()` will
#' automatically align columns based on their name, and fill in any missing
#' columns with missing values.
#'
#' @param ... <[dynamic-dots][rlang::dyn-dots]> Lazy tables to combine.
#' @returns A lazy query.
#' @export
#' @examples
#' lf1 <- lazy_frame(x = 1, y = "a")
#' lf2 <- lazy_frame(x = 2, y = "b")
#' bind_queries(lf1, lf2)
#'
#' lf3 <- lazy_frame(y = "c", x = 3, z = 10)
#' bind_queries(lf2, lf3)
#'
#' # If you already have a list, you can use splice operator
#' queries <- list(lf1, lf2)
#' bind_queries(!!!queries)
bind_queries <- function(...) {
  tables <- list2(...)
  if (length(tables) == 0) {
    cli_abort("{.fn bind_queries} requires at least one input.")
  }
  for (i in seq_along(tables)) {
    if (!is_tbl_lazy(tables[[i]])) {
      stop_input_type(tables[[i]], "a lazy query", arg = paste0("..", i))
    }
  }

  purrr::reduce(tables, union_all)
}
