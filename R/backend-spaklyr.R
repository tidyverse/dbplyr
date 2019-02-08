#' @export
sql_select.spark_connection <- function(con, select, from, where = NULL,
                                     group_by = NULL, having = NULL,
                                     order_by = NULL,
                                     limit = NULL,
                                     distinct = FALSE,
                                     ...) {
  out <- vector("list", 7)
  names(out) <- c("select", "from", "where", "group_by", "having", "order_by",
                  "limit")

  out$select    <- sql_clause_select(select, con, distinct)
  out$from      <- sql_clause_from(from, con)
  out$where     <- sql_clause_where(where, con)
  out$group_by  <- sql_clause_group_by(group_by, con)
  out$having    <- sql_clause_having(having, con)
  out$order_by  <- sql_clause_order_by(order_by, con)
  out$limit     <- sql_clause_limit(limit, con)

  escape(unname(purrr::compact(out)), collapse = "\n", parens = FALSE, con = con)
}
