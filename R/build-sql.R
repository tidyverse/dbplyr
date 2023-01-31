#' Build a SQL string.
#'
#' This is a convenience function that should prevent sql injection attacks
#' (which in the context of dplyr are most likely to be accidental not
#' deliberate) by automatically escaping all expressions in the input, while
#' treating bare strings as sql. This is unlikely to prevent any serious
#' attack, but should make it unlikely that you produce invalid sql.
#'
#' This function should be used only when generating `SELECT` clauses,
#' other high level queries, or for other syntax that has no R equivalent.
#' For individual function translations, prefer [sql_expr()].
#'
#' @param ... input to convert to SQL. Use [sql()] to preserve
#'   user input as is (dangerous), and [ident()] to label user
#'   input as sql identifiers (safe)
#' @param .env the environment in which to evaluate the arguments. Should not
#'   be needed in typical use.
#' @param con database connection; used to select correct quoting characters.
#' @keywords internal
#' @export
#' @examples
#' con <- simulate_dbi()
#' build_sql("SELECT * FROM TABLE", con = con)
#' x <- "TABLE"
#' build_sql("SELECT * FROM ", x, con = con)
#' build_sql("SELECT * FROM ", ident(x), con = con)
#' build_sql("SELECT * FROM ", sql(x), con = con)
#'
#' # http://xkcd.com/327/
#' name <- "Robert'); DROP TABLE Students;--"
#' build_sql("INSERT INTO Students (Name) VALUES (", name, ")", con = con)
build_sql <- function(..., .env = parent.frame(), con = sql_current_con()) {
  check_con(con)

  escape_expr <- function(x, con) {
    # If it's a string, leave it as is
    if (is.character(x)) return(x)

    val <- eval_bare(x, .env)
    # Skip nulls, so you can use if statements like in paste
    if (is.null(val)) return("")

    escape(val, con = con)
  }

  pieces <- purrr::map_chr(enexprs(...), escape_expr, con = con)
  sql(paste0(pieces, collapse = ""))
}
