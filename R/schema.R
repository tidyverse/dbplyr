#' Refer to a table in a schema
#'
#' @param schema,table Names of schema and table.
#' @export
#' @examples
#' in_schema("my_schema", "my_table")
#'
#' # Example using schemas with SQLite
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'
#' # Add auxilary schema
#' tmp <- tempfile()
#' DBI::dbExecute(con, paste0("ATTACH '", tmp, "' AS aux"))
#'
#' library(dplyr, warn.conflicts = FALSE)
#' copy_to(con, iris, "df", temporary = FALSE)
#' copy_to(con, mtcars, in_schema("aux", "df"), temporary = FALSE)
#'
#' con %>% tbl("df")
#' con %>% tbl(in_schema("aux", "df"))
in_schema <- function(schema, table) {
  ident_q(paste0(schema, ".", table))
}
