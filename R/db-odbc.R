#' @export
db_desc.OdbcConnection <- function(x) {
  info <- DBI::dbGetInfo(x)

  host <- if (info$servername == "") "localhost" else info$servername
  port <- if (info$port == "") "" else paste0(":", port)

  paste0(
    info$dbms.name, " ", info$db.version,
    "[", info$username, "@", host, port,
    "/", info$dbname , "]")

}

#' @export
sql_escape_ident.OdbcConnection <- function(con, x) {
  x
}

#' @export
sql_escape_string.OdbcConnection <- function(con, x) {
  sql_quote(x, "'")
}

