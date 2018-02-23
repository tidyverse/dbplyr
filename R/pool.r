#' Pool methods for dplyr and dbplyr functions
#'
#' Pool object wrappers around DBIConnection methods, whose generics are
#' defined either in \code{dplyr} or in \code{dbplyr}.
#' For the original documentation, see
#' \href{http://dplyr.tidyverse.org/reference/index.html}{dplyr's reference page}
#' and \href{http://dbplyr.tidyverse.org/reference/index.html}{dbplyr's reference page}.
#'
#' @param dest,df,name,overwrite,temporary,...,src,from,con,table,columns,unique,indexes,types,fields,x,force,sql,values,y,vars,type,by,select,where,group_by,having,order_by,limit,distinct,anti,n,warn_incomplete,unique_indexes,analyze See original documentation.
#'
#' @name pool-methods
#'
#' @examples
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   library(dplyr)
#'
#'   pool <- dbPool(RSQLite::SQLite(), dbname = ":memory:")
#'
#'   # describe the type of the pool/its connections
#'   db_desc(pool)
#'
#'   # use dplyr syntax to copy a table into the database
#'   copy_to(pool, mtcars, "mtcars", temporary = FALSE)
#'
#'   # list the current tables in the database
#'   db_list_tables(pool)
#'
#'   # extract a table from the database
#'   mtcars_db <- tbl(pool, "mtcars")
#'
#'   # select only 3 columns
#'   mtcars_db_thin <- select(mtcars_db, mpg, cyl, disp)
#'
#'   # get the names of the columns in the databases's table
#'   db_query_fields(pool, "mtcars")
#'
#'   # get the number of rows in the databases's table
#'   db_query_rows(pool, "mtcars")
#'
#'   # drop the "mtcars" table from the database
#'   db_drop_table(pool, "mtcars")
#'
#'   # list the current tables in the database
#'   db_list_tables(pool)
#'
#'   poolClose(pool)
#'
#' } else {
#'   message("Please install the 'RSQLite' package to run this example")
#' }
NULL

stopIfTemporary <- function(temporary) {
  temporaryErrorMessage <- paste0("You cannot use `temporary = TRUE` ",
                                  "when using a Pool object, since temporary tables are local to a ",
                                  "connection, and there's no guarantee you'll get the same ",
                                  "connection back next time. You must either create a permanent ",
                                  "table, or checkout a connection from `pool` directly with ",
                                  "`con <- poolCheckout(pool)`, and then release the connection ",
                                  "back to the pool when you're finished (`poolReturn(con)`).")
  if (temporary) stop(temporaryErrorMessage)
}

# --- These generics are set in dplyr (not database-specific)
#' @export
#' @rdname pool-methods
copy_to.Pool <- function(dest, df, name = deparse(substitute(df)),
                         overwrite = FALSE, temporary = TRUE, ...) {
  stopIfTemporary(temporary)
  db_con <- pool::poolCheckout(dest)
  on.exit(pool::poolReturn(db_con))
  dplyr::copy_to(db_con, df = df, name = name, overwrite = overwrite,
          temporary = temporary, ...)
}

#' @export
#' @rdname pool-methods
tbl.Pool <- function(src, from, ...) {
  db_con <- pool::poolCheckout(src)
  on.exit(pool::poolReturn(db_con))
  dplyr::tbl(db_con, from = from, ...)
}

# --- These generics are set in dplyr (database-specific)
#' @export
#' @rdname pool-methods
db_analyze.Pool <- function(con, table, ...) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::db_analyze(db_con, table = table, ...)
}

#' @export
#' @rdname pool-methods
db_begin.Pool <- function(con, ...) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::db_begin(db_con, ...)
}

#' @export
#' @rdname pool-methods
db_commit.Pool <- function(con, ...) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::db_commit(db_con, ...)
}

#' @export
#' @rdname pool-methods
db_create_index.Pool <- function(con, table, columns, name = NULL,
                                 unique = FALSE, ...) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::db_create_index(db_con, table = table, columns = columns,
                  name = name, unique = unique, ...)
}

#' @export
#' @rdname pool-methods
db_create_indexes.Pool <- function(con, table, indexes = NULL,
                                   unique = FALSE, ...) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::db_create_indexes(db_con, table = table, indexes = indexes,
                    unique = unique, ...)
}

#' @export
#' @rdname pool-methods
db_create_table.Pool <- function(con, table, types, temporary = FALSE, ...) {
  stopIfTemporary(temporary)
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::db_create_table(db_con, table = table, types = types,
                  temporary = temporary, ...)
}

#' @export
#' @rdname pool-methods
db_data_type.Pool <- function(con, fields) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::db_data_type(db_con, fields = fields)
}

#' @export
#' @rdname pool-methods
db_desc.Pool <- function(x) {
  db_con <- pool::poolCheckout(x)
  on.exit(pool::poolReturn(db_con))
  dplyr::db_desc(db_con)
}

#' @export
#' @rdname pool-methods
db_drop_table.Pool <-  function(con, table, force = FALSE, ...) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::db_drop_table(db_con, table = table, force = force, ...)
}

#' @export
#' @rdname pool-methods
db_explain.Pool <- function(con, sql, ...) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::db_explain(db_con, sql = sql, ...)
}

#' @export
#' @rdname pool-methods
db_has_table.Pool <- function(con, table) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::db_has_table(db_con, table = table)
}

#' @export
#' @rdname pool-methods
db_insert_into.Pool <- function(con, table, values, ...) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::db_insert_into(db_con, table = table, values = values, ...)
}

#' @export
#' @rdname pool-methods
db_list_tables.Pool <- function(con) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::db_list_tables(db_con)
}

#' @export
#' @rdname pool-methods
db_query_fields.Pool <- function(con, sql, ...) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::db_query_fields(db_con, sql = sql, ...)
}

#' @export
#' @rdname pool-methods
db_query_rows.Pool <- function(con, sql, ...) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::db_query_rows(db_con, sql = sql, ...)
}

#' @export
#' @rdname pool-methods
db_rollback.Pool <- function(con, ...) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::db_rollback(db_con, ...)
}

#' @export
#' @rdname pool-methods
db_save_query.Pool <- function(con, sql, name, temporary = TRUE, ...) {
  stopIfTemporary(temporary)
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::db_save_query(db_con, sql = sql, name = name,
                temporary = temporary, ...)
}

#' @export
#' @rdname pool-methods
db_write_table.Pool <- function(con, table, types, values,
                                temporary = FALSE, ...) {
  stopIfTemporary(temporary)
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::db_write_table(db_con, table = table, types = types,
                 values = values, temporary = temporary, ...)
}

#' @export
#' @rdname pool-methods
sql_escape_ident.Pool <- function(con, x) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::sql_escape_ident(db_con, x = x)
}

#' @export
#' @rdname pool-methods
sql_escape_string.Pool <- function(con, x) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::sql_escape_string(db_con, x = x)
}

#' @export
#' @rdname pool-methods
sql_join.Pool <- function(con, x, y, vars, type = "inner", by = NULL, ...) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::sql_join(db_con, x = x, y = y, vars = vars, type = type,
           by = by, ...)
}

#' @export
#' @rdname pool-methods
sql_select.Pool <- function(con, select, from, where = NULL,
                            group_by = NULL, having = NULL, order_by = NULL, limit = NULL,
                            distinct = FALSE, ...) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::sql_select(db_con, select = select, from = from,
             where = where, group_by = group_by, having = having,
             order_by = order_by, limit = limit, distinct = distinct, ...)
}

#' @export
#' @rdname pool-methods
sql_semi_join.Pool <- function(con, x, y, anti = FALSE, by = NULL, ...) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::sql_semi_join(db_con, x = x, y = y, anti = anti, by = by, ...)
}

random_table_name <- function(n = 10) {
  paste0(sample(letters, n, replace = TRUE), collapse = "")
}

#' @export
#' @rdname pool-methods
sql_subquery.Pool <- function(con, from,
                              name = random_table_name(), ...) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::sql_subquery(db_con, from = from, name = name, ...)
}

#' @export
#' @rdname pool-methods
sql_translate_env.Pool <- function(con) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  dplyr::sql_translate_env(db_con)
}

# --- These generics are set in dbplyr (database-specific)
#' @export
#' @rdname pool-methods
db_collect.Pool <- function(con, sql, n = -1, warn_incomplete = TRUE, ...) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  db_collect(db_con, sql = sql, n = n,
             warn_incomplete = warn_incomplete, ...)
}

#' @export
#' @rdname pool-methods
db_compute.Pool <- function(con, table, sql, temporary = TRUE,
                            unique_indexes = list(), indexes = list(), ...) {
  stopIfTemporary(temporary)
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  db_compute(db_con, table = table, sql = sql,
             temporary = temporary, unique_indexes = unique_indexes,
             indexes = indexes, ...)
}

#' @export
#' @rdname pool-methods
db_copy_to.Pool <- function(con, table, values, overwrite = FALSE,
                            types = NULL, temporary = TRUE, unique_indexes = NULL,
                            indexes = NULL, analyze = TRUE, ...) {
  stopIfTemporary(temporary)
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  db_copy_to(db_con, table = table, values = values,
             overwrite = overwrite, types = types, temporary = temporary,
             unique_indexes = unique_indexes, indexes = indexes,
             analyze = analyze, ...)
}

#' @export
#' @rdname pool-methods
db_sql_render.Pool <- function(con, sql, ...) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  db_sql_render(db_con, sql = sql, ...)
}

#' @export
#' @rdname pool-methods
sql_escape_logical.Pool <- function(con, x) {
  db_con <- pool::poolCheckout(con)
  on.exit(pool::poolReturn(db_con))
  sql_escape_logical(db_con, x = x)
}
