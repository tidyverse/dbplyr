#' @export
db_desc.MySQLConnection <- function(x) {
  info <- dbGetInfo(x)

  paste0(
    "mysql ", info$serverVersion, " [",
    info$user, "@", info$host, ":", info$port, "/", info$dbname,
    "]"
  )
}

#' @export
db_desc.MariaDBConnection <- db_desc.MySQLConnection

#' @export
sql_translate_env.MySQLConnection <- function(con) {
  sql_variant(
    sql_translator(.parent = base_scalar,
      as.character = sql_cast("CHAR"),
      paste = sql_paste(" "),
      paste0 = sql_paste("")
    ),
    sql_translator(.parent = base_agg,
      n = function() sql("COUNT(*)"),
      sd =  sql_aggregate("stddev_samp"),
      var = sql_aggregate("var_samp"),
      str_flatten = function(x, collapse) {
        sql_expr(group_concat(!!x %separator% !!collapse))
      }
    ),
    base_no_win
  )
}

#' @export
sql_translate_env.MariaDBConnection <- sql_translate_env.MySQLConnection

# DBI methods ------------------------------------------------------------------

#' @export
db_has_table.MySQLConnection <- function(con, table, ...) {
  # MySQL has no way to list temporary tables, so we always NA to
  # skip any local checks and rely on the database to throw informative errors
  NA
}

#' @export
db_has_table.MariaDBConnection <- db_has_table.MySQLConnection

#' @export
db_data_type.MySQLConnection <- function(con, fields, ...) {
  char_type <- function(x) {
    n <- max(nchar(as.character(x), "bytes"), 0L, na.rm = TRUE)
    if (n <= 65535) {
      paste0("varchar(", n, ")")
    } else {
      "mediumtext"
    }
  }

  data_type <- function(x) {
    switch(
      class(x)[1],
      logical =   "boolean",
      integer =   "integer",
      numeric =   "double",
      factor =    char_type(x),
      character = char_type(x),
      Date =      "date",
      POSIXct =   "datetime",
      stop("Unknown class ", paste(class(x), collapse = "/"), call. = FALSE)
    )
  }
  vapply(fields, data_type, character(1))
}

#' @export
db_begin.MySQLConnection <- function(con, ...) {
  dbExecute(con, "START TRANSACTION")
}

#' @export
db_commit.MySQLConnection <- function(con, ...) {
  dbExecute(con, "COMMIT")
}

#' @export
db_rollback.MySQLConnection <- function(con, ...) {
  dbExecute(con, "ROLLBACK")
}

#' @export
db_write_table.MySQLConnection <- function(con, table, types, values,
                                           temporary = TRUE, ...) {
  db_create_table(con, table, types, temporary = temporary)

  values <- purrr::modify_if(values, is.logical, as.integer)
  values <- purrr::modify_if(values, is.factor, as.character)
  values <- purrr::modify_if(values, is.character, encodeString, na.encode = FALSE)

  tmp <- tempfile(fileext = ".csv")
  utils::write.table(values, tmp, sep = "\t", quote = FALSE, qmethod = "escape",
    na = "\\N", row.names = FALSE, col.names = FALSE)

  sql <- build_sql("LOAD DATA LOCAL INFILE ", encodeString(tmp), " INTO TABLE ",
    as.sql(table), con = con)
  dbExecute(con, sql)

  table
}

#' @export
db_create_index.MySQLConnection <- function(con, table, columns, name = NULL,
                                            unique = FALSE, ...) {
  name <- name %||% paste0(c(table, columns), collapse = "_")
  fields <- escape(ident(columns), parens = TRUE, con = con)
  index <- build_sql(
    "ADD ",
    if (unique) sql("UNIQUE "),
    "INDEX ", ident(name), " ", fields,
    con = con
  )

  sql <- build_sql("ALTER TABLE ", as.sql(table), "\n", index, con = con)
  dbExecute(con, sql)
}

#' @export
db_create_index.MariaDBConnection <- db_create_index.MySQLConnection

#' @export
db_analyze.MySQLConnection <- function(con, table, ...) {
  sql <- build_sql("ANALYZE TABLE", as.sql(table), con = con)
  dbExecute(con, sql)
}

#' @export
db_analyze.MariaDBConnection <- db_analyze.MySQLConnection

# SQL methods -------------------------------------------------------------

#' @export
sql_escape_ident.MySQLConnection <- function(con, x) {
  sql_quote(x, "`")
}

#' @export
sql_join.MySQLConnection <- function(con, x, y, vars, type = "inner", by = NULL, ...) {
  if (identical(type, "full")) {
    stop("MySQL does not support full joins", call. = FALSE)
  }
  NextMethod()
}

globalVariables(c("%separator%", "group_concat"))
