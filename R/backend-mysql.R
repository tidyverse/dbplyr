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
db_desc.MySQL <- db_desc.MySQLConnection

#' @export
sql_translate_env.MySQLConnection <- function(con) {
  sql_variant(
    sql_translator(.parent = base_scalar,
      as.logical = function(x) {
        sql_expr(IF(x, TRUE, FALSE))
      },
      as.character = sql_cast("CHAR"),

      # string functions ------------------------------------------------
      paste = sql_paste(" "),
      paste0 = sql_paste(""),

      # stringr
      str_c = sql_paste(""),
      # https://dev.mysql.com/doc/refman/8.0/en/regexp.html
      # NB: case insensitive by default; could use REGEXP_LIKE for MySQL,
      # but available in MariaDB. A few more details at:
      # https://www.oreilly.com/library/view/mysql-cookbook/0596001452/ch04s11.html
      str_detect = sql_infix("REGEXP"),
      str_locate = function(string, pattern) {
        sql_expr(REGEXP_INSTR(!!string, !!pattern))
      },
      str_replace_all = function(string, pattern, replacement){
        sql_expr(regexp_replace(!!string, !!pattern, !!replacement))
      }
    ),
    sql_translator(.parent = base_agg,
      n = function() sql("COUNT(*)"),
      sd =  sql_aggregate("STDDEV_SAMP", "sd"),
      var = sql_aggregate("VAR_SAMP", "var"),
      str_flatten = function(x, collapse) {
        sql_expr(group_concat(!!x %separator% !!collapse))
      }
    ),
    sql_translator(.parent = base_win,
      n = function() {
        win_over(sql("COUNT(*)"), partition = win_current_group())
      },
      sd = win_aggregate("STDDEV_SAMP"),
      var = win_aggregate("VAR_SAMP"),
      # GROUP_CONCAT not currently available as window function
      # https://mariadb.com/kb/en/library/aggregate-functions-as-window-functions/
      str_flatten = win_absent("str_flatten")
    )
  )
}

#' @export
sql_translate_env.MariaDBConnection <- sql_translate_env.MySQLConnection
#' @export
sql_translate_env.MySQL <- sql_translate_env.MySQLConnection

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
db_has_table.MySQL <- db_has_table.MySQLConnection

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
db_create_index.MySQL <- db_create_index.MySQLConnection

#' @export
db_analyze.MySQLConnection <- function(con, table, ...) {
  sql <- build_sql("ANALYZE TABLE", as.sql(table), con = con)
  dbExecute(con, sql)
}

#' @export
db_analyze.MariaDBConnection <- db_analyze.MySQLConnection
#' @export
db_analyze.MySQL <- db_analyze.MySQLConnection

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

globalVariables(c("%separator%", "group_concat", "IF", "REGEXP_INSTR"))
