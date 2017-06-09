#' @export
sql_select.Oracle<- function(con, select, from, where = NULL,
                             group_by = NULL, having = NULL,
                             order_by = NULL,
                             limit = NULL,
                             distinct = FALSE,
                             ...) {
  out <- vector("list", 7)
  names(out) <- c("select", "from", "where", "group_by", "having", "order_by",
                  "limit")

  assertthat::assert_that(is.character(select), length(select) > 0L)
  out$select <- build_sql(
    "SELECT ",
    if (distinct) sql("DISTINCT "),
    escape(toupper(select), collapse = ", ", con = con)
  )

  assertthat::assert_that(is.character(from), length(from) == 1L)
  out$from <- build_sql("FROM ", from, con = con)

  if (length(where) > 0L) {
    assertthat::assert_that(is.character(where))

    where_paren <- escape(where, parens = TRUE, con = con)
    out$where <- build_sql("WHERE ", sql_vector(where_paren, collapse = " AND "))
  }

  if (length(group_by) > 0L) {
    assertthat::assert_that(is.character(group_by))
    out$group_by <- build_sql(
      "GROUP BY ",
      escape(group_by, collapse = ", ", con = con)
    )
  }

  if (length(having) > 0L) {
    assertthat::assert_that(is.character(having))
    out$having <- build_sql(
      "HAVING ",
      escape(having, collapse = ", ", con = con)
    )
  }

  if (length(order_by) > 0L) {
    assertthat::assert_that(is.character(order_by))
    out$order_by <- build_sql(
      "ORDER BY ",
      escape(order_by, collapse = ", ", con = con)
    )
  }

  # Using Oracle's FETCH FIRST SQL command instead of LIMIT
  # https://oracle-base.com/articles/12c/row-limiting-clause-for-top-n-queries-12cr1

  if (!is.null(limit) && !identical(limit, Inf)) {
    assertthat::assert_that(is.numeric(limit), length(limit) == 1L, limit > 0)
    out$limit <- build_sql(
      "FETCH FIRST ", sql(format(trunc(limit), scientific = FALSE)), " ROWS ONLY ",
      con = con
    )
  }

  escape(unname(plyr::compact(out)), collapse = "\n", parens = FALSE, con = con)
}


#' @export
sql_translate_env.Oracle <- function(con) {
  sql_variant(
    scalar = sql_translator(.parent = base_odbc_scalar,
                            as.character  = sql_cast("VARCHAR(255)"),
                            as.numeric = sql_cast("NUMERIC"),
                            as.double = sql_cast("NUMERIC")
    ) ,
    base_odbc_agg,
    base_odbc_win
  )
}


#' @export
sql_subquery.Oracle <- function(con, from, name = dbplyr:::unique_name(), ...) {
  # Query fails if a quoted table is used.  Using sql() to remove quotes from an
  # already 'idented' argument.
  if (is.ident(from)) {
    if (is.null(name)) {
      build_sql("(", sql(from), ") ", con = con)
    } else {
      build_sql("(", sql(from), ") ", ident(name), con = con)
    }
  } else {
    if (is.null(name)) {
      build_sql("(", from, ")", con = con)
    } else {
      build_sql("(", from, ") ", ident(name), con = con)
    }
  }
}

#' @export
db_analyze.Oracle <- function(con, table, ...) {
  sql <- dbplyr::build_sql(
    "ANALYZE TABLE ",
    dbplyr::ident(table)
    , con = con)
  DBI::dbExecute(con, sql)
}


#' @export
setMethod(
  "dbListTables", "Oracle",
  function(conn, ...) {
    table_names <- dbGetQuery(con, "select table_name from dba_tables")
    table_names$upper <- toupper(table_names$TABLE_NAME)
    table_names <- table_names[table_names$TABLE_NAME == table_names$upper , 1]
    as.character(table_names)
  })

#' @export
setMethod(
  "dbExistsTable", c("Oracle", "character"),
  function(conn, name, ...) {
    stopifnot(length(name) == 1)
    name <- toupper(name)
    dbUnQuoteIdentifier(conn, name) %in% dbListTables(conn)
  })



#' @export
setMethod(
  "dbWriteTable", c("Oracle", "character", "data.frame"),
  function(conn, name, value, overwrite=FALSE, append=FALSE, temporary = FALSE,
           row.names = NA, ...) {

    # Oracle requires case matching for the table in INSERT
    # operation, easier to upper case name in the beginning

    name <- toupper(name)

    if (overwrite && append)
      stop("overwrite and append cannot both be TRUE", call. = FALSE)

    found <- dbExistsTable(conn, name)
    if (found && !overwrite && !append) {
      stop("Table ", name, " exists in database, and both overwrite and",
           " append are FALSE", call. = FALSE)
    }

    tryCatch({

      # Using transactions to both create and insert into the table, like in a
      # a Store Procedure. This enables us to execute multiple smaller INSERT INTO
      # commands instead of one large string with all of the values that may
      # overflow a string buffer and to prevent leaving an empty table behind
      # if the INSERT INTO fails. As of 5/28/17, the full ROLLBACK does not work
      # we may need support for END and SAVEPOINT (
      # https://stackoverflow.com/questions/11966020/begin-end-block-atomic-transactions-in-pl-sql
      # )

      dbBegin(conn)

      if (found && overwrite) {
        dbRemoveTable(conn, name)
      }

      values <- sqlData(conn, row.names = row.names, value[, , drop = FALSE])

      if (!found || overwrite) {
        # Oracle does not like quote marks for table and field names
        # conn@quote will be set to a single quote in the section
        # where we insert the records
        conn@quote <- ""
        sql <- sqlCreateTable(conn, name, values, row.names = FALSE, temporary = temporary)
        dbExecute(conn, sql)
      }


      if (nrow(value) > 0) {

        conn@quote <- "'"
        fields <- paste0(colnames(values), ", ", collapse = "")
        fields <- substr(fields, 1, nchar(fields) - 2)

        lapply(1:nrow(values),
               function(x){
                 row_values <- paste0(
                   dbQuoteIdentifier(conn, as.character(values[x, ])),
                   ", ",
                   collapse = ""
                 )
                 row_values <- substr(row_values, 1, nchar(row_values) - 2)
                 insert_sql <- paste0("INSERT INTO ", toupper(name), " (", fields, ") VALUES (", row_values, ")")
                 dbExecute(conn, insert_sql)
               }
        )

      }


      dbCommit(conn)
    }  ,  error = function(err) {
      dbRollback(conn)
      stop(err)
    })
    invisible(TRUE)
  })

#' @export
setMethod("sqlCreateTable", "Oracle",
          function(con, table, fields, row.names = NA, temporary = FALSE, ...) {
            table <- dbQuoteIdentifier(con, table)

            if (is.data.frame(fields)) {
              fields <- sqlRownamesToColumn(fields, row.names)
              fields <- vapply(fields, function(x) DBI::dbDataType(con, x), character(1))
            }

            field_names <- dbQuoteIdentifier(con, names(fields))
            field_types <- unname(fields)
            fields <- paste0(field_names, " ", field_types)

            SQL(paste0(
              "CREATE ", if (temporary) " GLOBAL TEMPORARY ", "TABLE ", table, " (\n",
              "  ", paste(fields, collapse = ",\n  "), "\n)\n", if (temporary) " ON COMMIT DELETE ROWS"
            ))
          }
)





