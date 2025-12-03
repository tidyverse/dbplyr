deparse_all <- function(x) {
  x <- purrr::map_if(x, is_formula, f_rhs)
  purrr::map_chr(x, expr_text, width = 500L)
}

#' Provides comma-separated string out of the parameters
#' @export
#' @keywords internal
named_commas <- function(x) {
  if (is.list(x)) {
    x <- purrr::map_chr(x, as_label)
  } else {
    x <- as.character(x)
  }

  nms <- names2(x)
  out <- ifelse(nms == "", x, paste0(nms, " = ", x))
  paste0(out, collapse = ", ")
}

commas <- function(...) paste0(..., collapse = ", ")

unique_table_name <- function(prefix = "") {
  vals <- c(letters, LETTERS, 0:9)
  name <- paste0(sample(vals, 10, replace = TRUE), collapse = "")
  paste0(prefix, "dbplyr_tmp_", name)
}

unique_subquery_name <- function() {
  # Needs to use option so can reset at the start of each query
  i <- getOption("dbplyr_subquery_name", 0) + 1
  options(dbplyr_subquery_name = i)
  sprintf("q%02i", i)
}
unique_column_name <- function() {
  # Needs to use option so can reset at the start of each query
  i <- getOption("dbplyr_column_name", 0) + 1
  options(dbplyr_column_name = i)
  sprintf("col%02i", i)
}
unique_subquery_name_reset <- function() {
  options(dbplyr_subquery_name = 0)
}
unique_column_name_reset <- function() {
  options(dbplyr_column_name = 0)
}

succeeds <- function(x, quiet = FALSE) {
  tryCatch(
    {
      x
      TRUE
    },
    error = function(e) {
      if (!quiet) {
        message("Error: ", e$message)
      } # nocov
      FALSE
    }
  )
}

c_character <- function(...) {
  x <- c(...)
  if (length(x) == 0) {
    return(character())
  }

  if (!is.character(x)) {
    cli_abort("Character input expected")
  }

  x
}

cat_line <- function(...) cat(paste0(..., "\n"), sep = "")

# nocov start
res_warn_incomplete <- function(res, hint = "n = -1") {
  if (dbHasCompleted(res)) {
    return()
  }

  rows <- big_mark(dbGetRowCount(res))
  cli::cli_warn(
    "Only first {rows} results retrieved. Use {hint} to retrieve all."
  )
}

add_temporary_prefix <- function(con, table, temporary = TRUE) {
  check_bool(temporary)
  check_table_path(table)

  if (!temporary) {
    return(table)
  }

  pieces <- table_path_components(table, con)[[1]]
  table_name <- pieces[length(pieces)]

  if (substr(table_name, 1, 1) != "#") {
    new_name <- paste0("#", table_name)
    cli::cli_inform(
      paste0("Created a temporary table named ", new_name),
      class = c("dbplyr_message_temp_table", "dbplyr_message")
    )
    pieces[[length(pieces)]] <- new_name
    table <- make_table_path(pieces, con)
  }

  table
}
# nocov end

# Helper for testing
local_methods <- function(..., .frame = caller_env()) {
  local_bindings(..., .env = global_env(), .frame = .frame)
}

local_db_table <- function(
  con,
  value,
  name,
  ...,
  temporary = TRUE,
  envir = parent.frame()
) {
  if (inherits(con, "Microsoft SQL Server") && temporary) {
    name <- paste0("#", name)
  }

  withr::defer(DBI::dbRemoveTable(con, name), envir = envir)
  copy_to(con, value, name, temporary = temporary, ...)
  tbl(con, name)
}

local_sqlite_connection <- function(envir = parent.frame()) {
  withr::local_db_connection(
    DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
    .local_envir = envir
  )
}

local_memdb_frame <- function(name, ..., frame = parent.frame()) {
  df <- tibble::tibble(...)

  withr::defer(DBI::dbRemoveTable(src_memdb()$con, name), envir = frame)
  copy_to(src_memdb(), df, name, temporary = TRUE)
}
