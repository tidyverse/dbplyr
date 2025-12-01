#' SQL helpers for string functions
#'
#' @description
#' These functions help you create custom string SQL translations when
#' implementing a new backend. They are typically used within [sql_translator()]
#' to define how R string functions should be translated to SQL.
#'
#' * `sql_substr()` creates a SQL substring function translator that converts
#'   R's `substr(x, start, stop)` to SQL's `SUBSTR(x, start, length)`.
#' * `sql_str_sub()` creates a SQL substring function translator that handles
#'   stringr's `str_sub()` with support for negative indices.
#' * `sql_paste()` creates a SQL paste function using `CONCAT_WS()` or similar.
#' * `sql_paste_infix()` creates a SQL paste function using an infix operator
#'   like `||`.
#'
#' @param f The name of the SQL function as a string.
#' @param subset_f The name of the SQL substring function.
#' @param length_f The name of the SQL string length function.
#' @param optional_length Whether the length argument is optional in the SQL
#'   substring function.
#' @param default_sep The default separator for paste operations.
#' @param op The SQL operator to use for infix paste operations.
#' @param cast A function to cast values to strings.
#'
#' @family SQL translation helpers
#' @name sql_translation_string
NULL

#' @export
#' @rdname sql_translation_string
sql_substr <- function(f = "SUBSTR") {
  # R prefers to specify start / stop or start / end
  # databases usually specify start / length
  # https://www.postgresql.org/docs/current/functions-string.html
  function(x, start, stop) {
    start <- max(cast_number_whole(start), 1L)
    stop <- max(cast_number_whole(stop), 1L)
    length <- max(stop - start + 1L, 0L)

    sql_call2(f, x, start, length)
  }
}

cast_number_whole <- function(x, arg = caller_arg(x), call = caller_env()) {
  check_number_whole(x, arg = arg, call = call)
  vctrs::vec_cast(x, integer(), x_arg = arg)
}

# str_sub(x, start, end) - start and end can be negative
# SUBSTR(string, start, length) - start can be negative

#' @export
#' @rdname sql_translation_string
sql_str_sub <- function(
  subset_f = "SUBSTR",
  length_f = "LENGTH",
  optional_length = TRUE
) {
  function(string, start = 1L, end = -1L) {
    start <- cast_number_whole(start)
    end <- cast_number_whole(end)

    start_sql <- start_pos(string, start, length_f)

    if (optional_length && end == -1L) {
      sql_call2(subset_f, string, start_sql)
    } else {
      if (end == 0L) {
        length_sql <- 0L
      } else if (start > 0 && end < 0) {
        n <- start - end - 2L
        if (n == 0) {
          length_sql <- sql_call2(length_f, string)
        } else {
          length_sql <- sql_expr(!!sql_call2(length_f, string) - !!n)
        }
      } else {
        length_sql <- pmax(end - start + 1L, 0L)
      }
      sql_call2(subset_f, string, start_sql, length_sql)
    }
  }
}

start_pos <- function(string, start, length_f) {
  if (start == -1) {
    sql_call2(length_f, string)
  } else if (start < 0) {
    sql_expr(!!sql_call2(length_f, string) - !!abs(start + 1L))
  } else {
    start
  }
}

sql_str_trim <- function(string, side = c("both", "left", "right")) {
  side <- match.arg(side)
  switch(
    side,
    left = sql_expr(ltrim(!!string)),
    right = sql_expr(rtrim(!!string)),
    both = sql_expr(ltrim(rtrim(!!string))),
  )
}


sql_str_pattern_switch <- function(
  string,
  pattern,
  negate = FALSE,
  f_fixed = NULL,
  f_regex = NULL,
  error_call = caller_env()
) {
  pattern_quo <- enquo(pattern)
  is_fixed <- quo_is_call(pattern_quo, "fixed") ||
    inherits(pattern, "stringr_fixed")

  if (is_fixed) {
    f_fixed(string, pattern, negate)
  } else {
    if (is_null(f_regex)) {
      cli_abort(
        "Only fixed patterns are supported on database backends.",
        call = error_call
      )
    } else {
      f_regex(string, pattern, negate)
    }
  }
}

# INSTR
# * SQLite https://www.sqlitetutorial.net/sqlite-functions/sqlite-instr/
# * MySQL https://dev.mysql.com/doc/refman/8.0/en/string-functions.html#function_instr
# * Oracle https://docs.oracle.com/en/database/oracle/oracle-database/19/sqlrf/INSTR.html#GUID-47E3A7C4-ED72-458D-A1FA-25A9AD3BE113
# * Teradata https://docs.teradata.com/r/Teradata-VantageTM-SQL-Functions-Expressions-and-Predicates/March-2019/String-Operators-and-Functions/INSTR
# * Access https://support.microsoft.com/de-de/office/instr-funktion-85d3392c-3b1c-4232-bb18-77cd0cb8a55b
# * Hana https://help.sap.com/docs/SAP_HANA_PLATFORM/e8e6c8142e60469bb401de5fdb6f7c00/f5a9ca3718354a499a98ba61ae3da170.html
# * Hive https://www.revisitclass.com/hadoop/instr-function-in-hive-with-examples/
# * Impala https://impala.apache.org/docs/build/html/topics/impala_string_functions.html#string_functions__instr
# POSITION
# * Snowflake https://docs.snowflake.com/en/sql-reference/functions/position
sql_str_detect_fixed_instr <- function(type = c("detect", "start", "end")) {
  type <- arg_match(type)

  function(string, pattern, negate = FALSE) {
    con <- sql_current_con()
    pattern <- unclass(pattern)
    index_sql <- glue_sql2(con, "INSTR({.val string}, {.val pattern})")

    if (negate) {
      switch(
        type,
        detect = translate_sql(!!index_sql == 0L, con = con),
        start = translate_sql(!!index_sql != 1L, con = con),
        end = translate_sql(
          !!index_sql != nchar(!!string) - nchar(!!pattern) + 1L,
          con = con
        )
      )
    } else {
      switch(
        type,
        detect = translate_sql(!!index_sql > 0L, con = con),
        start = translate_sql(!!index_sql == 1L, con = con),
        end = translate_sql(
          !!index_sql == nchar(!!string) - nchar(!!pattern) + 1L,
          con = con
        )
      )
    }
  }
}

sql_str_detect_fixed_position <- function(type = c("detect", "start", "end")) {
  type <- arg_match(type)

  function(string, pattern, negate = FALSE) {
    con <- sql_current_con()
    pattern <- unclass(pattern)
    index_sql <- glue_sql2(con, "POSITION({.val pattern} in {.val string})")

    if (negate) {
      switch(
        type,
        detect = translate_sql(!!index_sql == 0L, con = con),
        start = translate_sql(!!index_sql != 1L, con = con),
        end = translate_sql(
          !!index_sql != nchar(!!string) - nchar(!!pattern) + 1L,
          con = con
        )
      )
    } else {
      switch(
        type,
        detect = translate_sql(!!index_sql > 0L, con = con),
        start = translate_sql(!!index_sql == 1L, con = con),
        end = translate_sql(
          !!index_sql == nchar(!!string) - nchar(!!pattern) + 1L,
          con = con
        )
      )
    }
  }
}

utils::globalVariables(c("ltrim", "rtrim"))
