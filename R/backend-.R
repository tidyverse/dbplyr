#' @include translate-sql-conditional.R
#' @include translate-sql-window.R
#' @include translate-sql-helpers.R
#' @include translate-sql-scalar.R
#' @include translate-sql-aggregate.R
#' @include translate-sql-string.R
#' @include translate-sql-cut.R
#' @include escape.R
#' @include sql.R
#' @include utils.R
#' @include import-standalone-obj-type.R
#' @include import-standalone-types-check.R
#' @include utils-check.R
#' @include db.R
#' @include db-sql.R
#' @include verb-copy-to.R
#' @include verb-copy-inline.R
NULL

#' Simulate database connections
#'
#' @description
#' These functions generate S3 objects that have been designed to simulate
#' the action of a database connection, without actually having the database
#' available. Obviously, this simulation can only be incomplete, but most
#' importantly it allows us to simulate SQL generation for any database without
#' actually connecting to it.
#'
#' Simulated SQL quotes identifiers with `"x"` (double quotes) by default,
#' `` `x` `` (backticks) for MySQL/MariaDB/SQLite, and `[x]` (square brackets)
#' for SQL Server. Strings are quoted with `'x'`.
#'
#' @keywords internal
#' @export
simulate_dbi <- function(class = character(), ...) {
  structure(
    list(),
    ...,
    class = c(class, "TestConnection", "DBIConnection")
  )
}

dialect_ansi <- function() {
  structure(
    list(
      quote_identifier = function(x) sql_quote(x, '"'),
      has = list(
        window_clause = FALSE,
        table_alias_with_as = TRUE
      )
    
    class = "sql_dialect"
  )
}

#' @export
dbplyr_edition.TestConnection <- function(con) 2L

#' @export
sql_escape_ident.TestConnection <- function(con, x) {
  if (inherits(con, "Microsoft SQL Server")) {
    sql_quote(x, c("[", "]"))
  } else if (
    inherits(con, "MySQLConnection") ||
      inherits(con, "MariaDBConnection") ||
      inherits(con, "SQLiteConnection")
  ) {
    sql_quote(x, "`")
  } else {
    sql_quote(x, '"')
  }
}


#' @export
sql_translation.DBIConnection <- function(con) {
  sql_variant(
    base_scalar,
    base_agg,
    base_win
  )
}

#' @export
#' @rdname sql_variant
#' @format NULL
base_scalar <- sql_translator(
  `+` = function(x, y = NULL) {
    x <- escape_infix_expr(enexpr(x), x, escape_unary_minus = TRUE)
    if (is.null(y)) {
      if (is.numeric(x)) {
        x
      } else {
        sql_glue("{x}")
      }
    } else {
      y <- escape_infix_expr(enexpr(y), y)

      sql_glue("{x} + {y}")
    }
  },
  `*` = sql_infix("*"),
  `/` = sql_infix("/"),
  `%/%` = sql_not_supported("%/%"),
  `%%` = sql_infix("%"),
  `^` = sql_prefix("POWER", 2),
  `-` = function(x, y = NULL) {
    x <- escape_infix_expr(enexpr(x), x, escape_unary_minus = TRUE)
    if (is.null(y)) {
      if (is.numeric(x)) {
        -x
      } else {
        sql_glue("-{x}")
      }
    } else {
      y <- escape_infix_expr(enexpr(y), y)

      sql_glue("{x} - {y}")
    }
  },

  `$` = function(x, name) {
    if (is.ident(x)) {
      sql_glue("{x}.{name}")
    } else {
      eval(bquote(`$`(x, .(substitute(name)))))
    }
  },

  `[[` = function(x, i) {
    # `x` can be a table, column or even an expression (e.g. for json)
    i <- enexpr(i)
    if (is.character(i)) {
      i <- ident(i)
      sql_glue("{x}.{i}")
    } else if (is.numeric(i)) {
      i <- as.integer(i)
      sql_glue("{x}[{i}]")
    } else {
      cli_abort("Can only index with strings and numbers")
    }
  },
  `[` = function(x, i) {
    sql_glue("CASE WHEN ({i}) THEN ({x}) END")
  },

  `!=` = sql_infix("!="),
  `==` = sql_infix("="),
  `<` = sql_infix("<"),
  `<=` = sql_infix("<="),
  `>` = sql_infix(">"),
  `>=` = sql_infix(">="),

  `%in%` = function(x, table) {
    if (is.ident(table)) {
      return(sql_glue("{x} IN {table}"))
    }

    table <- unname(table)
    if (length(table) == 0) {
      sql("FALSE")
    } else {
      sql_glue("{x} IN {table*}")
    }
  },

  `!` = sql_prefix("NOT"),
  `&` = sql_infix("AND"),
  `&&` = sql_infix("AND"),
  `|` = sql_infix("OR"),
  `||` = sql_infix("OR"),
  xor = function(x, y) {
    sql_glue("{x} OR {y} AND NOT ({x} AND {y})")
  },

  # bitwise operators
  # SQL Syntax reference links:
  #   Hive: https://cwiki.apache.org/confluence/display/Hive/LanguageManual+UDF#LanguageManualUDF-ArithmeticOperators
  #   Impala: https://www.cloudera.com/documentation/enterprise/5-9-x/topics/impala_bit_functions.html
  #   PostgreSQL: https://www.postgresql.org/docs/7.4/functions-math.html
  #   MS SQL: https://docs.microsoft.com/en-us/sql/t-sql/language-elements/bitwise-operators-transact-sql?view=sql-server-2017
  #   MySQL https://dev.mysql.com/doc/refman/5.7/en/bit-functions.html
  #   Oracle: https://docs.oracle.com/cd/E19253-01/817-6223/chp-typeopexpr-7/index.html
  #   SQLite: https://www.tutorialspoint.com/sqlite/sqlite_bitwise_operators.htm
  #   Teradata: https://docs.teradata.com/reader/1DcoER_KpnGTfgPinRAFUw/h3CS4MuKL1LCMQmnubeSRQ
  bitwNot = \(x) sql_glue("~({x})"),
  bitwAnd = sql_infix("&"),
  bitwOr = sql_infix("|"),
  bitwXor = sql_infix("^"),
  bitwShiftL = sql_infix("<<"),
  bitwShiftR = sql_infix(">>"),

  abs = sql_prefix("ABS", 1),
  acos = sql_prefix("ACOS", 1),
  asin = sql_prefix("ASIN", 1),
  atan = sql_prefix("ATAN", 1),
  atan2 = sql_prefix("ATAN2", 2),
  ceil = sql_prefix("CEIL", 1),
  ceiling = sql_prefix("CEIL", 1),
  cos = sql_prefix("COS", 1),
  cot = sql_prefix("COT", 1),
  exp = sql_prefix("EXP", 1),
  floor = sql_prefix("FLOOR", 1),
  log = function(x, base = exp(1)) {
    if (isTRUE(all.equal(base, exp(1)))) {
      sql_glue("LN({x})")
    } else {
      sql_glue("LOG({base}, {x})")
    }
  },
  log10 = sql_prefix("LOG10", 1),
  round = function(x, digits = 0L) {
    digits <- as.integer(digits)
    sql_glue("ROUND({x}, {digits})")
  },
  sign = sql_prefix("SIGN", 1),
  sin = sql_prefix("SIN", 1),
  sqrt = sql_prefix("SQRT", 1),
  tan = sql_prefix("TAN", 1),
  # cosh, sinh, coth and tanh calculations are based on this article
  # https://en.wikipedia.org/wiki/Hyperbolic_function
  cosh = \(x) sql_glue("({sql_exp(1, x)} + {sql_exp(-1, x)}) / 2"),
  sinh = \(x) sql_glue("({sql_exp(1, x)} - {sql_exp(-1, x)}) / 2"),
  tanh = \(x) sql_glue("({sql_exp(2, x)} - 1) / ({sql_exp(2, x)} + 1)"),
  coth = \(x) sql_glue("({sql_exp(2, x)} + 1) / ({sql_exp(2, x)} - 1)"),

  `if` = function(cond, if_true, if_false = NULL) {
    sql_if(enquo(cond), enquo(if_true), enquo(if_false))
  },
  if_else = function(condition, true, false, missing = NULL) {
    sql_if(
      enquo(condition),
      enquo(true),
      enquo(false),
      enquo(missing)
    )
  },
  ifelse = \(test, yes, no) sql_if(enquo(test), enquo(yes), enquo(no)),

  switch = \(x, ...) sql_switch(x, ...),
  case_when = function(..., .default = NULL, .ptype = NULL, .size = NULL) {
    sql_case_when(..., .default = .default, .ptype = .ptype, .size = .size)
  },
  case_match = sql_case_match,

  `(` = function(x) {
    sql_glue("({x})")
  },
  `{` = function(x) {
    sql_glue("({x})")
  },
  desc = function(x) {
    sql_glue("{x} DESC")
  },

  is.null = sql_is_null,
  is.na = sql_is_null,
  na_if = sql_prefix("NULLIF", 2),
  coalesce = sql_prefix("COALESCE"),

  as.numeric = sql_cast("NUMERIC"),
  as.double = sql_cast("NUMERIC"),
  as.integer = sql_cast("INTEGER"),
  as.character = sql_cast("TEXT"),
  as.logical = sql_cast("BOOLEAN"),
  as.Date = sql_cast("DATE"),
  as.POSIXct = sql_cast("TIMESTAMP"),
  # MS SQL - https://docs.microsoft.com/en-us/sql/t-sql/data-types/int-bigint-smallint-and-tinyint-transact-sql
  # Hive - https://cwiki.apache.org/confluence/display/Hive/LanguageManual+Types#LanguageManualTypes-IntegralTypes(TINYINT,SMALLINT,INT/INTEGER,BIGINT)
  # Postgres - https://www.postgresql.org/docs/8.4/static/datatype-numeric.html
  # Impala - https://impala.apache.org/docs/build/html/topics/impala_bigint.html
  as.integer64 = sql_cast("BIGINT"),
  as.blob = sql_cast("BLOB"),
  as = function(x, type) {
    check_string(type)
    sql_glue("CAST({x} AS {.sql type})")
  },

  c = function(...) {
    c(...)
  },
  `:` = \(from, to) from:to,

  between = function(x, left, right) {
    sql_glue("{x} BETWEEN {left} AND {right}")
  },

  pmin = sql_aggregate_n("LEAST", "pmin"),
  pmax = sql_aggregate_n("GREATEST", "pmax"),

  # Copied onLoad
  `%>%` = NULL,

  # lubridate ---------------------------------------------------------------
  # https://en.wikibooks.org/wiki/SQL_Dialects_Reference/Functions_and_expressions/Date_and_time_functions
  as_date = sql_cast("DATE"),
  as_datetime = sql_cast("TIMESTAMP"),

  today = \() sql("CURRENT_DATE"),
  now = \() sql("CURRENT_TIMESTAMP"),

  # https://modern-sql.com/feature/extract
  year = \(x) sql_glue("EXTRACT(year FROM {x})"),
  month = \(x) sql_glue("EXTRACT(month FROM {x})"),
  day = \(x) sql_glue("EXTRACT(day FROM {x})"),
  mday = \(x) sql_glue("EXTRACT(day FROM {x})"),
  yday = sql_not_supported("yday"),
  qday = sql_not_supported("qday"),
  wday = sql_not_supported("wday"),
  hour = \(x) sql_glue("EXTRACT(hour FROM {x})"),
  minute = \(x) sql_glue("EXTRACT(minute FROM {x})"),
  second = \(x) sql_glue("EXTRACT(second FROM {x})"),

  # String functions ------------------------------------------------------
  # SQL Syntax reference links:
  #   MySQL https://dev.mysql.com/doc/refman/5.7/en/string-functions.html
  #   Hive: https://cwiki.apache.org/confluence/display/Hive/LanguageManual+UDF#LanguageManualUDF-StringFunctions
  #   Impala: https://www.cloudera.com/documentation/enterprise/5-9-x/topics/impala_string_functions.html
  #   PostgreSQL: https://www.postgresql.org/docs/9.1/static/functions-string.html
  #   MS SQL: https://docs.microsoft.com/en-us/sql/t-sql/functions/string-functions-transact-sql
  #   Oracle: https://docs.oracle.com/database/121/SQLRF/functions002.htm#SQLRF51180

  # base R
  nchar = sql_prefix("LENGTH", 1),
  nzchar = function(x, keepNA = FALSE) {
    check_bool(keepNA)
    if (keepNA) {
      exp <- expr(!!x != "")
      translate_sql(!!exp, con = sql_current_con())
    } else {
      exp <- expr((is.na(!!x) | !!x != ""))
      translate_sql(!!exp, con = sql_current_con())
    }
  },
  tolower = sql_prefix("LOWER", 1),
  toupper = sql_prefix("UPPER", 1),
  trimws = \(x, which = "both") sql_str_trim(x, side = which),
  paste = sql_paste(" "),
  paste0 = sql_paste(""),
  substr = sql_substr("SUBSTR"),
  substring = sql_substr("SUBSTR"),
  cut = sql_cut,
  runif = function(n = n(), min = 0, max = 1) {
    sql_runif("RANDOM()", n = {{ n }}, min = min, max = max)
  },

  # stringr functions
  str_length = sql_prefix("LENGTH", 1),
  str_to_lower = sql_prefix("LOWER", 1),
  str_to_upper = sql_prefix("UPPER", 1),
  str_to_title = sql_prefix("INITCAP", 1),
  str_trim = sql_str_trim,
  str_c = sql_paste(""),
  str_sub = sql_str_sub("SUBSTR"),
  # https://docs.getdbt.com/sql-reference/like is typically case sensitive (#1490)
  str_like = function(string, pattern, ignore_case = deprecated()) {
    ignore_case <- deprecate_ignore_case(ignore_case)

    if (ignore_case) {
      cli_abort(c(
        "Backend does not support case insensitive {.fn str_like}.",
        i = "Use {.fn tolower} on both arguments to achieve a case insensitive match."
      ))
    } else {
      sql_glue("{string} LIKE {pattern}")
    }
  },

  str_ilike = sql_not_supported("str_ilike"),
  str_conv = sql_not_supported("str_conv"),
  str_count = sql_not_supported("str_count"),

  fixed = function(pattern, ignore_case = FALSE) {
    check_unsupported_arg(ignore_case, allowed = FALSE)
    pattern
  },
  str_detect = function(string, pattern, negate = FALSE) {
    sql_str_pattern_switch(
      string = string,
      pattern = {{ pattern }},
      negate = negate,
      f_fixed = sql_str_detect_fixed_instr("detect")
    )
  },
  str_dup = sql_not_supported("str_dup"),
  str_ends = function(string, pattern, negate = FALSE) {
    sql_str_pattern_switch(
      string = string,
      pattern = {{ pattern }},
      negate = negate,
      f_fixed = sql_str_detect_fixed_instr("end")
    )
  },
  str_extract = sql_not_supported("str_extract"),
  str_extract_all = sql_not_supported("str_extract_all"),
  str_flatten = sql_not_supported("str_flatten"),
  str_glue = sql_not_supported("str_glue"),
  str_glue_data = sql_not_supported("str_glue_data"),
  str_interp = sql_not_supported("str_interp"),
  str_locate = sql_not_supported("str_locate"),
  str_locate_all = sql_not_supported("str_locate_all"),
  str_match = sql_not_supported("str_match"),
  str_match_all = sql_not_supported("str_match_all"),
  str_order = sql_not_supported("str_order"),
  str_pad = sql_not_supported("str_pad"),
  str_remove = sql_not_supported("str_remove"),
  str_remove_all = sql_not_supported("str_remove_all"),
  str_replace = sql_not_supported("str_replace"),
  str_replace_all = sql_not_supported("str_replace_all"),
  str_replace_na = sql_not_supported("str_replace_na"),
  str_sort = sql_not_supported("str_sort"),
  str_split = sql_not_supported("str_split"),
  str_split_fixed = sql_not_supported("str_split_fixed"),
  str_squish = sql_not_supported("str_squish"),
  str_starts = function(string, pattern, negate = FALSE) {
    sql_str_pattern_switch(
      string = string,
      pattern = {{ pattern }},
      negate = negate,
      f_fixed = sql_str_detect_fixed_instr("start")
    )
  },
  str_subset = sql_not_supported("str_subset"),
  str_trunc = sql_not_supported("str_trunc"),
  str_view = sql_not_supported("str_view"),
  str_view_all = sql_not_supported("str_view_all"),
  str_which = sql_not_supported("str_which"),
  str_wrap = sql_not_supported("str_wrap")
)

sql_exp <- function(a, x) {
  a <- as.integer(a)
  if (identical(a, 1L)) {
    sql_glue("EXP({x})")
  } else if (identical(a, -1L)) {
    sql_glue("EXP(-({x}))")
  } else {
    sql_glue("EXP({a} * ({x}))")
  }
}

#' @export
#' @rdname sql_variant
#' @format NULL
base_agg <- sql_translator(
  # SQL-92 aggregates
  # http://db.apache.org/derby/docs/10.7/ref/rrefsqlj33923.html
  n = \() sql("COUNT(*)"),
  mean = sql_aggregate("AVG", "mean"),
  sum = sql_aggregate("SUM"),
  min = sql_aggregate("MIN"),
  max = sql_aggregate("MAX"),

  # https://blog.jooq.org/a-true-sql-gem-you-didnt-know-yet-the-every-aggregate-function/#comment-344695
  all = sql_aggregate("MIN"),
  any = sql_aggregate("MAX"),

  sd = sql_not_supported("sd"),
  var = sql_not_supported("var"),
  cor = sql_not_supported("cor"),
  cov = sql_not_supported("cov"),

  # Ordered set functions
  quantile = sql_quantile("PERCENTILE_CONT", "ordered"),
  median = sql_median("PERCENTILE_CONT", "ordered"),

  # `first()`, `last()` and `nth()` don't work as aggregate functions in SQL
  # b/c grouping takes places before sorting => it is undefined which value comes
  # first/last. See
  # https://stackoverflow.com/a/46805009/7529482
  # first = sql_prefix("FIRST_VALUE", 1),
  # last = sql_prefix("LAST_VALUE", 1),
  # nth = sql_prefix("NTH_VALUE", 2),

  n_distinct = function(x, na.rm = FALSE) {
    sql_check_na_rm(na.rm)
    sql_glue("COUNT(DISTINCT {x})")
  }
)

#' @export
#' @rdname sql_variant
#' @format NULL
base_win <- sql_translator(
  # rank functions have a single order argument that overrides the default
  row_number = win_rank("ROW_NUMBER"),
  min_rank = win_rank("RANK"),
  rank = win_rank("RANK"),
  dense_rank = win_rank("DENSE_RANK"),
  percent_rank = win_rank("PERCENT_RANK"),
  cume_dist = win_rank("CUME_DIST"),
  ntile = function(x, n) {
    n <- as.integer(n)
    win_over(
      sql_glue("NTILE({n})"),
      win_current_group(),
      x %||% win_current_order()
    )
  },

  # Variants that take more arguments
  first = function(x, order_by = NULL, na_rm = FALSE) {
    sql_nth(
      x = x,
      n = 1L,
      order_by = order_by,
      na_rm = na_rm,
      ignore_nulls = "inside"
    )
  },
  last = function(x, order_by = NULL, na_rm = FALSE) {
    sql_nth(
      x = x,
      n = Inf,
      order_by = order_by,
      na_rm = na_rm,
      ignore_nulls = "inside"
    )
  },
  nth = function(x, n, order_by = NULL, na_rm = FALSE) {
    sql_nth(
      x = x,
      n = n,
      order_by = order_by,
      na_rm = na_rm,
      ignore_nulls = "inside"
    )
  },

  lead = function(x, n = 1L, default = NA, order_by = NULL) {
    n <- as.integer(n)
    win_over(
      sql_glue("LEAD({x}, {n}, {default})"),
      win_current_group(),
      order_by %||% win_current_order(),
      win_current_frame()
    )
  },
  lag = function(x, n = 1L, default = NA, order_by = NULL) {
    n <- as.integer(n)
    win_over(
      sql_glue("LAG({x}, {n}, {default})"),
      win_current_group(),
      order_by %||% win_current_order(),
      win_current_frame()
    )
  },
  # Recycled aggregate fuctions take single argument, don't need order and
  # include entire partition in frame.
  mean = win_aggregate("AVG"),
  sum = win_aggregate("SUM"),
  min = win_aggregate("MIN"),
  max = win_aggregate("MAX"),

  all = win_aggregate("MIN"),
  any = win_aggregate("MAX"),

  sd = sql_not_supported("sd"),
  var = sql_not_supported("var"),
  cor = sql_not_supported("cor"),
  cov = sql_not_supported("cov"),

  # Ordered set functions
  quantile = sql_quantile("PERCENTILE_CONT", "ordered", window = TRUE),
  median = sql_median("PERCENTILE_CONT", "ordered", window = TRUE),

  # Counts
  n = function() {
    frame <- win_current_frame()
    win_over(
      sql("COUNT(*)"),
      partition = win_current_group(),
      order = if (!is.null(frame)) win_current_order(),
      frame = frame
    )
  },
  n_distinct = function(x, na.rm = FALSE) {
    sql_check_na_rm(na.rm)
    win_over(
      sql_glue("COUNT(DISTINCT {x})"),
      win_current_group()
    )
  },

  # Cumulative function are like recycled aggregates except that R names
  # have cum prefix, order_by is inherited and frame goes from -Inf to 0.
  cummean = win_cumulative("AVG"),
  cumsum = win_cumulative("SUM"),
  cummin = win_cumulative("MIN"),
  cummax = win_cumulative("MAX"),

  # Manually override other parameters --------------------------------------
  order_by = function(order_by, expr) {
    old <- set_win_current_order(order_by)
    on.exit(set_win_current_order(old))

    expr
  }
)

#' @export
#' @rdname sql_variant
#' @format NULL
base_no_win <- sql_translator(
  row_number = win_absent("row_number"),
  min_rank = win_absent("min_rank"),
  rank = win_absent("rank"),
  dense_rank = win_absent("dense_rank"),
  percent_rank = win_absent("percent_rank"),
  cume_dist = win_absent("cume_dist"),
  ntile = win_absent("ntile"),
  mean = win_absent("mean"),
  sd = win_absent("sd"),
  var = win_absent("var"),
  cov = win_absent("cov"),
  cor = win_absent("cor"),
  sum = win_absent("sum"),
  min = win_absent("min"),
  max = win_absent("max"),
  all = win_absent("all"),
  any = win_absent("any"),
  median = win_absent("median"),
  quantile = win_absent("quantile"),
  n = win_absent("n"),
  n_distinct = win_absent("n_distinct"),
  cummean = win_absent("cummean"),
  cumsum = win_absent("cumsum"),
  cummin = win_absent("cummin"),
  cummax = win_absent("cummax"),
  nth = win_absent("nth"),
  first = win_absent("first"),
  last = win_absent("last"),
  lead = win_absent("lead"),
  lag = win_absent("lad"),
  order_by = win_absent("order_by"),
  str_flatten = win_absent("str_flatten"),
  count = win_absent("count")
)
