#' @include translate-sql-window.r
#' @include translate-sql-helpers.r
#' @include sql-escape.r
NULL


sql_if <- function(cond, if_true, if_false = NULL) {
  build_sql(
    "CASE WHEN (", cond, ")", " THEN (", if_true, ")",
    if (!is.null(if_false))
      build_sql(" WHEN NOT(", cond, ") THEN (", if_false, ")"),
    " END"
  )
}

sql_null <- function(x) {
  if (old_qq()) {
    sql_expr(((!!x) %is% NULL))
  } else {
    sql_expr((((!!x)) %is% NULL))
  }
}


#' @export
#' @rdname sql_variant
#' @format NULL
base_scalar <- sql_translator(
  `+`    = sql_infix("+"),
  `*`    = sql_infix("*"),
  `/`    = sql_infix("/"),
  `%%`   = sql_infix("%"),
  `^`    = sql_prefix("power", 2),
  `-`    = function(x, y = NULL) {
    if (is.null(y)) {
      if (is.numeric(x)) {
        -x
      } else {
        build_sql(sql("-"), x)
      }
    } else {
      build_sql(x, sql(" - "), y)
    }
  },

  `!=`    = sql_infix("!="),
  `==`    = sql_infix("="),
  `<`     = sql_infix("<"),
  `<=`    = sql_infix("<="),
  `>`     = sql_infix(">"),
  `>=`    = sql_infix(">="),

  `%in%` = function(x, table) {
    if (is.sql(table) || length(table) > 1) {
      build_sql(x, " IN ", table)
    } else {
      build_sql(x, " IN (", table, ")")
    }
  },

  `!`     = sql_prefix("not"),
  `&`     = sql_infix("and"),
  `&&`    = sql_infix("and"),
  `|`     = sql_infix("or"),
  `||`    = sql_infix("or"),
  xor     = function(x, y) {
    sql(sprintf("%1$s OR %2$s AND NOT (%1$s AND %2$s)", escape(x), escape(y)))
  },

  abs     = sql_prefix("abs", 1),
  acos    = sql_prefix("acos", 1),
  acosh   = sql_prefix("acosh", 1),
  asin    = sql_prefix("asin", 1),
  asinh   = sql_prefix("asinh", 1),
  atan    = sql_prefix("atan", 1),
  atan2   = sql_prefix("atan2", 2),
  atanh   = sql_prefix("atanh", 1),
  ceil    = sql_prefix("ceil", 1),
  ceiling = sql_prefix("ceil", 1),
  cos     = sql_prefix("cos", 1),
  cosh    = sql_prefix("cosh", 1),
  cot     = sql_prefix("cot", 1),
  coth    = sql_prefix("coth", 1),
  exp     = sql_prefix("exp", 1),
  floor   = sql_prefix("floor", 1),
  log     = function(x, base = exp(1)) {
    if (isTRUE(all.equal(base, exp(1)))) {
      sql_expr(ln(!!x))
    } else {
      sql_expr(log(!!base, !!x))
    }
  },
  log10   = sql_prefix("log10", 1),
  round   = sql_prefix("round", 2),
  sign    = sql_prefix("sign", 1),
  sin     = sql_prefix("sin", 1),
  sinh    = sql_prefix("sinh", 1),
  sqrt    = sql_prefix("sqrt", 1),
  tan     = sql_prefix("tan", 1),
  tanh     = sql_prefix("tanh", 1),

  tolower = sql_prefix("lower", 1),
  toupper = sql_prefix("upper", 1),
  trimws = sql_prefix("trim", 1),
  nchar   = sql_prefix("length", 1),
  substr = function(x, start, stop) {
    start <- as.integer(start)
    length <- pmax(as.integer(stop) - start + 1L, 0L)

    build_sql(sql("substr"), list(x, start, length))
  },

  `if` = sql_if,
  if_else = function(condition, true, false) sql_if(condition, true, false),
  ifelse = function(test, yes, no) sql_if(test, yes, no),

  case_when = function(...) sql_case_when(...),

  sql = function(...) sql(...),
  `(` = function(x) {
    build_sql("(", x, ")")
  },
  `{` = function(x) {
    build_sql("(", x, ")")
  },
  desc = function(x) {
    build_sql(x, sql(" DESC"))
  },

  is.null = sql_null,
  is.na = sql_null,
  na_if = sql_prefix("NULL_IF", 2),
  coalesce = sql_prefix("coalesce"),

  as.numeric = sql_cast("NUMERIC"),
  as.double = sql_cast("NUMERIC"),
  as.integer = sql_cast("INTEGER"),
  as.character = sql_cast("TEXT"),

  c = function(...) c(...),
  `:` = function(from, to) from:to,

  between = function(x, left, right) {
    build_sql(x, " BETWEEN ", left, " AND ", right)
  },

  pmin = sql_prefix("min"),
  pmax = sql_prefix("max"),

  `%>%` = `%>%`,

  # stringr functions

  # SQL Syntax reference links:
  #   MySQL https://dev.mysql.com/doc/refman/5.7/en/string-functions.html
  #   Hive: https://cwiki.apache.org/confluence/display/Hive/LanguageManual+UDF#LanguageManualUDF-StringFunctions
  #   Impala: https://www.cloudera.com/documentation/enterprise/5-9-x/topics/impala_string_functions.html
  #   PostgreSQL: https://www.postgresql.org/docs/9.1/static/functions-string.html
  #   MS SQL: https://docs.microsoft.com/en-us/sql/t-sql/functions/string-functions-transact-sql
  #   Oracle: https://docs.oracle.com/middleware/bidv1221/desktop/BIDVD/GUID-BBA975C7-B2C5-4C94-A007-28775680F6A5.htm#BILUG685
  str_length      = sql_prefix("LENGTH"),
  str_to_upper    = sql_prefix("UPPER"),
  str_to_lower    = sql_prefix("LOWER"),
  str_replace_all = function(string, pattern, replacement){
                      build_sql(
                        "REPLACE(", string, ", ", pattern, ", ", replacement, ")"
                      )},
  str_detect      = function(string, pattern){
                      build_sql(
                        "INSTR(", pattern, ", ", string, ") > 0"
                      )},
  str_trim        = function(string, side = "both"){
                      build_sql(
                        sql(ifelse(side == "both" | side == "left", "LTRIM(", "(")),
                        sql(ifelse(side == "both" | side == "right", "RTRIM(", "(")),
                        string
                        ,"))"
                      )}
)

base_symbols <- sql_translator(
  pi = sql("PI()"),
  `*` = sql("*"),
  `NULL` = sql("NULL")
)

#' @export
#' @rdname sql_variant
#' @format NULL
base_agg <- sql_translator(
  # SQL-92 aggregates
  # http://db.apache.org/derby/docs/10.7/ref/rrefsqlj33923.html
  n          = function() sql("COUNT()"),
  mean       = sql_aggregate("avg"),
  var        = sql_aggregate("variance"),
  sum        = sql_aggregate("sum"),
  min        = sql_aggregate("min"),
  max        = sql_aggregate("max"),
  n_distinct = function(...) {
    vars <- sql_vector(list(...), parens = FALSE, collapse = ", ")
    build_sql("COUNT(DISTINCT ", vars, ")")
  }
)

#' @export
#' @rdname sql_variant
#' @format NULL
base_win <- sql_translator(
  # rank functions have a single order argument that overrides the default
  row_number   = win_rank("row_number"),
  min_rank     = win_rank("rank"),
  rank         = win_rank("rank"),
  dense_rank   = win_rank("dense_rank"),
  percent_rank = win_rank("percent_rank"),
  cume_dist    = win_rank("cume_dist"),
  ntile        = function(order_by, n) {
    win_over(
      build_sql("NTILE", list(as.integer(n))),
      win_current_group(),
      order_by %||% win_current_order()
    )
  },

  # Variants that take more arguments
  first = function(x, order_by = NULL) {
    win_over(
      build_sql("first_value", list(x)),
      win_current_group(),
      order_by %||% win_current_order()
    )
  },
  last = function(x, order_by = NULL) {
    win_over(
      build_sql("last_value", list(x)),
      win_current_group(),
      order_by %||% win_current_order()
    )
  },
  nth = function(x, n, order_by = NULL) {
    win_over(
      build_sql("nth_value", list(x, as.integer(n))),
      win_current_group(),
      order_by %||% win_current_order()
    )
  },

  lead = function(x, n = 1L, default = NA, order_by = NULL) {
    win_over(
      build_sql("LEAD", list(x, n, default)),
      win_current_group(),
      order_by %||% win_current_order()
    )
  },
  lag = function(x, n = 1L, default = NA, order_by = NULL) {
    win_over(
      build_sql("LAG", list(x, as.integer(n), default)),
      win_current_group(),
      order_by %||% win_current_order()
    )
  },

  # Recycled aggregate fuctions take single argument, don't need order and
  # include entire partition in frame.
  mean  = win_aggregate("avg"),
  var   = win_aggregate("variance"),
  sum   = win_aggregate("sum"),
  min   = win_aggregate("min"),
  max   = win_aggregate("max"),
  n     = function() {
    win_over(sql("COUNT(*)"), win_current_group())
  },
  n_distinct = function(...) {
    vars <- sql_vector(list(...), parens = FALSE, collapse = ", ")
    win_over(build_sql("COUNT(DISTINCT ", vars, ")"), win_current_group())
  },

  # Cumulative function are like recycled aggregates except that R names
  # have cum prefix, order_by is inherited and frame goes from -Inf to 0.
  cummean = win_cumulative("mean"),
  cumsum  = win_cumulative("sum"),
  cummin  = win_cumulative("min"),
  cummax  = win_cumulative("max"),

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
  row_number   = win_absent("row_number"),
  min_rank     = win_absent("rank"),
  rank         = win_absent("rank"),
  dense_rank   = win_absent("dense_rank"),
  percent_rank = win_absent("percent_rank"),
  cume_dist    = win_absent("cume_dist"),
  ntile        = win_absent("ntile"),
  mean         = win_absent("avg"),
  sd           = win_absent("sd"),
  var          = win_absent("var"),
  cov          = win_absent("cov"),
  cor          = win_absent("cor"),
  sum          = win_absent("sum"),
  min          = win_absent("min"),
  max          = win_absent("max"),
  n            = win_absent("n"),
  n_distinct   = win_absent("n_distinct"),
  cummean      = win_absent("mean"),
  cumsum       = win_absent("sum"),
  cummin       = win_absent("min"),
  cummax       = win_absent("max"),
  nth          = win_absent("nth_value"),
  first        = win_absent("first_value"),
  last         = win_absent("last_value"),
  lead         = win_absent("lead"),
  lag          = win_absent("lag"),
  order_by     = win_absent("order_by"),
  str_flatten = win_absent("str_flatten"),
  count        = win_absent("count")
)



# case_when ---------------------------------------------------------------

sql_case_when <- function(...) {
  # TODO: switch to dplyr::case_when_prepare when available

  formulas <- dots_list(...)
  n <- length(formulas)

  if (n == 0) {
    abort("No cases provided")
  }

  query <- vector("list", n)
  value <- vector("list", n)

  for (i in seq_len(n)) {
    f <- formulas[[i]]

    env <- environment(f)
    query[[i]] <- escape(eval_bare(f[[2]], env), con = sql_current_con())
    value[[i]] <- escape(eval_bare(f[[3]], env), con = sql_current_con())
  }

  clauses <- purrr::map2_chr(query, value, ~ paste0("WHEN (", .x, ") THEN (", .y, ")"))
  sql(paste0(
    "CASE\n",
    paste0(clauses, collapse = "\n"),
    "\nEND"
  ))
}

