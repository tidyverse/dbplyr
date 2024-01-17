#' Translate an expression to SQL
#'
#' @description
#' dbplyr translates commonly used base functions including logical
#' (`!`, `&`, `|`), arithmetic (`^`), and comparison (`!=`) operators, as well
#' as common summary (`mean()`, `var()`), and transformation (`log()`)
#' functions.  All other functions will be preserved as is. R's infix functions
#' (e.g. `%like%`) will be converted to their SQL equivalents (e.g. `LIKE`).
#'
#' Learn more in `vignette("translation-function")`.
#'
#' @param ...,dots Expressions to translate. `translate_sql()`
#'   automatically quotes them for you.  `translate_sql_()` expects
#'   a list of already quoted objects.
#' @param con An optional database connection to control the details of
#'   the translation. The default, `NULL`, generates ANSI SQL.
#' @param vars_group,vars_order,vars_frame Parameters used in the `OVER`
#'   expression of windowed functions.
#' @param window Use `FALSE` to suppress generation of the `OVER`
#'   statement used for window functions. This is necessary when generating
#'   SQL for a grouped summary.
#' @param context Use to carry information for special translation cases. For example, MS SQL needs a different conversion for is.na() in WHERE vs. SELECT clauses.  Expects a list.
#' @export
#' @examples
#' con <- simulate_dbi()
#'
#' # Regular maths is translated in a very straightforward way
#' translate_sql(x + 1, con = con)
#' translate_sql(sin(x) + tan(y), con = con)
#'
#' # Note that all variable names are escaped
#' translate_sql(like == "x", con = con)
#' # In ANSI SQL: "" quotes variable _names_, '' quotes strings
#'
#' # Logical operators are converted to their sql equivalents
#' translate_sql(x < 5 & !(y >= 5), con = con)
#' # xor() doesn't have a direct SQL equivalent
#' translate_sql(xor(x, y), con = con)
#'
#' # If is translated into case when
#' translate_sql(if (x > 5) "big" else "small", con = con)
#'
#' # Infix functions are passed onto SQL with % removed
#' translate_sql(first %like% "Had%", con = con)
#' translate_sql(first %is% NA, con = con)
#' translate_sql(first %in% c("John", "Roger", "Robert"), con = con)
#'
#' # And be careful if you really want integers
#' translate_sql(x == 1, con = con)
#' translate_sql(x == 1L, con = con)
#'
#' # If you have an already quoted object, use translate_sql_:
#' x <- quote(y + 1 / sin(t))
#' translate_sql_(list(x), con = simulate_dbi())
#'
#' # Windowed translation --------------------------------------------
#' # Known window functions automatically get OVER()
#' translate_sql(mpg > mean(mpg), con = con)
#'
#' # Suppress this with window = FALSE
#' translate_sql(mpg > mean(mpg), window = FALSE, con = con)
#'
#' # vars_group controls partition:
#' translate_sql(mpg > mean(mpg), vars_group = "cyl", con = con)
#'
#' # and vars_order controls ordering for those functions that need it
#' translate_sql(cumsum(mpg), con = con)
#' translate_sql(cumsum(mpg), vars_order = "mpg", con = con)
translate_sql <- function(...,
                          con,
                          vars_group = NULL,
                          vars_order = NULL,
                          vars_frame = NULL,
                          window = TRUE) {
  translate_sql_(
    quos(...),
    con = con,
    vars_group = vars_group,
    vars_order = vars_order,
    vars_frame = vars_frame,
    window = window
  )
}

test_translate_sql <- function(...,
                          con = NULL,
                          vars_group = NULL,
                          vars_order = NULL,
                          vars_frame = NULL,
                          window = TRUE) {
  translate_sql(
    ...,
    con = con %||% sql_current_con(),
    vars_group = vars_group,
    vars_order = vars_order,
    vars_frame = vars_frame,
    window = window
  )
}

#' @export
#' @rdname translate_sql
translate_sql_ <- function(dots,
                           con,
                           vars_group = NULL,
                           vars_order = NULL,
                           vars_frame = NULL,
                           window = TRUE,
                           context = list()) {
  check_con(con)

  if (length(dots) == 0) {
    return(sql())
  }

  check_list(dots)

  if (!any(have_name(dots))) {
    names(dots) <- NULL
  }

  old_con <- set_current_con(con)
  on.exit(set_current_con(old_con), add = TRUE)

  if (length(context) > 0) {
    old_context <- set_current_context(context)
    on.exit(set_current_context(old_context), add = TRUE)
  }

  if (window) {
    old_group <- set_win_current_group(vars_group)
    on.exit(set_win_current_group(old_group), add = TRUE)

    old_order <- set_win_current_order(vars_order)
    on.exit(set_win_current_order(old_order), add = TRUE)

    old_frame <- set_win_current_frame(vars_frame)
    on.exit(set_win_current_frame(old_frame), add = TRUE)
  }

  variant <- dbplyr_sql_translation(con)
  pieces <- lapply(dots, function(x) {
    if (is_null(get_expr(x))) {
      NULL
    } else if (is_atomic(get_expr(x))) {
      escape(get_expr(x), con = con)
    } else {
      mask <- sql_data_mask(x, variant, con = con, window = window)
      escape(eval_tidy(x, mask), con = con)
    }
  })

  sql(unlist(pieces))
}

sql_data_mask <- function(expr,
                          variant,
                          con,
                          window = FALSE,
                          strict = getOption("dplyr.strict_sql", FALSE)) {
  stopifnot(is.sql_variant(variant))

  # Default for unknown functions
  unknown <- setdiff(all_calls(expr), names(variant))
  op <- if (strict) missing_op else default_op
  top_env <- ceply(unknown, op, parent = empty_env(), env = get_env(expr))

  # Known R -> SQL functions
  special_calls <- copy_env(variant$scalar, parent = top_env)
  if (!window) {
    special_calls2 <- copy_env(variant$aggregate, parent = special_calls)
  } else {
    special_calls2 <- copy_env(variant$window, parent = special_calls)
  }
  special_calls2$`::` <- function(pkg, name) {
    pkg <- as.character(substitute(pkg))
    name <- as.character(substitute(name))
    if (!is_installed(pkg)) {
      cli_abort("There is no package called {.pkg {pkg}}")
    }
    if (!env_has(ns_env(pkg), name)) {
      cli_abort("{.val {name}} is not an exported object from {.pkg {pkg}}")
    }

    if (env_has(special_calls2, name) || env_has(special_calls, name)) {
      env_get(special_calls2, name, inherit = TRUE)
    } else {
      cli_abort("No known translation", call = call2(call2("::", sym(pkg), sym(name))))
    }
  }

  special_calls2$sql <- function(...) {
    dots <- exprs(...)

    env <- get_env(expr)
    dots <- purrr::map(dots, eval_tidy, env = env)

    exec(sql, !!!dots)
  }

  # Existing symbols in expression
  names <- all_names(expr)
  idents <- lapply(names, ident)
  name_env <- ceply(idents, escape, con = con, parent = special_calls2)

  # Known sql expressions
  symbol_env <- env_clone(base_symbols, parent = name_env)

  new_data_mask(symbol_env, top_env)
}

is_infix_base <- function(x) {
  x %in% c("::", "$", "@", "^", "*", "/", "+", "-", ">", ">=", "<", "<=",
    "==", "!=", "!", "&", "&&", "|", "||", "~", "<-", "<<-")
}
is_infix_user <- function(x) {
  grepl("^%.*%$", x)
}

default_op <- function(x, env) {
  check_string(x)

  # Check for shiny reactives; these are zero-arg functions
  # so need special handling to give a useful error
  obj <- env_get(env, x, default = NULL, inherit = TRUE)
  if (inherits(obj, "reactive")) {
    error_embed("a shiny reactive", "foo()")
  }

  if (is_infix_base(x)) {
    sql_infix(x)
  } else if (is_infix_user(x)) {
    x <- substr(x, 2, nchar(x) - 1)
    sql_infix(x)
  } else {
    sql_prefix(x)
  }
}

missing_op <- function(x, env) {
  force(x)

  function(...) {
    needs_parens <- !is_infix_base(x) && !is_infix_user(x)

    if (needs_parens) {
      cli_abort("Don't know how to translate {.fun {x}}")
    } else {
      cli_abort("Don't know how to translate `{x}`")
    }
  }
}

all_calls <- function(x) {
  if (is_quosure(x)) return(all_calls(quo_get_expr(x)))
  if (!is.call(x)) return(NULL)

  fname <- as.character(x[[1]])
  unique(c(fname, unlist(lapply(x[-1], all_calls), use.names = FALSE)))
}

all_names <- function(x) {
  if (is.name(x)) return(as.character(x))
  if (is_quosure(x)) return(all_names(quo_get_expr(x)))
  if (!is.call(x)) return(NULL)

  unique(unlist(lapply(x[-1], all_names), use.names = FALSE))
}

# character vector -> environment
ceply <- function(x, f, ..., parent = parent.frame()) {
  if (length(x) == 0) return(new.env(parent = parent))
  l <- lapply(x, f, ...)
  names(l) <- x
  list2env(l, parent = parent)
}
