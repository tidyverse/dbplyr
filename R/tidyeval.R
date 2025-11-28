#' Partially evaluate an expression.
#'
#' This function partially evaluates an expression, using information from
#' the tbl to determine whether names refer to local expressions
#' or remote variables. This simplifies SQL translation because expressions
#' don't need to carry around their environment - all relevant information
#' is incorporated into the expression.
#'
#' @section Symbol substitution:
#'
#' `partial_eval()` needs to guess if you're referring to a variable on the
#' server (remote), or in the current environment (local). It's not possible to
#' do this 100% perfectly. `partial_eval()` uses the following heuristic:
#'
#' \itemize{
#'   \item If the tbl variables are known, and the symbol matches a tbl
#'     variable, then remote.
#'   \item If the symbol is defined locally, local.
#'   \item Otherwise, remote.
#' }
#'
#' You can override the guesses using `local()` and `remote()` to force
#' computation, or by using the `.data` and `.env` pronouns of tidy evaluation.
#'
#' @param call an unevaluated expression, as produced by [quote()]
#' @param data A lazy data frame backed by a database query.
#' @param env environment in which to search for local values
#' @export
#' @keywords internal
#' @examples
#' lf <- lazy_frame(year = 1980, id = 1)
#' partial_eval(quote(year > 1980), data = lf)
#'
#' ids <- c("ansonca01", "forceda01", "mathebo01")
#' partial_eval(quote(id %in% ids), lf)
#'
#' # cf.
#' partial_eval(quote(id == .data$id), lf)
#'
#' # You can use local() or .env to disambiguate between local and remote
#' # variables: otherwise remote is always preferred
#' year <- 1980
#' partial_eval(quote(year > year), lf)
#' partial_eval(quote(year > local(year)), lf)
#' partial_eval(quote(year > .env$year), lf)
#'
#' # Functions are always assumed to be remote. Use local to force evaluation
#' # in R.
#' f <- function(x) x + 1
#' partial_eval(quote(year > f(1980)), lf)
#' partial_eval(quote(year > local(f(1980))), lf)
partial_eval <- function(
  call,
  data,
  env = caller_env(),
  error_call = caller_env()
) {
  if (!inherits(data, "tbl_lazy")) {
    cli::cli_abort("`data` must be a lazy data frame", call = error_call)
  }

  if (is_sql_literal(call)) {
    call
  } else if (is_symbol(call)) {
    partial_eval_sym(call, data, env)
  } else if (is_quosure(call)) {
    partial_eval(get_expr(call), data, get_env(call), error_call = error_call)
  } else if (is_call(call, "if_any")) {
    out <- partial_eval_if(
      call,
      data,
      env,
      reduce = "|",
      error_call = error_call
    )
    expr(((!!out)))
  } else if (is_call(call, "if_all")) {
    out <- partial_eval_if(
      call,
      data,
      env,
      reduce = "&",
      error_call = error_call
    )
    expr(((!!out)))
  } else if (is_call(call, "across")) {
    partial_eval_across(call, data, env, error_call)
  } else if (is_call(call, "pick")) {
    partial_eval_pick(call, data, env, error_call)
  } else if (is_call(call)) {
    partial_eval_call(call, data, env)
  } else {
    cli_abort("Unknown input type: {typeof(call)}")
  }
}

is_sql_literal <- function(x) {
  is_atomic(x) || is_null(x) || blob::is_blob(x)
}

capture_dot <- function(.data, x) {
  partial_eval(enquo(x), data = .data)
}

partial_eval_dots <- function(
  .data,
  ...,
  # .env = NULL,
  .named = TRUE,
  error_call = caller_env()
) {
  # corresponds to `capture_dots()`
  # browser()
  dots <- as.list(enquos(..., .named = .named))
  dot_names <- names2(exprs(...))
  was_named <- have_name(exprs(...))

  for (i in seq_along(dots)) {
    dot <- dots[[i]]
    # if (!is_null(.env)) {
    #   dot <- quo_set_env(dot, .env)
    # }
    dot_name <- dot_names[[i]]
    dots[[i]] <- partial_eval_quo(
      dot,
      .data,
      error_call,
      dot_name,
      was_named[[i]]
    )
  }

  # Remove names from any list elements
  is_list <- purrr::map_lgl(dots, is.list)
  names2(dots)[is_list] <- ""

  # Auto-splice list results from partial_eval_quo()
  dots[!is_list] <- lapply(dots[!is_list], list)
  unlist(dots, recursive = FALSE)
}

partial_eval_quo <- function(x, data, error_call, dot_name, was_named) {
  # no direct equivalent in `dtplyr`, mostly handled in `dt_squash()`
  withCallingHandlers(
    expr <- partial_eval(
      get_expr(x),
      data,
      get_env(x),
      error_call = error_call
    ),
    error = \(cnd) {
      label <- expr_as_label(x, dot_name)
      msg <- c(i = "In argument: {.code {label}}")
      cli_abort(msg, call = error_call, parent = cnd)
    }
  )

  if (is.list(expr)) {
    if (was_named) {
      msg <- c(
        "In dbplyr, the result of `across()` must be unnamed.",
        i = "`{dot_name} = {as_label(x)}` is named."
      )
      cli_abort(msg, call = error_call)
    }
    lapply(expr, new_quosure, env = get_env(x))
  } else {
    new_quosure(expr, get_env(x))
  }
}

partial_eval_sym <- function(sym, data, env) {
  vars <- op_vars(data)
  name <- as_string(sym)
  if (name %in% vars) {
    sym
  } else if (env_has(env, name, inherit = TRUE)) {
    # Inline the value so that the translation function can choose what to do

    val <- eval_bare(sym, env)
    if (is_atomic(val)) {
      val <- unname(val)
    }
    val
  } else {
    cli::cli_abort(
      "Object {.var {name}} not found.",
      call = NULL
    )
  }
}

is_mask_pronoun <- function(call) {
  is_call(call, c("$", "[["), n = 2) && is_symbol(call[[2]], c(".data", ".env"))
}

partial_eval_call <- function(call, data, env) {
  fun <- call[[1]]

  # Try to find the name of inlined functions
  if (inherits(fun, "inline_colwise_function")) {
    vars <- colnames(tidyselect_data_proxy(data))
    dot_var <- vars[[attr(call, "position")]]
    call <- replace_sym(attr(fun, "formula")[[2]], c(".", ".x"), sym(dot_var))
    env <- get_env(attr(fun, "formula"))
  } else if (is.function(fun)) {
    fun_name <- find_fun(fun)
    if (is.null(fun_name)) {
      # This probably won't work, but it seems like it's worth a shot.
      return(eval_bare(call, env))
    }

    call[[1]] <- fun <- sym(fun_name)
  }

  # Compound calls, apart from `::` aren't translatable
  if (is_call(fun) && !is_call(fun, "::")) {
    if (is_mask_pronoun(fun)) {
      stop(
        "Use local() or remote() to force evaluation of functions",
        call. = FALSE
      )
    } else {
      return(eval_bare(call, env))
    }
  }

  # .data$, .data[[]], .env$, .env[[]] need special handling
  if (is_mask_pronoun(call)) {
    var <- call[[3]]
    if (is_call(call, "[[")) {
      var <- sym(eval(var, env))
    }

    if (is_symbol(call[[2]], ".data")) {
      var
    } else {
      eval_bare(var, env)
    }
  } else {
    # Process call arguments recursively, unless user has manually called
    # remote/local
    if (is_call(call, "local")) {
      eval_bare(call[[2]], env)
    } else if (is_call(call, "remote")) {
      call[[2]]
    } else if (is_call(call, "$")) {
      # Only the 1st argument is evaluated
      call[[2]] <- partial_eval(call[[2]], data = data, env = env)
      call
    } else {
      call[-1] <- lapply(call[-1], partial_eval, data = data, env = env)
      call
    }
  }
}

find_fun <- function(fun) {
  if (is_lambda(fun)) {
    body <- body(fun)
    if (!is_call(body)) {
      return(NULL)
    }

    fun_name <- body[[1]]
    if (!is_symbol(fun_name)) {
      return(NULL)
    }

    as.character(fun_name)
  } else if (is.function(fun)) {
    fun_name(fun)
  }
}

fun_name <- function(fun) {
  # `dtplyr` uses the same idea but needs different environments
  pkg_env <- env_parent(global_env())
  known <- c(ls(base_agg), ls(base_scalar))

  for (x in known) {
    if (!env_has(pkg_env, x, inherit = TRUE)) {
      next
    }

    fun_x <- env_get(pkg_env, x, inherit = TRUE)
    if (identical(fun, fun_x)) {
      return(sym(x))
    }
  }

  NULL
}

replace_sym <- function(call, sym, replace) {
  if (is_symbol(call, sym)) {
    if (is_list(replace)) {
      replace[[match(as_string(call), sym)]]
    } else {
      replace
    }
  } else if (is_call(call)) {
    call[] <- lapply(call, replace_sym, sym = sym, replace = replace)
    call
  } else {
    call
  }
}
