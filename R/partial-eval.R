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
#' @param vars character vector of variable names.
#' @param env environment in which to search for local values
#' @export
#' @keywords internal
#' @examples
#' vars <- c("year", "id")
#' partial_eval(quote(year > 1980), vars = vars)
#'
#' ids <- c("ansonca01", "forceda01", "mathebo01")
#' partial_eval(quote(id %in% ids), vars = vars)
#'
#' # cf.
#' partial_eval(quote(id == .data$ids), vars = vars)
#'
#' # You can use local() or .env to disambiguate between local and remote
#' # variables: otherwise remote is always preferred
#' year <- 1980
#' partial_eval(quote(year > year), vars = vars)
#' partial_eval(quote(year > local(year)), vars = vars)
#' partial_eval(quote(year > .env$year), vars = vars)
#'
#' # Functions are always assumed to be remote. Use local to force evaluation
#' # in R.
#' f <- function(x) x + 1
#' partial_eval(quote(year > f(1980)), vars = vars)
#' partial_eval(quote(year > local(f(1980))), vars = vars)
#'
#' # For testing you can also use it with the tbl omitted
#' partial_eval(quote(1 + 2 * 3))
#' x <- 1
#' partial_eval(quote(x ^ y))
partial_eval <- function(call, vars = character(), env = caller_env()) {
  if (is_null(call)) {
    NULL
  } else if (is_atomic(call)) {
    call
  } else if (is_symbol(call)) {
    partial_eval_sym(call, vars, env)
  } else if (is_quosure(call)) {
    partial_eval_call(get_expr(call), vars, get_env(call))
  } else if (is_call(call)) {
    partial_eval_call(call, vars, env)
  } else {
    abort(glue("Unknown input type: ", class(call)))
  }
}

partial_eval_dots <- function(dots, vars) {
  stopifnot(inherits(dots, "quosures"))

  lapply(dots, function(x) {
    new_quosure(
      partial_eval(get_expr(x), vars = vars, env = get_env(x)),
      get_env(x)
    )
  })
}

partial_eval_sym <- function(sym, vars, env) {
  name <- as_string(sym)
  if (name %in% vars) {
    sym
  } else if (env_has(env, name, inherit = TRUE)) {
    eval_bare(sym, env)
  } else {
    sym
  }
}

is_namespaced_dplyr_call <- function(call) {
  is_symbol(call[[1]], "::") && is_symbol(call[[2]], "dplyr")
}

is_tidy_pronoun <- function(call) {
  is_symbol(call[[1]], c("$", "[[")) && is_symbol(call[[2]], c(".data", ".env"))
}

partial_eval_call <- function(call, vars, env) {
  fun <- call[[1]]

  # Try to find the name of inlined functions
  if (inherits(fun, "inline_colwise_function")) {
    dot_var <- vars[[attr(call, "position")]]
    call <- replace_dot(attr(fun, "formula")[[2]], dot_var)
    env <- get_env(attr(fun, "formula"))
  } else if (is.function(fun)) {
    fun_name <- find_fun(fun)
    if (is.null(fun_name)) {
      # This probably won't work, but it seems like it's worth a shot.
      return(eval_bare(call, env))
    }

    call[[1]] <- fun <- sym(fun_name)
  }

  # So are compound calls, EXCEPT dplyr::foo()
  if (is.call(fun)) {
    if (is_namespaced_dplyr_call(fun)) {
      call[[1]] <- fun[[3]]
    } else if (is_tidy_pronoun(fun)) {
      stop("Use local() or remote() to force evaluation of functions", call. = FALSE)
    } else {
      return(eval_bare(call, env))
    }
  }

  # .data$, .data[[]], .env$, .env[[]] need special handling
  if (is_tidy_pronoun(call)) {
    if (is_symbol(call[[1]], "$")) {
      idx <- call[[3]]
    } else {
      idx <- as.name(eval_bare(call[[3]], env))
    }

    if (is_symbol(call[[2]], ".data")) {
      idx
    } else {
      eval_bare(idx, env)
    }
  } else {
    # Process call arguments recursively, unless user has manually called
    # remote/local
    name <- as_string(call[[1]])
    if (name == "local") {
      eval_bare(call[[2]], env)
    } else if (name == "remote") {
      call[[2]]
    } else {
      call[-1] <- lapply(call[-1], partial_eval, vars = vars, env = env)
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
  pkg_env <- env_parent(global_env())
  known <- c(ls(base_agg), ls(base_scalar))

  for (x in known) {
    if (!env_has(pkg_env, x, inherit = TRUE))
      next

    fun_x <- env_get(pkg_env, x, inherit = TRUE)
    if (identical(fun, fun_x))
      return(x)
  }

  NULL
}

replace_dot <- function(call, var) {
  if (is_symbol(call, ".")) {
    sym(var)
  } else if (is_call(call)) {
    call[] <- lapply(call, replace_dot, var = var)
    call
  } else {
    call
  }
}
