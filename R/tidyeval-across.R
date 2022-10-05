capture_across <- function(data, x) {
  x <- enquo(x)
  partial_eval_across(get_expr(x), data, get_env(x))
}

partial_eval_across <- function(call, data, env, error_call = caller_env()) {
  call <- match.call(dplyr::across, call, expand.dots = FALSE, envir = env)
  deprecate_across_dots(call, error_call)

  across_setup(data, call, env, allow_rename = TRUE, fn = "across()", error_call = error_call)
}

capture_if_all <- function(data, x) {
  x <- enquo(x)
  partial_eval_if(get_expr(x), data, get_env(x))
}

partial_eval_if <- function(call, data, env, reduce = "&", error_call = caller_env()) {
  call <- match.call(dplyr::if_any, call, expand.dots = FALSE, envir = env)
  deprecate_across_dots(call, error_call)

  if (reduce == "&") {
    fn <- "if_all()"
  } else {
    fn <- "if_any()"
  }
  out <- across_setup(data, call, env, allow_rename = FALSE, fn = fn, error_call = error_call)
  Reduce(function(x, y) call2(reduce, x, y), out)
}

deprecate_across_dots <- function(call, error_call) {
  if (!is_empty(call$...)) {
    details <- paste(c(
      "Supply arguments directly to `.fns` through a lambda instead.",
      "",
      "  # Previously",
      "  across(a:b, mean, na.rm = TRUE)",
      "",
      "  # Now",
      "  across(a:b, ~mean(.x, na.rm = TRUE))"),
      collapse = "\n"
    )
    lifecycle::deprecate_warn(
      when = "2.3.0",
      what = "across(...)",
      details = details,
      env = env_parent(error_call),
      user_env = env_parent(error_call, n = 4L),
    )
  }
}

across_funs <- function(funs, env, data, dots, names_spec, fn, evaluated = FALSE) {
  if (is.null(funs)) {
    fns <- list(`1` = function(x, ...) x)
    names_spec <- names_spec %||% "{.col}"
  } else if (is_quosure(funs)) {
    return(across_funs(quo_squash(funs), env, data, dots, names_spec, fn, evaluated = evaluated))
  } else if (is_symbol(funs) || is_function(funs) ||
             is_call(funs, "~") || is_call(funs, "function")) {
    is_local_list <- function(funs) {
      if (!is_symbol(funs)) {
        return(FALSE)
      }

      funs <- as_name(funs)
      exists(funs, env) && is.list(get(funs, envir = env))
    }

    if (is_local_list(funs)) {
      funs <- eval(funs, env)
      return(across_funs(funs, env, data, dots, names_spec, fn, evaluated = evaluated))
    }

    fns <- list(`1` = across_fun(funs, env, data, dots = dots, fn = fn))
    names_spec <- names_spec %||% "{.col}"
  } else if (is_call(funs, "list")) {
    args <- call_args(funs)
    fns <- lapply(args, across_fun, env, data, dots = dots, fn = fn)
    names_spec <- names_spec %||% "{.col}_{.fn}"
  } else if (is.list(funs)) {
    fns <- lapply(funs, across_fun, env, data, dots = dots, fn = fn)
    names_spec <- names_spec %||% "{.col}_{.fn}"
  } else if (!is.null(env) && !evaluated) {
    # Try evaluating once, just in case
    funs <- eval(funs, env)
    return(across_funs(funs, env, data = data, dots = dots, names_spec = NULL, fn = fn, evaluated = TRUE))
  } else {
    cli_abort(
      "{.arg .fns} must be a NULL, a function, formula, or list",
      call = call2(fn, .ns = "dbplyr")
    )
  }

  list(fns = fns, names = names_spec)
}

across_fun <- function(fun, env, data, dots, fn) {
  if (is_function(fun)) {
    fn_name <- find_fun(fun)
    if (!is_null(fn_name)) {
      return(function(x, cur_col) call2(fn_name, x, !!!dots))
    }
    partial_eval_fun(fun, env, data, fn)
  } else if (is_symbol(fun) || is_string(fun)) {
    function(x, cur_col) call2(fun, x, !!!dots)
  } else if (is_call(fun, "~")) {
    if (!is_empty(dots)) {
      # TODO use {.fun dbplyr::{fn}} after https://github.com/r-lib/cli/issues/422 is fixed
      cli_abort(c(
        "Can't use `...` when a purrr-style lambda is used in {.arg .fns}.",
        i = "Use a lambda instead.",
        i = "Or inline them via a purrr-style lambda."
      ),
      call = call2(fn, .ns = "dbplyr"))
    }

    partial_eval_prepare_fun(f_rhs(fun), c(".", ".x"))
  } else if (is_call(fun, "function")) {
    fun <- eval(fun, env)
    partial_eval_fun(fun, env, data, fn)
  } else {
    cli_abort(c(
      "{.arg .fns} must contain a function or a formula.",
      x = "Problem with {expr_deparse(fun)}"
    ),
    call = call2(fn, .ns = "dbplyr"))
  }
}

partial_eval_fun <- function(fun, env, data, fn) {
  body <- fn_body(fun)
  if (length(body) > 2) {
    cli_abort(
      "Cannot translate functions consisting of more than one statement.",
      call = call2(fn, .ns = "dbplyr")
    )
  }
  args <- fn_fmls_names(fun)

  partial_eval_prepare_fun(body[[2]], args[[1]])
}

partial_eval_prepare_fun <- function(call, sym) {
  call <- replace_sym(call, sym, replace = quote(!!.x))
  call <- replace_call(call, replace = quote(!!.cur_col))
  function(x, .cur_col) inject(
    expr(!!call),
    child_env(empty_env(), .x = x, expr = rlang::expr, .cur_col = .cur_col)
  )
}

across_setup <- function(data,
                         call,
                         env,
                         allow_rename,
                         fn,
                         error_call) {
  grps <- group_vars(data)
  tbl <- ungroup(tidyselect_data_proxy(data))
  tbl <- tbl[setdiff(colnames(tbl), grps)]

  .cols <- call$.cols %||% expr(everything())
  locs <- tidyselect::eval_select(
    .cols,
    tbl,
    env = env,
    allow_rename = allow_rename,
    error_call = call(fn)
  )

  vars <- syms(names(tbl))[locs]
  if (allow_rename) {
    names_vars <- names(locs)
  } else {
    names_vars <- names(tbl)[locs]
  }

  dots <- call$...
  for (i in seq_along(call$...)) {
    dot <- call$...[[i]]
    try_fetch({
      dots[[i]] <- partial_eval(dot, data = data, env = env, error_call = error_call)
    }, error = function(cnd) {
      dot_name <- get_dot_name(call$..., i, have_name(call$...))
      expr <- glue::glue("{dot_name} = {as_label(dot)}")
      msg <- "Problem while evaluating {.code {expr}}."
      cli_abort(msg, call = call(fn), parent = cnd)
    })
  }

  names_spec <- eval(call$.names, env)
  funs_across_data <- across_funs(
    funs = call$.fns,
    env = env,
    data = data,
    dots = dots,
    names_spec = names_spec,
    fn = fn
  )

  fns_is_null <- funs_across_data$fns_is_null
  fns <- funs_across_data$fns
  names_spec <- funs_across_data$names

  # make sure fns has names, use number to replace unnamed
  names_fns <- names2(fns)
  empties <- which(names_fns == "")
  if (length(empties)) {
    names_fns[empties] <- empties
  }

  glue_mask <- across_glue_mask(env,
    .col = rep(names_vars, each = length(fns)),
    .fn  = rep(names_fns , length(vars))
  )
  names_out <- vctrs::vec_as_names(glue(names_spec, .envir = glue_mask), repair = "check_unique")

  across_apply_fns(vars, fns, names_out, env)
}

across_apply_fns <- function(vars, fns, names, env) {
  out <- vector("list", length(vars) * length(fns))
  out <- set_names(out, names)
  k <- 1
  for (i in seq_along(vars)) {
    for (j in seq_along(fns)) {
      out[[k]] <- exec(fns[[j]], vars[[i]], as_name(vars[[i]]))
      k <- k + 1
    }
  }
  out
}

replace_call <- function(call, replace) {
  if (is_call(call)) {
    if (is_call(call, "cur_column")) {
      replace
    } else {
      call[] <- lapply(call, replace_call, replace = replace)
      call
    }
  } else {
    call
  }
}

across_glue_mask <- function(.col, .fn, .caller_env) {
  glue_mask <- env(.caller_env, .col = .col, .fn = .fn)
  # TODO: we can make these bindings louder later
  env_bind_active(
    glue_mask, col = function() glue_mask$.col, fn = function() glue_mask$.fn
  )
  glue_mask
}
