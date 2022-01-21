partial_eval_across <- function(call, vars, env) {
  call <- match.call(dplyr::across, call, expand.dots = FALSE, envir = env)

  # TODO pass lazy_tbl instead of vars and use `simulate_vars(drop_groups = TRUE)`
  # tbl <- simulate_vars(data, drop_groups = TRUE)
  tbl <- as_tibble(rep_named(vars, list(logical())))
  .cols <- call$.cols %||% expr(everything())
  locs <- tidyselect::eval_select(.cols, tbl)
  cols <- syms(names(tbl))[locs]

  funs <- across_funs(call$.fns, env, vars)

  dots <- quos(!!!call$...)
  # TODO this hack gets unnecessary when adapting `partial_eval_quo()`
  dots <- lapply(dots, quo_set_env, env)
  dots <- lapply(dots, partial_eval_quo, vars = vars)
  # dots <- lapply(dots, dt_squash, env = env, data = data, j = j)

  # Generate grid of expressions
  out <- vector("list", length(cols) * length(funs))
  k <- 1
  for (i in seq_along(cols)) {
    for (j in seq_along(funs)) {
      out[[k]] <- exec(funs[[j]], cols[[i]], !!!dots)
      k <- k + 1
    }
  }

  .names <- eval(call$.names, env)
  names(out) <- across_names(names(locs), names(funs), .names, env)
  out
}

capture_if_all <- function(data, x) {
  x <- enquo(x)
  db_squash_if(get_expr(x), get_env(x), colnames(data))
}

db_squash_if <- function(call, env, vars, reduce = "&") {
  call <- match.call(dplyr::if_any, call, expand.dots = FALSE, envir = env)

  # TODO pass lazy_tbl instead of vars and use `simulate_vars(drop_groups = TRUE)`
  # tbl <- simulate_vars(data, drop_groups = TRUE)
  tbl <- as_tibble(rep_named(vars, list(logical())))
  .cols <- call$.cols %||% expr(everything())
  locs <- tidyselect::eval_select(.cols, tbl, allow_rename = FALSE)
  cols <- syms(names(tbl))[locs]

  if (is.null(call$.fns)) {
    return(Reduce(function(x, y) call2(reduce, x, y), cols))
  }

  fun <- across_fun(call$.fns, env, vars)

  out <- vector("list", length(cols))
  for (i in seq_along(cols)) {
    out[[i]] <- exec(fun, cols[[i]], !!!call$...)
  }

  Reduce(function(x, y) call2(reduce, x, y), out)
}

across_funs <- function(funs, env, vars) {
  # TODO add argument `data`?
  if (is.null(funs)) {
    list(function(x, ...) x)
  } else if (is_symbol(funs)) {
    # unlike in `dtplyr` anonymous functions do not work (`is_function(funs)`)
    set_names(list(across_fun(funs, env, vars)), as_label(funs))
  } else if (is.character(funs)) {
    names(funs)[names2(funs) == ""] <- funs
    lapply(funs, across_fun, env, vars)
  } else if (is_call(funs, "~")) {
    set_names(list(across_fun(funs, env, vars)), expr_name(f_rhs(funs)))
  } else if (is_call(funs, "list")) {
    args <- rlang::exprs_auto_name(funs[-1])
    lapply(args, across_fun, env, vars)
  } else if (!is.null(env)) {
    # Try evaluating once, just in case
    funs <- eval(funs, env)
    across_funs(funs, NULL, vars)
  } else {
    abort("`.fns` argument to dbplyr::across() must be a NULL, a function name, formula, or list")
  }
}

across_fun <- function(fun, env, vars) {
  # TODO this should use `data` instead of `vars`
  # unlike in `dtplyr` anonymous functions do not work (`is_function(funs)` || is_call(fun, "function"))
  if (is_symbol(fun) || is_string(fun)) {
    function(x, ...) call2(fun, x, ...)
  } else if (is_call(fun, "~")) {
    call <- db_squash_formula(fun, env, vars, replace = quote(!!.x))
    function(x, ...) expr_interp(call, child_env(emptyenv(), .x = x))
  } else {
    abort(c(
      ".fns argument to dbplyr::across() must contain a function name or a formula",
      x = paste0("Problem with ", expr_deparse(fun))
    ))
  }
}

db_squash_formula <- function(x, env, vars, replace = quote(!!.x)) {
  call <- f_rhs(x)
  call <- replace_dot(call, replace)
  if (is_call(call)) {
    call <- partial_eval_call(call, vars, env)
  }
  call
}

across_names <- function(cols, funs, names = NULL, env = parent.frame()) {
  n_reps <- if (is_empty(funs)) 1 else length(funs)
  if (n_reps == 1) {
    names <- names %||% "{.col}"
  } else {
    names <- names %||% "{.col}_{.fn}"
  }

  col_nms <- names2(cols)
  cols[col_nms != ""] <- col_nms[col_nms != ""]
  glue_env <- child_env(env,
    .col = rep(cols, each = n_reps),
    .fn = rep(funs %||% seq_len(n_reps), length(cols))
  )
  glue(names, .envir = glue_env)
}

across_vars <- function(x) {
  # TODO this can be remove after passing `data` to `partial_eval()` etc.
  # and adding the argument `drop_groups` to `simulate_vars()`
  setdiff(op_vars(x), group_vars(x))
}
