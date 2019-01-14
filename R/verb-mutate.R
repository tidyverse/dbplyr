# mutate ------------------------------------------------------------------

#' @export
mutate.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- quos(..., .named = TRUE)
  dots <- partial_eval(dots, vars = op_vars(.data))

  # For each expression, check if it uses any newly created variables.
  # If so, nest the mutate()
  new_vars <- character()
  init <- 0L
  for (i in seq_along(dots)) {
    cur_var <- names(dots)[[i]]
    used_vars <- all_names(get_expr(dots[[i]]))

    if (any(used_vars %in% new_vars)) {
      .data <- add_op_single("mutate", .data, dots = dots[new_vars])
      new_vars <- cur_var
      init <- i
    } else {
      new_vars <- c(new_vars, cur_var)
    }
  }

  if (init != 0L) {
    dots <- dots[-seq2(1L, init - 1)]
  }
  add_op_single("mutate", .data, dots = dots)
}

#' @export
op_vars.op_mutate <- function(op) {
  unique(c(op_vars(op$x), names(op$dots)))
}

#' @export
sql_build.op_mutate <- function(op, con, ...) {
  vars <- op_vars(op$x)

  new_vars <- translate_sql_(
    op$dots,
    con = con,
    vars_group = op_grps(op),
    vars_order = translate_sql_(op_sort(op), con, context = list(clause = "ORDER")),
    vars_frame = op_frame(op),
    context = list(clause = "SELECT")
  )

  select_query(
    sql_build(op$x, con),
    select = overwrite_vars(vars, new_vars, con)
  )
}

# transmute ---------------------------------------------------------------

#' @export
transmute.tbl_lazy <- function(.data, ...) {
  dots <- quos(..., .named = TRUE)
  dots <- partial_eval(dots, vars = op_vars(.data))
  add_op_single("transmute", .data, dots = dots)
}

#' @export
op_vars.op_transmute <- function(op) {
  c(op_grps(op$x), names(op$dots))
}

#' @export
sql_build.op_transmute <- function(op, con, ...) {
  new_vars <- translate_sql_(
    op$dots, con,
    vars_group = op_grps(op),
    vars_order = translate_sql_(op_sort(op), con, context = list(clause = "ORDER")),
    vars_frame = op_frame(op),
    context = list(clause = "SELECT")
  )

  select_query(
    sql_build(op$x, con),
    select = new_vars
  )
}

# helpers -----------------------------------------------------------------

overwrite_vars <- function(vars, new_vars, con) {
  all_names <- unique(c(vars, names(new_vars)))
  new_idx <- match(names(new_vars), all_names)
  all_vars <- c.sql(ident(all_names), con = con)
  all_vars[new_idx] <- c.sql(new_vars, con = con)
  all_vars
}

