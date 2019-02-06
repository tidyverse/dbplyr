# select and rename -----------------------------------------------------------

#' @export
select.tbl_lazy <- function(.data, ...) {
  dots <- quos(...)

  old_vars <- op_vars(.data$ops)
  new_vars <- tidyselect::vars_select(old_vars, !!!dots, .include = op_grps(.data$ops))

  .data$ops <- op_select(.data$ops, syms(new_vars))
  .data
}

#' @export
rename.tbl_lazy <- function(.data, ...) {
  dots <- quos(...)

  old_vars <- op_vars(.data$ops)
  new_vars <- tidyselect::vars_rename(old_vars, !!!dots)

  .data$ops <- op_select(.data$ops, syms(new_vars))
  .data
}

# op_select ---------------------------------------------------------------

op_select <- function(x, vars) {
  if (inherits(x, "op_select")) {
    # Special optimisation when applied to pure projection() - this is
    # conservative and we could expand to any op_select() if combined with
    # the logic in nest_vars()
    prev_vars <- x$args$vars

    if (purrr::every(vars, is.symbol)) {
      # if current operation is pure projection
      # we can just subset the previous selection
      sel_vars <- purrr::map_chr(vars, as_string)
      vars <- set_names(prev_vars[sel_vars], names(sel_vars))
      x <- x$x
    } else if (purrr::every(prev_vars, is.symbol)) {
      # if previous operation is pure projection
      sel_vars <- purrr::map_chr(prev_vars, as_string)
      if (all(names(sel_vars) == sel_vars)) {
        # and there's no renaming
        # we can just ignore the previous step
        x <- x$x
      }
    }
  }

  new_op_select(x, vars)
}

# SELECT in the SQL sense - powers select(), rename(), mutate(), and transmute()
new_op_select <- function(x, vars) {
  stopifnot(inherits(x, "op"))
  stopifnot(is.list(vars))

  op_single("select", x, dots = list(), args = list(vars = vars))
}

#' @export
op_vars.op_select <- function(op) {
  names(op$args$vars)
}

#' @export
op_grps.op_select <- function(op) {
  # Find renamed variables
  symbols <- purrr::keep(op$args$vars, is_symbol)
  new2old <- purrr::map_chr(symbols, as_string)
  old2new <- set_names(names(new2old), new2old)

  grps <- op_grps(op$x)
  grps[grps %in% names(old2new)] <- old2new[grps]
  grps
}

#' @export
sql_build.op_select <- function(op, con, ...) {

  new_vars <- translate_sql_(
    op$args$vars, con,
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
