# mutate ------------------------------------------------------------------

#' @export
#' @importFrom dplyr mutate
mutate.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- quos(..., .named = TRUE)
  dots <- partial_eval_dots(dots, vars = op_vars(.data))

  nest_vars(.data, dots, union(op_vars(.data), op_grps(.data)))
}

# transmute ---------------------------------------------------------------

#' @export
#' @importFrom dplyr transmute
transmute.tbl_lazy <- function(.data, ...) {
  dots <- quos(..., .named = TRUE)
  dots <- partial_eval_dots(dots, vars = op_vars(.data))

  nest_vars(.data, dots, character())
}

# helpers -----------------------------------------------------------------

# TODO: refactor to remove `.data` argument and return a list of layers.
nest_vars <- function(.data, dots, all_vars) {
  # For each expression, check if it uses any newly created variables.
  # If so, nest the mutate()
  new_vars <- character()
  init <- 0L
  for (i in seq_along(dots)) {
    cur_var <- names(dots)[[i]]
    used_vars <- all_names(get_expr(dots[[i]]))

    if (any(used_vars %in% new_vars)) {
      .data$ops <- op_select(.data$ops, carry_over(all_vars, dots[new_vars]))
      all_vars <- c(all_vars, setdiff(new_vars, all_vars))
      new_vars <- cur_var
      init <- i
    } else {
      new_vars <- c(new_vars, cur_var)
    }
  }

  if (init != 0L) {
    dots <- dots[-seq2(1L, init - 1)]
  }
  .data$ops <- op_select(.data$ops, carry_over(all_vars, dots))
  .data
}

# Combine a selection (passed through from subquery)
# with new actions
carry_over <- function(sel = character(), act = list()) {
  if (is.null(names(sel))) {
    names(sel) <- sel
  }
  sel <- syms(sel)

  # Keep last of duplicated acts
  act <- act[!duplicated(names(act), fromLast = TRUE)]

  # Preserve order of sel
  both <- intersect(names(sel), names(act))
  sel[both] <- act[both]

  # Adding new variables at end
  new <- setdiff(names(act), names(sel))

  c(sel, act[new])
}

