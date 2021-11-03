#' Create, modify, and delete columns
#'
#' These are methods for the dplyr [mutate()] and [transmute()] generics.
#' They are translated to computed expressions in the `SELECT` clause of
#' the SQL query.
#'
#' @inheritParams arrange.tbl_lazy
#' @inheritParams dplyr::mutate
#' @inherit arrange.tbl_lazy return
#' @export
#' @importFrom dplyr mutate
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' db <- memdb_frame(x = 1:5, y = 5:1)
#' db %>%
#'   mutate(a = (x + y) / 2, b = sqrt(x^2L + y^2L)) %>%
#'   show_query()
#'
#' # dbplyr automatically creates subqueries as needed
#' db %>%
#'   mutate(x1 = x + 1, x2 = x1 * 2) %>%
#'   show_query()
mutate.tbl_lazy <- function(.data, ..., .keep = c("all", "used", "unused", "none")) {
  keep <- arg_match(.keep)

  dots <- quos(..., .named = TRUE)
  if (!is_null(dots[[".before"]])) {
    abort("`mutate()` for lazy tables does not support the `.before` argument")
  }
  if (!is_null(dots[[".after"]])) {
    abort("`mutate()` for lazy tables does not support the `.after` argument")
  }
  dots <- partial_eval_dots(dots, vars = op_vars(.data))

  cols_data <- op_vars(.data)
  cols_group <- group_vars(.data)

  cols_expr <- names(dots)
  cols_expr_modified <- intersect(cols_expr, cols_data)

  cols_used <- setdiff(get_expr_vars(dots), c(cols_group, cols_expr_modified))
  cols_unused <- setdiff(cols_data, c(cols_group, cols_expr_modified, get_expr_vars(dots)))

  cols_out <- union(cols_data, cols_expr)
  if (keep == "all") {
    cols_retain <- cols_out
  } else if (keep == "used") {
    cols_retain <- setdiff(cols_out, cols_unused)
  } else if (keep == "unused") {
    cols_retain <- setdiff(cols_out, cols_used)
  } else if (keep == "none") {
    cols_retain <- setdiff(cols_out, c(cols_used, cols_unused))
  }

  layers <- get_mutate_layers(dots, cols_data, cols_retain)

  for (layer in layers) {
    .data$ops <- op_select(.data$ops, layer)
  }

  .data
}

#' @export
#' @importFrom dplyr transmute
transmute.tbl_lazy <- function(.data, ...) {
  dots <- dbplyr_check_transmute_args(...)
  mutate(.data, !!!dots, .keep = "none")
}

# copy of `dplyr:::check_transmute_args()`
dbplyr_check_transmute_args <- function (..., .keep, .before, .after) {
  if (!missing(.keep)) {
    abort("`transmute()` does not support the `.keep` argument")
  }
  if (!missing(.before)) {
    abort("`transmute()` does not support the `.before` argument")
  }
  if (!missing(.after)) {
    abort("`transmute()` does not support the `.after` argument")
  }
  enquos(...)
}

# helpers -----------------------------------------------------------------

get_expr_vars <- function(dots) {
  used_vars <- character()

  for (i in seq_along(dots)) {
    cur_used_vars <- all_names(get_expr(dots[[i]]))
    used_vars <- c(used_vars, cur_used_vars)
  }

  unique(used_vars)
}

get_mutate_layers <- function(dots, all_vars, vars_out) {
  # For each expression, check if it uses any newly created variables.
  # If so, create a new layer
  new_vars <- character()
  layers <- list()
  cur_layer <- set_names(syms(all_vars), all_vars)

  for (i in seq_along(dots)) {
    used_vars <- get_expr_vars(dots[i])

    if (any(used_vars %in% new_vars)) {
      layers <- append(layers, list(cur_layer))
      all_vars <- c(all_vars, setdiff(new_vars, all_vars))
      cur_layer <- set_names(syms(all_vars), all_vars)
      new_vars <- character()
    }

    cur_var <- names(dots)[[i]]
    cur_layer[[cur_var]] <- dots[[i]]
    new_vars <- c(new_vars, cur_var)
  }

  layers <- append(layers, list(cur_layer))
  simplify_mutate_layers(layers, vars_out)
}

simplify_mutate_layers <- function(layers, vars_out) {
  # In each layer only keep the vars actually required
  vars_required <- vars_out
  for (i in rev(seq_along(layers))) {
    layers[[i]] <- layers[[i]][vars_required]
    vars_required <- get_expr_vars(layers[[i]])
  }

  layers
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

