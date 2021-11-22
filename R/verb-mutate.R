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
  # dots from `across()` do not have a name yet
  dots <- quos_auto_name(dots)

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

  layers <- get_mutate_layers(
    dots, cols_data, cols_retain,
    keep_layer_order = TRUE
  )

  for (layer in layers) {
    .data$ops <- op_select(.data$ops, layer)
  }

  .data
}

#' @export
#' @importFrom dplyr transmute
transmute.tbl_lazy <- function(.data, ...) {
  dots <- dbplyr_check_transmute_args(...)
  dots <- partial_eval_dots(dots, vars = op_vars(.data))
  # dots from `across()` do not have a name yet
  dots <- quos_auto_name(dots)

  # # Retain expression columns in order of their appearance
  cols_expr <- unique(names(dots))

  # Retain untouched group variables up front
  cols_group <- group_vars(.data)
  cols_group <- setdiff(cols_group, cols_expr)

  cols_retain <- c(cols_group, cols_expr)

  layers <- get_mutate_layers(
    dots, op_vars(.data), cols_retain,
    keep_layer_order = FALSE
  )
  for (layer in layers) {
    .data$ops <- op_select(.data$ops, layer)
  }

  .data
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
  quos(..., .named = TRUE)
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

expr_has_sql <- function(x) {
  if (is.sql(x)) return(TRUE)
  if (is_call(x, "sql")) return(TRUE)
  if (is_quosure(x)) return(expr_has_sql(quo_get_expr(x)))
  if (!is.call(x)) return(FALSE)

  any(purrr::map_lgl(x[-1], expr_has_sql))
}

get_used_vars <- function(dots, all_vars) {
  # if SQL is used, we don't know which variables are actually used
  # -> we simply assume every variable is used
  has_sql <- any(purrr::map_lgl(dots, expr_has_sql))
  if (has_sql) return(all_vars)

  used_vars <- character()
  for (i in seq_along(dots)) {
    cur_used_vars <- all_names(get_expr(dots[[i]]))
    used_vars <- c(used_vars, cur_used_vars)
  }

  unique(used_vars)
}

get_mutate_layers <- function(dots, all_vars, vars_out, keep_layer_order) {
  # For each expression, check if it uses any newly created variables.
  # If so, increase the level of the out variable
  vars_level <- rep_named(all_vars, 0)
  max_level <- 1
  cur_layer <- named_syms(all_vars)
  layers <- list(cur_layer)

  for (i in seq_along(dots)) {
    cur_quo <- dots[[i]]
    cur_var <- names(dots)[[i]]
    var_is_known <- cur_var %in% names(vars_level)
    if (quo_is_trivial(cur_quo, cur_var) && var_is_known) next

    used_vars <- get_used_vars(dots[i], all_vars)

    used_vars_max_level <- max(c(vars_level[used_vars], 0))
    if (var_is_known) {
      level <- max(used_vars_max_level + 1, vars_level[[cur_var]] + 1)
    } else {
      all_vars <- c(all_vars, cur_var)
      level <- used_vars_max_level + 1
      layers <- add_mutate_layer_var(layers, cur_var, level)
    }

    vars_level[[cur_var]] <- level
    vars_level[c(used_vars)] <- pmax(vars_level[c(used_vars)], level - 1)

    if (level > max_level) {
      layers <- add_mutate_layer(layers, all_vars)
      max_level <- level
    }
    layers[[level]][[cur_var]] <- cur_quo
  }

  simplify_mutate_layers(layers, vars_out, keep_layer_order)
}


quo_is_trivial <- function(x, name) {
  if (is_quosure(x)) {
    x_expr <- quo_get_expr(x)
  } else {
    x_expr <- x
  }

  if (is_empty(x_expr)) return(FALSE)

  x_expr == sym(name)
}

add_mutate_layer_var <- function(layers, var, var_level) {
  level_max <- length(layers)
  for (cur_level in seq2(var_level, level_max)) {
    layers[[cur_level]][[var]] <- sym(var)
  }

  layers
}

add_mutate_layer <- function(layers, all_vars) {
  new_layer <- named_syms(all_vars)
  append(layers, list(new_layer))
}

named_syms <- function(x) {
  set_names(syms(x), x)
}

simplify_mutate_layers <- function(layers, vars_out, keep_layer_order) {
  # In each layer only keep the vars actually required
  vars_required <- vars_out
  for (i in rev(seq_along(layers))) {
    cur_vars <- names(layers[[i]])
    if (keep_layer_order) {
      layers[[i]] <- layers[[i]][cur_vars %in% vars_required]
    } else {
      layers[[i]] <- layers[[i]][vars_required]
    }

    has_sql <- any(purrr::map_lgl(layers[[i]], expr_has_sql))
    if (has_sql && i > 1) {
      vars_required <- names(layers[[i - 1]])
    } else {
      vars_required <- get_expr_vars(layers[[i]])
    }
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

