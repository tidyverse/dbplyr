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
mutate.tbl_lazy <- function(.data,
                            ...,
                            .by = NULL,
                            .keep = c("all", "used", "unused", "none"),
                            .before = NULL,
                            .after = NULL) {
  keep <- arg_match(.keep)

  by <- compute_by(
    {{ .by }},
    .data,
    by_arg = ".by",
    data_arg = ".data",
    error_call = caller_env()
  )
  if (by$from_by) {
    .data$lazy_query$group_vars <- by$names
  }

  layer_info <- get_mutate_layers(.data, ...)
  used <- layer_info$used_vars
  layers <- layer_info$layers

  # The layers may contain `var = quo(NULL)` at this point.
  # They are removed in `add_select()`.
  out <- .data
  for (layer in layers) {
    out$lazy_query <- add_select(out, layer, "mutate")
  }

  cols_data <- op_vars(.data)
  cols_group <- group_vars(.data)

  cols_expr <- layer_info$modified_vars
  cols_expr_modified <- intersect(cols_expr, cols_data)
  cols_expr_new <- setdiff(cols_expr, cols_expr_modified)

  cols_used <- setdiff(cols_data, c(cols_group, cols_expr_modified, names(used)[!used]))
  cols_unused <- setdiff(cols_data, c(cols_group, cols_expr_modified, names(used)[used]))

  .before <- enquo(.before)
  .after <- enquo(.after)

  if (!quo_is_null(.before) || !quo_is_null(.after)) {
    # Only change the order of new columns
    out <- relocate(out, all_of(cols_expr_new), .before = !!.before, .after = !!.after)
  }

  cols_out <- op_vars(out)

  if (keep == "all") {
    cols_retain <- cols_out
  } else if (keep == "used") {
    cols_retain <- setdiff(cols_out, cols_unused)
  } else if (keep == "unused") {
    cols_retain <- setdiff(cols_out, cols_used)
  } else if (keep == "none") {
    cols_retain <- setdiff(cols_out, c(cols_used, cols_unused))
  }

  if (by$from_by) {
    out$lazy_query$group_vars <- character()
  }

  select(out, all_of(cols_retain))
}

#' @export
#' @importFrom dplyr transmute
transmute.tbl_lazy <- function(.data, ...) {
  layer_info <- get_mutate_layers(.data, ...)

  for (layer in layer_info$layers) {
    .data$lazy_query <- add_select(.data, layer, "mutate")
  }

  # Retain expression columns in order of their appearance
  cols_expr <- layer_info$modified_vars

  # Retain untouched group variables up front
  cols_group <- group_vars(.data)
  cols_group <- setdiff(cols_group, cols_expr)

  cols_retain <- c(cols_group, cols_expr)

  select(.data, all_of(cols_retain))
}

# helpers -----------------------------------------------------------------

# Split mutate expressions in independent layers, e.g.
#
# `get_mutate_layers(lf, b = a + 1, c = a - 1, d = b + 1)`
#
# creates two layers:
# 1) a = a, b = a + 1, c = a - 1
#    because `b` and `c` are independent of each other they can be on the
#    same layer
# 2) a = a, b = b, c = c, d = b + 1
#    because `d` depends on `b` it must be on a new layer
get_mutate_layers <- function(.data, ..., error_call = caller_env()) {
  dots <- as.list(enquos(..., .named = TRUE))
  was_named <- have_name(exprs(...))

  layer_modified_vars <- character()
  all_modified_vars <- character()
  all_used_vars <- character()
  all_vars <- op_vars(.data)
  var_is_null <- rep_named(all_vars, FALSE)

  # Each dot may contain an `across()` expression which can refer to freshly
  # created variables. So, it is necessary to keep track of the current data
  # to partially evaluate the dot.
  # `dot_layer` contains the expressions of the current dot and is only needed
  # to correctly update `cur_data`
  cur_data <- .data
  dot_layer <- syms(set_names(all_vars))

  cur_layer <- syms(set_names(all_vars))
  layers <- list()

  for (i in seq_along(dots)) {
    dot <- dots[[i]]
    dot_name <- get_dot_name(dots, i, was_named)
    quosures <- partial_eval_quo(dot, cur_data, error_call, dot_name, was_named[[i]])

    if (!is.list(quosures)) {
      quosures <- set_names(list(quosures), names(dots)[[i]])
    }
    quosures <- unclass(quosures)

    for (k in seq_along(quosures)) {
      cur_quo <- quosures[[k]]
      cur_var <- names(quosures)[[k]]

      if (quo_is_null(cur_quo)) {
        var_is_null[[cur_var]] <- TRUE
        cur_layer[[cur_var]] <- cur_quo
        dot_layer[[cur_var]] <- cur_quo
        layer_modified_vars <- setdiff(layer_modified_vars, cur_var)
        all_modified_vars <- setdiff(all_modified_vars, cur_var)
        next
      }

      all_modified_vars <- c(all_modified_vars, setdiff(cur_var, all_modified_vars))

      used_vars <- all_names(cur_quo)
      all_used_vars <- c(all_used_vars, used_vars)
      if (any(used_vars %in% layer_modified_vars)) {
        layers <- append(layers, list(cur_layer))

        cur_layer[!var_is_null] <- syms(names(cur_layer)[!var_is_null])
        layer_modified_vars <- character()
      }

      var_is_null[[cur_var]] <- FALSE
      if (quo_is_symbol(cur_quo)) {
        cur_sym <- quo_get_expr(cur_quo)
        if (as_name(cur_sym) %in% all_vars) {
          cur_quo <- cur_sym
        }
      }
      cur_layer[[cur_var]] <- cur_quo
      dot_layer[[cur_var]] <- cur_quo
      layer_modified_vars <- c(layer_modified_vars, cur_var)
    }

    all_vars <- names(cur_layer)[!var_is_null]
    cur_data$lazy_query <- add_select(cur_data, dot_layer, "mutate")
    dot_layer <- syms(set_names(all_vars))
  }

  list(
    layers = append(layers, list(cur_layer)),
    modified_vars = all_modified_vars,
    used_vars = set_names(all_vars %in% all_used_vars, all_vars)
  )
}

get_dot_name <- function(dots, i, was_named) {
  dot_name <- names2(dots)[[i]]
  if (!was_named[[i]]) {
    dot_name <- paste0("..", i)
  }

  dot_name
}
