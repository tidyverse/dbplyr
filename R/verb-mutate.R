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
mutate.tbl_lazy <- function(.data, ...) {
  layer_info <- get_mutate_layers(.data, ...)

  for (layer in layer_info$layers) {
    .data$lazy_query <- add_select(.data, layer, "mutate")
  }

  .data
}

#' @export
#' @importFrom dplyr transmute
transmute.tbl_lazy <- function(.data, ...) {
  layer_info <- get_mutate_layers(.data, ...)

  for (layer in layer_info$layers) {
    .data$lazy_query <- add_select(.data, layer, "mutate")
  }

  cols_expr <- layer_info$modified_vars
  cols_group <- group_vars(.data)
  cols_group <- setdiff(cols_group, cols_expr)
  cols_retain <- c(cols_group, cols_expr)

  select(.data, all_of(cols_retain))
}

# helpers -----------------------------------------------------------------

get_mutate_layers <- function(.data, ...) {
  dots <- enquos(..., .named = TRUE)
  grps <- syms(op_grps(.data))
  cur_data <- simulate_lazy_tbl(op_vars(.data), grps)

  layer_modified_vars <- character()
  all_modified_vars <- character()
  all_vars <- op_vars(cur_data)

  cur_layer <- set_names(syms(all_vars), all_vars)
  layers <- list()

  for (i in seq_along(dots)) {
    quosures <- partial_eval_quo(dots[[i]], cur_data)
    if (!is.list(quosures)) {
      quosures <- set_names(list(quosures), names(dots)[[i]])
    }
    quosures <- unclass(quosures)

    for (k in seq_along(quosures)) {
      cur_quo <- quosures[[k]]
      cur_var <- names(quosures)[[k]]

      if (quo_is_null(cur_quo)) {
        cur_layer[[cur_var]] <- NULL
        layer_modified_vars <- setdiff(layer_modified_vars, cur_var)
        all_modified_vars <- setdiff(all_modified_vars, cur_var)
        all_vars <- setdiff(all_vars, cur_var)
        next
      }

      all_modified_vars <- c(all_modified_vars, setdiff(cur_var, all_modified_vars))
      all_vars <- c(all_vars, setdiff(cur_var, all_vars))

      used_vars <- all_names(cur_quo)
      if (any(used_vars %in% layer_modified_vars)) {
        layers <- append(layers, list(cur_layer))

        cur_layer <- set_names(syms(all_vars), all_vars)
        layer_modified_vars <- character()
      }

      cur_layer[[cur_var]] <- cur_quo
      layer_modified_vars <- c(layer_modified_vars, cur_var)
    }

    cur_data <- simulate_lazy_tbl(all_vars, grps)
  }

  list(
    layers = append(layers, list(cur_layer)),
    modified_vars = all_modified_vars
  )
}

simulate_lazy_tbl <- function(vars, groups) {
  df <- as_tibble(as.list(set_names(vars)), .name_repair = "minimal")
  tbl_lazy(df) %>%
    group_by(!!!groups)
}
