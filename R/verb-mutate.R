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

  by <- compute_by({{ .by }}, .data, by_arg = ".by", data_arg = ".data")
  if (by$from_by) {
    .data$lazy_query$group_vars <- by$names
  }

  layer_info <- get_mutate_layers(.data, ...)
  used <- layer_info$used_vars
  layers <- layer_info$layers

  # The layers may contain `var = quo(NULL)` at this point.
  # They are removed in `add_mutate()`.
  out <- .data
  for (layer in layers) {
    out$lazy_query <- add_mutate(out$lazy_query, layer)
  }

  if (by$from_by) {
    out$lazy_query$group_vars <- character()
  }

  names_original <- names(.data)

  out <- mutate_relocate(
    out = out,
    before = {{ .before }},
    after = {{ .after }},
    names_original = names_original
  )

  names_new <- layer_info$modified_vars
  names_groups <- by$names

  out <- mutate_keep(
    out = out,
    keep = keep,
    used = used,
    names_new = names_new,
    names_groups = names_groups
  )

  out
}

#' @export
#' @importFrom dplyr transmute
transmute.tbl_lazy <- function(.data, ...) {
  layer_info <- get_mutate_layers(.data, ...)

  for (layer in layer_info$layers) {
    .data$lazy_query <- add_mutate(.data$lazy_query, layer)
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

add_mutate <- function(lazy_query, vars) {
  # drop NULLs
  vars <- purrr::discard(vars, ~ (is_quosure(.x) && quo_is_null(.x)) || is.null(.x))

  if (is_projection(vars)) {
    sel_vars <- purrr::map_chr(vars, as_string)
    out <- add_select(lazy_query, sel_vars)

    return(out)
  }

  if (is_lazy_select_query(lazy_query)) {
    # Special optimisation when applied to pure projection() - this is
    # conservative and we could expand to any op_select() if combined with
    # the logic in get_mutate_layers()
    select <- lazy_query$select
    is_select_op <- lazy_query$select_operation %in% c("select", "mutate")
    if (is_pure_projection(select$expr, select$name) && is_select_op && !is_true(lazy_query$distinct)) {
      lazy_query$select <- new_lazy_select(
        vars,
        group_vars = op_grps(lazy_query),
        order_vars = op_sort(lazy_query),
        frame = op_frame(lazy_query)
      )
      return(lazy_query)
    }
  }

  lazy_select_query(
    x = lazy_query,
    select_operation = "mutate",
    select = vars
  )
}

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
  dot_names <- names2(exprs(...))
  was_named <- have_name(exprs(...))

  layer_modified_vars <- character()
  all_modified_vars <- character()
  used_vars <- character()
  all_vars <- op_vars(.data)

  # Each dot may contain an `across()` expression which can refer to freshly
  # created variables. So, it is necessary to keep track of the current data
  # to partially evaluate the dot.
  cur_data <- .data
  cur_layer <- syms(set_names(all_vars))
  layers <- list()

  for (i in seq_along(dots)) {
    dot <- dots[[i]]
    dot_name <- dot_names[[i]]
    quosures <- partial_eval_quo(dot, cur_data, error_call, dot_name, was_named[[i]])

    if (!is.list(quosures)) {
      quosures <- set_names(list(quosures), names(dots)[[i]])
    }
    quosures <- unclass(quosures)
    cols_result <- get_mutate_dot_cols(quosures, all_vars)

    if (any(cols_result$used_vars %in% layer_modified_vars)) {
      layers <- append(layers, list(cur_layer))

      cur_layer <- syms(set_names(names(cur_layer)))
      layer_modified_vars <- character()
    }

    used_vars <- c(used_vars, cols_result$used_vars)
    layer_modified_vars <- c(layer_modified_vars, cols_result$modified_vars)
    all_modified_vars <- c(all_modified_vars, cols_result$modified_vars)

    cur_layer <- purrr::list_assign(cur_layer, !!!cols_result$cols)
    all_vars <- c(all_vars, setdiff(cols_result$modified_vars, all_vars))

    cols <- set_names(syms(names(cur_layer)))
    cols <- purrr::list_assign(cur_layer, !!!cols_result$cols)
    cur_data$lazy_query <- add_mutate(cur_data$lazy_query, cols)

    removed_cols <- cols_result$removed_cols
    cur_data$lazy_query <- add_select(
      cur_data$lazy_query,
      set_names(setdiff(all_vars, removed_cols))
    )
  }

  list(
    layers = append(layers, list(cur_layer)),
    modified_vars = all_modified_vars,
    used_vars = set_names(all_vars %in% used_vars, all_vars)
  )
}

get_mutate_dot_cols <- function(quosures, all_vars) {
  cols <- list()
  modified_vars <- character()
  used_vars <- character()
  var_is_null <- logical()

  for (k in seq_along(quosures)) {
    cur_quo <- quosures[[k]]
    cur_var <- names(quosures)[[k]]

    if (quo_is_null(cur_quo)) {
      var_is_null[[cur_var]] <- TRUE
      cols[[cur_var]] <- cur_quo
      modified_vars <- setdiff(modified_vars, cur_var)
      next
    }

    var_is_null[[cur_var]] <- FALSE
    if (quo_is_symbol(cur_quo)) {
      cur_sym <- quo_get_expr(cur_quo)
      if (as_name(cur_sym) %in% all_vars) {
        cur_quo <- cur_sym
      }
    }
    cols[[cur_var]] <- cur_quo

    used_vars <- c(used_vars, all_names(cur_quo))
    modified_vars <- c(modified_vars, cur_var)
  }

  list(
    cols = cols,
    used_vars = used_vars,
    modified_vars = modified_vars,
    removed_cols = names2(var_is_null)[var_is_null]
  )
}

mutate_relocate <- function(out, before, after, names_original) {
  before <- enquo(before)
  after <- enquo(after)

  if (quo_is_null(before) && quo_is_null(after)) {
    return(out)
  }

  # Only change the order of completely new columns that
  # didn't exist in the original data
  names <- names(out)
  names <- setdiff(names, names_original)

  relocate(
    out,
    all_of(names),
    .before = !!before,
    .after = !!after
  )
}

mutate_keep <- function(out, keep, used, names_new, names_groups) {
  names <- names(out)

  if (keep == "all") {
    names_out <- names
  } else {
    names_keep <- switch(
      keep,
      used = names(used)[used],
      unused = names(used)[!used],
      none = character(),
      abort("Unknown `keep`.", .internal = TRUE)
    )
    names_out <- intersect(names, c(names_new, names_groups, names_keep))
  }

  select(out, all_of(names_out))
}
