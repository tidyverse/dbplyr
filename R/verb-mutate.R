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
  dots <- quos(..., .named = TRUE)
  dots <- partial_eval_dots(dots, vars = op_vars(.data), named = TRUE)

  nest_vars(.data, dots, union(op_vars(.data), op_grps(.data)))
}

#' @export
#' @importFrom dplyr transmute
transmute.tbl_lazy <- function(.data, ...) {
  dots <- quos(..., .named = TRUE)
  dots <- partial_eval_dots(dots, vars = op_vars(.data), named = TRUE)

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
      new_actions <- dots[seq2(init, length(dots))][new_vars]
      .data$ops <- op_select(.data$ops, carry_over(union(all_vars, used_vars), new_actions))
      .data$lazy_query <- add_mutate(.data, carry_over(union(all_vars, used_vars), new_actions))
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
  .data$lazy_query <- add_mutate(.data, carry_over(all_vars, dots))
  .data
}

add_mutate <- function(.data, vars) {
  lazy_query <- .data$lazy_query

  # TODO avoid code duplication with `add_select()`
  if (identical(lazy_query$last_op, "select")) {
    # Special optimisation when applied to pure projection() - this is
    # conservative and we could expand to any op_select() if combined with
    # the logic in nest_vars()
    select <- lazy_query$select

    if (purrr::every(vars, is.symbol)) {
      # if current operation is pure projection
      # we can just subset the previous selection
      sel_vars <- purrr::map_chr(vars, as_string)
      lazy_query <- update_lazy_select(lazy_query, sel_vars)

      return(lazy_query)
    }

    prev_vars <- select$expr
    if (purrr::every(prev_vars, is.symbol)) {
      # if previous operation is pure projection
      sel_vars <- purrr::map_chr(prev_vars, as_string)
      if (all(select$name == sel_vars)) {
        # and there's no renaming
        # we can just ignore the previous step
        lazy_query$select <- new_lazy_select(
          vars,
          group_vars = op_grps(lazy_query),
          order_vars = op_sort(lazy_query),
          frame = op_frame(lazy_query)
        )
        # lazy_query <- update_lazy_select(lazy_query, vars)
        return(lazy_query)
      }
    }
  }

  lazy_select_query(
    from = lazy_query,
    last_op = "mutate",
    select = vars
  )
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

