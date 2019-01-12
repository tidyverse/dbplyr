#' Create a local lazy tibble
#'
#' These functions are useful for testing SQL generation without having to
#' have an active database connection. See [simulate_dbi()] for a list
#' available database simulations.
#'
#' @keywords internal
#' @export
#' @examples
#' library(dplyr)
#' df <- data.frame(x = 1, y = 2)
#'
#' df_sqlite <- tbl_lazy(df, src = simulate_sqlite())
#' df_sqlite %>% summarise(x = sd(x, na.rm = TRUE)) %>% show_query()
tbl_lazy <- function(df, src = simulate_dbi()) {
  subclass <- class(src)[[1]]

  make_tbl(
    purrr::compact(c(subclass, "lazy")),
    ops = op_base_local(df),
    src = src
  )
}

setOldClass(c("tbl_lazy", "tbl"))

#' @export
dim.tbl_lazy <- function(x) c(NA, length(op_vars(x)))

#' @export
as.data.frame.tbl_lazy <- function(x, row.names, optional, ...) {
  message("Use show_query() to see generated SQL")
  data.frame()
}

#' @export
#' @rdname tbl_lazy
lazy_frame <- function(..., src = simulate_dbi()) {
  tbl_lazy(tibble(...), src = src)
}

#' @export
same_src.tbl_lazy <- function(x, y) {
  inherits(y, "tbl_lazy")
}

#' @export
tbl_vars.tbl_lazy <- function(x) {
  op_vars(x$ops)
}

#' @export
groups.tbl_lazy <- function(x) {
  lapply(group_vars(x), as.name)
}

#' @export
group_vars.tbl_lazy <- function(x) {
  op_grps(x$ops)
}

# Single table methods ----------------------------------------------------

# registered onLoad
filter.tbl_lazy <- function(.data, ..., .preserve = FALSE) {
  dots <- quos(...)
  dots <- partial_eval(dots, vars = op_vars(.data))
  add_op_single("filter", .data, dots = dots)
}

#' @export
arrange.tbl_lazy <- function(.data, ...) {
  dots <- quos(...)
  dots <- partial_eval(dots, vars = op_vars(.data))
  names(dots) <- NULL

  add_op_single("arrange", .data, dots = dots)
}

#' @export
select.tbl_lazy <- function(.data, ...) {
  dots <- quos(...)
  add_op_single("select", .data, dots = dots)
}

#' @export
rename.tbl_lazy <- function(.data, ...) {
  dots <- quos(...)
  dots <- partial_eval(dots, vars = op_vars(.data))
  add_op_single("rename", .data, dots = dots)
}

#' @export
summarise.tbl_lazy <- function(.data, ...) {
  dots <- quos(..., .named = TRUE)
  dots <- partial_eval(dots, vars = op_vars(.data))
  add_op_single("summarise", .data, dots = dots)
}

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
transmute.tbl_lazy <- function(.data, ...) {
  dots <- quos(..., .named = TRUE)
  dots <- partial_eval(dots, vars = op_vars(.data))
  add_op_single("transmute", .data, dots = dots)
}

#' @export
group_by.tbl_lazy <- function(.data, ..., add = FALSE) {
  dots <- quos(...)
  dots <- partial_eval(dots, vars = op_vars(.data))

  if (length(dots) == 0) {
    return(.data)
  }

  groups <- group_by_prepare(.data, .dots = dots, add = add)
  names <- purrr::map_chr(groups$groups, as_string)

  add_op_single("group_by",
    groups$data,
    dots = set_names(groups$groups, names),
    args = list(add = FALSE)
  )
}

#' @export
head.tbl_lazy <- function(x, n = 6L, ...) {
  if (inherits(x$ops, "op_head")) {
    x$ops$args$n <- min(x$ops$args$n, n)
  } else {
    x$ops <- op_single("head", x = x$ops, args = list(n = n))
  }
  x
}

#' @export
ungroup.tbl_lazy <- function(x, ...) {
  add_op_single("ungroup", x)
}

#' @export
distinct.tbl_lazy <- function(.data, ..., .keep_all = FALSE) {
  dots <- quos(..., .named = TRUE)
  dots <- partial_eval(dots, vars = op_vars(.data))
  add_op_single("distinct", .data, dots = dots, args = list(.keep_all = .keep_all))
}

# Dual table verbs ------------------------------------------------------------

add_op_join <- function(x, y, type, by = NULL, copy = FALSE,
                        suffix = c(".x", ".y"),
                        auto_index = FALSE, ...) {

  if (identical(type, "full") && identical(by, character())) {
    type <- "cross"
    by <- list(x = character(0), y = character(0))
  } else {
    by <- common_by(by, x, y)
  }

  y <- auto_copy(
    x, y,
    copy = copy,
    indexes = if (auto_index) list(by$y)
  )

  vars <- join_vars(op_vars(x), op_vars(y), type = type, by = by, suffix = suffix)

  x$ops <- op_double("join", x, y, args = list(
    vars = vars,
    type = type,
    by = by,
    suffix = suffix
  ))
  x
}

join_vars <- function(x_names, y_names, type, by, suffix = c(".x", ".y")) {
  # Remove join keys from y
  y_names <- setdiff(y_names, by$y)

  # Add suffix where needed
  suffix <- check_suffix(suffix)
  x_new <- add_suffixes(x_names, y_names, suffix$x)
  y_new <- add_suffixes(y_names, x_names, suffix$y)

  # In left and inner joins, return key values only from x
  # In right joins, return key values only from y
  # In full joins, return key values by coalescing values from x and y
  x_x <- x_names
  x_y <- by$y[match(x_names, by$x)]
  x_y[type == "left" | type == "inner"] <- NA
  x_x[type == "right" & !is.na(x_y)] <- NA
  y_x <- rep_len(NA, length(y_names))
  y_y <- y_names

  # Return a list with 3 parallel vectors
  # At each position, values in the 3 vectors represent
  #  alias - name of column in join result
  #  x - name of column from left table or NA if only from right table
  #  y - name of column from right table or NA if only from left table
  list(alias = c(x_new, y_new), x = c(x_x, y_x), y = c(x_y, y_y))
}

check_suffix <- function(x) {
  if (!is.character(x) || length(x) != 2) {
    stop("`suffix` must be a character vector of length 2.", call. = FALSE)
  }

  list(x = x[1], y = x[2])
}

add_suffixes <- function(x, y, suffix) {
  if (identical(suffix, "")) {
    return(x)
  }

  out <- chr_along(x)
  for (i in seq_along(x)) {
    nm <- x[[i]]
    while (nm %in% y || nm %in% out) {
      nm <- paste0(nm, suffix)
    }

    out[[i]] <- nm
  }
  out
}


add_op_semi_join <- function(x, y, anti = FALSE, by = NULL, copy = FALSE,
                             auto_index = FALSE, ...) {
  by <- common_by(by, x, y)
  y <- auto_copy(
    x, y, copy,
    indexes = if (auto_index) list(by$y)
  )

  x$ops <- op_double("semi_join", x, y, args = list(
    anti = anti,
    by = by
  ))
  x
}

add_op_set_op <- function(x, y, type, copy = FALSE, ...) {
  y <- auto_copy(x, y, copy)

  if (inherits(x$src$con, "SQLiteConnection")) {
    # LIMIT only part the compound-select-statement not the select-core.
    #
    # https://www.sqlite.org/syntax/compound-select-stmt.html
    # https://www.sqlite.org/syntax/select-core.html

    if (inherits(x$ops, "op_head") || inherits(y$ops, "op_head")) {
      stop("SQLite does not support set operations on LIMITs", call. = FALSE)
    }
  }

  # Ensure each has same variables
  vars <- union(op_vars(x), op_vars(y))
  x <- fill_vars(x, vars)
  y <- fill_vars(y, vars)

  x$ops <- op_double("set_op", x, y, args = list(type = type))
  x
}

fill_vars <- function(x, vars) {
  x_vars <- op_vars(x)
  if (identical(x_vars, vars)) {
    return(x)
  }

  new_vars <- lapply(set_names(vars), function(var) {
    if (var %in% x_vars) {
      sym(var)
    } else {
      NA
    }
  })

  transmute(x, !!!new_vars)
}

# Currently the dual table verbs are defined on tbl_sql, because the
# because they definitions are bit too tightly connected to SQL.

# lazyeval ----------------------------------------------------------------

#' @export
filter_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  dots <- partial_eval(dots, vars = op_vars(.data))
  add_op_single("filter", .data, dots = dots)
}
#' @export
arrange_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  arrange(.data, !!! dots)
}
#' @export
select_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  add_op_single("select", .data, dots = dots)
}
#' @export
rename_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  dots <- partial_eval(dots, vars = op_vars(.data))
  add_op_single("rename", .data, dots = dots)
}
#' @export
summarise_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  dots <- partial_eval(dots, vars = op_vars(.data))
  add_op_single("summarise", .data, dots = dots)
}
#' @export
mutate_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  dots <- partial_eval(dots, vars = op_vars(.data))
  add_op_single("mutate", .data, dots = dots)
}
#' @export
group_by_.tbl_lazy <- function(.data, ..., .dots = list(), add = FALSE) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  group_by(.data, !!! dots, add = add)
}
#' @export
distinct_.tbl_lazy <- function(.data, ..., .dots = list(), .keep_all = FALSE) {
  dots <- dplyr:::compat_lazy_dots(.dots, caller_env(), ...)
  distinct(.data, !!! dots, .keep_all = .keep_all)
}
