#' Join SQL tables
#'
#' @description
#' These are methods for the dplyr [join] generics. They are translated
#' to the following SQL queries:
#'
#' * `inner_join(x, y)`: `SELECT * FROM x JOIN y ON x.a = y.a`
#' * `left_join(x, y)`:  `SELECT * FROM x LEFT JOIN y ON x.a = y.a`
#' * `right_join(x, y)`: `SELECT * FROM x RIGHT JOIN y ON x.a = y.a`
#' * `full_join(x, y)`:  `SELECT * FROM x FULL JOIN y ON x.a = y.a`
#' * `semi_join(x, y)`:  `SELECT * FROM x WHERE EXISTS (SELECT 1 FROM y WHERE x.a = y.a)`
#' * `anti_join(x, y)`:  `SELECT * FROM x WHERE NOT EXISTS (SELECT 1 FROM y WHERE x.a = y.a)`
#'
#' @param x,y A pair of lazy data frames backed by database queries.
#' @inheritParams dplyr::join
#' @param copy If `x` and `y` are not from the same data source,
#'   and `copy` is `TRUE`, then `y` will be copied into a
#'   temporary table in same database as `x`. `*_join()` will automatically
#'   run `ANALYZE` on the created table in the hope that this will make
#'   you queries as efficient as possible by giving more data to the query
#'   planner.
#'
#'   This allows you to join tables across srcs, but it's potentially expensive
#'   operation so you must opt into it.
#' @param auto_index if `copy` is `TRUE`, automatically create
#'   indices for the variables in `by`. This may speed up the join if
#'   there are matching indexes in `x`.
#' @param sql_on A custom join predicate as an SQL expression.
#'   Usually joins use column equality, but you can perform more complex
#'   queries by supply `sql_on` which should be a SQL expression that
#'   uses `LHS` and `RHS` aliases to refer to the left-hand side or
#'   right-hand side of the join respectively.
#' @param na_matches Should NA (NULL) values match one another?
#'   The default, "never", is how databases usually work. `"na"` makes
#'   the joins behave like the dplyr join functions, [merge()], [match()],
#'   and `%in%`.
#' @param x_as,y_as Alias to use for `x` resp. `y`. Defaults to `"LHS"` resp.
#'   `"RHS"`
#' @inherit arrange.tbl_lazy return
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' band_db <- tbl_memdb(dplyr::band_members)
#' instrument_db <- tbl_memdb(dplyr::band_instruments)
#' band_db %>% left_join(instrument_db) %>% show_query()
#'
#' # Can join with local data frames by setting copy = TRUE
#' band_db %>%
#'   left_join(dplyr::band_instruments, copy = TRUE)
#'
#' # Unlike R, joins in SQL don't usually match NAs (NULLs)
#' db <- memdb_frame(x = c(1, 2, NA))
#' label <- memdb_frame(x = c(1, NA), label = c("one", "missing"))
#' db %>% left_join(label, by = "x")
#' # But you can activate R's usual behaviour with the na_matches argument
#' db %>% left_join(label, by = "x", na_matches = "na")
#'
#' # By default, joins are equijoins, but you can use `sql_on` to
#' # express richer relationships
#' db1 <- memdb_frame(x = 1:5)
#' db2 <- memdb_frame(x = 1:3, y = letters[1:3])
#' db1 %>% left_join(db2) %>% show_query()
#' db1 %>% left_join(db2, sql_on = "LHS.x < RHS.x") %>% show_query()
#' @name join.tbl_sql
NULL

#' @rdname join.tbl_sql
#' @export
#' @importFrom dplyr inner_join
inner_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                                suffix = NULL,
                                auto_index = FALSE, ...,
                                sql_on = NULL, na_matches = c("never", "na"),
                                x_as = NULL, y_as = NULL) {
  x$lazy_query <- add_join(
    x, y,
    "inner",
    by = by,
    sql_on = sql_on,
    copy = copy,
    suffix = suffix,
    auto_index = auto_index,
    na_matches = na_matches,
    x_as = x_as,
    y_as = y_as,
    ...
  )

  x
}

#' @rdname join.tbl_sql
#' @export
#' @importFrom dplyr left_join
left_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                               suffix = NULL,
                               auto_index = FALSE, ...,
                               sql_on = NULL, na_matches = c("never", "na"),
                               x_as = NULL, y_as = NULL) {
  x$lazy_query <- add_join(
    x, y,
    "left",
    by = by,
    sql_on = sql_on,
    copy = copy,
    suffix = suffix,
    auto_index = auto_index,
    na_matches = na_matches,
    x_as = x_as,
    y_as = y_as,
    ...
  )

  x
}

#' @rdname join.tbl_sql
#' @export
#' @importFrom dplyr right_join
right_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                                suffix = NULL,
                                auto_index = FALSE, ...,
                                sql_on = NULL, na_matches = c("never", "na"),
                               x_as = NULL, y_as = NULL) {
  x$lazy_query <- add_join(
    x, y,
    "right",
    by = by,
    sql_on = sql_on,
    copy = copy,
    suffix = suffix,
    auto_index = auto_index,
    na_matches = na_matches,
    x_as = x_as,
    y_as = y_as,
    ...
  )

  x
}

#' @rdname join.tbl_sql
#' @export
#' @importFrom dplyr full_join
full_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                               suffix = NULL,
                               auto_index = FALSE, ...,
                               sql_on = NULL, na_matches = c("never", "na"),
                               x_as = NULL, y_as = NULL) {
  x$lazy_query <- add_join(
    x, y,
    "full",
    by = by,
    sql_on = sql_on,
    copy = copy,
    suffix = suffix,
    auto_index = auto_index,
    na_matches = na_matches,
    x_as = x_as,
    y_as = y_as,
    ...
  )

  x
}

#' @rdname join.tbl_sql
#' @export
#' @importFrom dplyr semi_join
semi_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                               auto_index = FALSE, ...,
                               sql_on = NULL, na_matches = c("never", "na"),
                               x_as = NULL, y_as = NULL) {
  x$lazy_query <- add_semi_join(
    x, y,
    anti = FALSE,
    by = by,
    sql_on = sql_on,
    copy = copy,
    auto_index = auto_index,
    na_matches = na_matches,
    x_as = x_as,
    y_as = y_as,
    ...
  )

  x
}

#' @rdname join.tbl_sql
#' @export
#' @importFrom dplyr anti_join
anti_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                               auto_index = FALSE, ...,
                               sql_on = NULL, na_matches = c("never", "na"),
                               x_as = NULL, y_as = NULL) {
  x$lazy_query <- add_semi_join(
    x, y,
    anti = TRUE,
    by = by,
    sql_on = sql_on,
    copy = copy,
    auto_index = auto_index,
    na_matches = na_matches,
    x_as = x_as,
    y_as = y_as,
    ...
  )

  x
}


add_join <- function(x, y, type, by = NULL, sql_on = NULL, copy = FALSE,
                     suffix = NULL,
                     auto_index = FALSE,
                     na_matches = "never",
                     x_as = NULL,
                     y_as = NULL,
                     call = caller_env()) {
  if (!is.null(sql_on)) {
    by <- list(x = character(0), y = character(0), on = sql(sql_on))
  } else if (identical(type, "full") && identical(by, character())) {
    type <- "cross"
    by <- list(x = character(0), y = character(0))
  } else {
    by <- dplyr::common_by(by, x, y)
  }

  y <- auto_copy(
    x, y,
    copy = copy,
    indexes = if (auto_index) list(by$y)
  )

  suffix <- suffix %||% sql_join_suffix(x$src$con, suffix)
  na_matches <- arg_match(na_matches, c("na", "never"), error_call = call)

  vars <- join_vars(op_vars(x), op_vars(y), type = type, by = by, suffix = suffix, call = call)

  inline_result <- join_inline_select(x$lazy_query, by$x, by$on)
  x_lq <- inline_result$lq
  x_vars <- inline_result$vars
  by_x_org <- by$x
  by$x <- inline_result$by

  inline_result <- join_inline_select(y$lazy_query, by$y, by$on)
  y_lq <- inline_result$lq
  y_vars <- inline_result$vars
  by$y <- inline_result$by

  vars <- update_join_vars(vars, x_lq, y_lq, x_vars, y_vars, type, by)

  # the table alias can only be determined after `select()` was inlined.
  # This works even though `by` is used in `join_inline_select()` and updated
  # because this does not touch `by$x_as` and `by$y_as`.
  join_alias <- check_join_alias(x_as, y_as, sql_on, call)

  x_name <- unclass(query_name(x_lq))
  y_name <- unclass(query_name(y_lq))
  by[c("x_as", "y_as")] <- join_two_table_alias(x_name, y_name, join_alias$x, join_alias$y)
  by$x_as <- ident(by$x_as)
  by$y_as <- ident(by$y_as)

  lazy_join_query(
    x = x_lq,
    y = y_lq,
    vars = vars,
    type = type,
    by = by,
    suffix = suffix,
    na_matches = na_matches,
    call = call
  )
}

join_inline_select <- function(lq, by, on) {
  if (is_empty(on) && is_lazy_select_query_simple(lq, select = "projection")) {
    vars <- purrr::map_chr(lq$select$expr, as_string)

    idx <- vctrs::vec_match(lq$select$name, by)
    by <- vctrs::vec_assign(by, idx, vars)

    lq_org <- lq
    lq <- lq$x
    lq$group_vars <- op_grps(lq_org)
    lq$order_vars <- op_sort(lq_org)
    lq$frame <- op_frame(lq_org)
  } else {
    vars <- op_vars(lq)
  }

  list(
    lq = lq,
    vars = vars,
    by = by
  )
}

update_join_vars <- function(vars, x_lq, y_lq, x_vars, y_vars, type, by) {
  vars$all_x <- op_vars(x_lq)
  vars$all_y <- op_vars(y_lq)

  if (type == "left" || type == "inner") {
    y_vars <- setdiff(y_vars, by$y)
  } else if (type == "right") {
    x_vars <- setdiff(x_vars, by$x)
  }

  vars$x[!is.na(vars$x)] <- x_vars
  vars$y[!is.na(vars$y)] <- y_vars
  vars
}

add_semi_join <- function(x, y, anti = FALSE, by = NULL, sql_on = NULL, copy = FALSE,
                          auto_index = FALSE, na_matches = "never",
                          x_as = NULL, y_as = NULL,
                          call = caller_env()) {
  if (!is.null(sql_on)) {
    by <- list(x = character(0), y = character(0), on = sql(sql_on))
  } else {
    by <- dplyr::common_by(by, x, y)
  }

  y <- auto_copy(
    x, y, copy,
    indexes = if (auto_index) list(by$y)
  )

  inline_result <- join_inline_select(x$lazy_query, by$x, sql_on)
  x_lq <- inline_result$lq
  by$x <- inline_result$by
  vars <- tibble(
    name = op_vars(x),
    var = inline_result$vars
  )

  # the table alias can only be determined after `select()` was inlined
  join_alias <- check_join_alias(x_as, y_as, sql_on, call)

  x_name <- unclass(query_name(x_lq))
  y_name <- unclass(query_name(y))
  by[c("x_as", "y_as")] <- join_two_table_alias(x_name, y_name, join_alias$x, join_alias$y)
  by$x_as <- ident(by$x_as)
  by$y_as <- ident(by$y_as)

  lazy_semi_join_query(
    x_lq,
    y$lazy_query,
    vars = vars,
    anti = anti,
    by = by,
    na_matches = na_matches,
    call = call
  )
}

check_join_alias <- function(x_as, y_as, sql_on, call) {
  x_as <- check_join_as1(x_as, arg = "x_as", sql_on, default = "LHS", call)
  y_as <- check_join_as1(y_as, arg = "y_as", sql_on, default = "RHS", call)

  if (identical(x_as, y_as) && !is.null(x_as)) {
    cli_abort("{.arg y_as} must be different from {.arg x_as}.", call = call)
  }

  list(x = x_as, y = y_as)
}

check_join_as1 <- function(as, arg, sql_on, default, call) {
  if (!is_null(as)) {
    vctrs::vec_assert(as, character(), size = 1, arg = arg, call = call)
  }

  if (!is_null(sql_on)) {
    # for backwards compatibility use "LHS"/"RHS" if `sql_on` is used
    # without a table alias
    as <- as %||% default
  }

  as
}

check_join_as <- function(x_as, x, y_as, y, sql_on, call) {
  if (!is_null(x_as)) {
    vctrs::vec_assert(x_as, character(), size = 1, arg = "x_as", call = call)
  }
  if (!is_null(y_as)) {
    vctrs::vec_assert(y_as, character(), size = 1, arg = "y_as", call = call)
  }

  if (is_null(sql_on)) {
    x_name <- unclass(query_name(x))
    y_name <- unclass(query_name(y))
    if (is_null(x_as) && is_null(y_as) && identical(x_name, y_name)) {
      # minor hack to deal with `*_name` = NULL
      x_as <- paste0(c(x_name, "LHS"), collapse = "_")
      y_as <- paste0(c(y_name, "RHS"), collapse = "_")
      # we can safely omit the check that x_as and y_as are identical
      return(list(x_as = ident(x_as), y_as = ident(y_as)))
    }

    x_as <- x_as %||% x_name %||% "LHS"
    y_as <- y_as %||% y_name %||% "RHS"
  } else {
    # for backwards compatibility use "LHS" and "RHS" if `sql_on` is used
    # without a table alias
    x_as <- x_as %||% "LHS"
    y_as <- y_as %||% "RHS"
  }

  if (identical(x_as, y_as)) {
    cli_abort("{.arg y_as} must be different from {.arg x_as}.", call = call)
  }

  list(x_as = ident(x_as), y_as = ident(y_as))
}

join_vars <- function(x_names, y_names, type, by, suffix = c(".x", ".y"), call = caller_env()) {
  y_names_org <- y_names
  # Remove join keys from y
  y_names <- setdiff(y_names, by$y)

  # Add suffix where needed
  suffix <- check_suffix(suffix, call)
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
  list(
    alias = c(x_new, y_new),
    x = c(x_x, y_x),
    y = c(x_y, y_y),
    all_x = x_names,
    all_y = y_names_org
  )
}

join_two_table_alias <- function(x_name, y_name, x_alias, y_alias) {
  stopifnot(is.null(x_name) || is.character(x_name))
  stopifnot(is.null(y_name) || is.character(y_name))
  stopifnot(is.null(x_alias) || is.character(x_alias))
  stopifnot(is.null(y_alias) || is.character(y_alias))

  x_out <- x_alias %||% x_name %||% "LHS"
  y_out <- y_alias %||% y_name %||% "RHS"
  out <- c(x_out, y_out)

  if (!identical(x_out, y_out)) {
    return(out)
  }
  # -> must rename

  if (is_null(x_alias) && is_null(y_alias)) {
    # self join of named table
    if (!is_null(x_name) && !is_null(y_name) && identical(x_name, y_name)) {
      out <- c(
        paste0(x_name, "_LHS"),
        paste0(y_name, "_RHS")
      )
      return(out)
    }
  }

  out_repaired <- vctrs::vec_as_names(c(x_out, y_out), repair = "unique", quiet = TRUE)
  may_repair <- c(is_null(x_alias), is_null(y_alias))
  out[may_repair] <- out_repaired[may_repair]

  out
}

check_suffix <- function(x, call) {
  vctrs::vec_assert(x, character(), size = 2, arg = "suffix", call = call)
  list(x = x[1], y = x[2])
}

add_suffixes <- function(x, y, suffix) {
  if (identical(suffix, "")) {
    return(x)
  }

  out <- character(length(x))
  for (i in seq_along(x)) {
    nm <- x[[i]]
    while (nm %in% y || nm %in% out) {
      nm <- paste0(nm, suffix)
    }

    out[[i]] <- nm
  }
  out
}
