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
  lazy_query <- add_join(
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

  x$lazy_query <- lazy_query
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
  lazy_query <- add_join(
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

  x$lazy_query <- lazy_query
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
  lazy_query <- add_join(
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

  x$lazy_query <- lazy_query
  x
}

#' @rdname join.tbl_sql
#' @export
#' @importFrom dplyr semi_join
semi_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                               auto_index = FALSE, ...,
                               sql_on = NULL, na_matches = c("never", "na"),
                               x_as = NULL, y_as = NULL) {
  lazy_query <- add_semi_join(
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

  x$lazy_query <- lazy_query
  x
}

#' @rdname join.tbl_sql
#' @export
#' @importFrom dplyr anti_join
anti_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                               auto_index = FALSE, ...,
                               sql_on = NULL, na_matches = c("never", "na"),
                               x_as = NULL, y_as = NULL) {
  lazy_query <- add_semi_join(
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

  x$lazy_query <- lazy_query
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

  x_lq <- x$lazy_query
  y_lq <- y$lazy_query

  table_alias <- check_join_as(
    x_as,
    x_lq,
    y_as,
    y_lq,
    sql_on = sql_on,
    type = type,
    call = call
  )
  x_as <- unclass(table_alias$x_as)
  y_as <- unclass(table_alias$y_as)

  new_query <- join_needs_new_query(x_lq, x_as, y_as, type)
  if (new_query) {
    table_id <- 2L
  } else {
    table_id <- vctrs::vec_size(x_lq$table_names) + 1L
  }

  multi_join_vars_result <- multi_join_vars(
    x_lq,
    y_lq,
    by_x = by$x,
    by_y = by$y,
    by_on = by$on,
    type = type,
    table_id = table_id,
    suffix = suffix,
    call = caller_env()
  )

  vars <- multi_join_vars_result$vars
  by$x <- multi_join_vars_result$by_x
  by$y <- multi_join_vars_result$by_y
  x_lq <- multi_join_vars_result$x_lq
  y_lq <- multi_join_vars_result$y_lq

  by$on <- by$on %||% NA_character_
  joins <- tibble(
    table = list(y_lq),
    type = type,
    by_x = list(by$x),
    by_y = list(by$y),
    on = by$on,
    na_matches
  )

  x_name <- as.character(query_name(x_lq) %||% NA)
  y_name <- as.character(query_name(y_lq) %||% NA)

  if (new_query) {
    out <- lazy_multi_join_query(
      x = x_lq,
      joins = joins,
      table_names = tibble(
        as = c(x_as, y_as),
        name = c(x_name, y_name)
      ),
      vars = vars,
      group_vars = op_grps(x),
      order_vars = op_sort(x),
      frame = op_frame(x)
    )
    return(out)
  }

  if (!is.na(x_as)) {
    x_lq$table_names$as[[1]] <- x_as
  }

  joins <- vctrs::vec_rbind(
    x_lq$joins,
    tibble(
      table = list(y_lq),
      type = type,
      by_x = list(by$x),
      by_y = list(by$y),
      on = by$on,
      na_matches = na_matches
    )
  )

  table_names <- vctrs::vec_rbind(
    x_lq$table_names,
    tibble(as = y_as, name = y_name)
  )

  lazy_multi_join_query(
    x = x_lq$x,
    joins = joins,
    table_names = table_names,
    vars = vars,
    group_vars = op_grps(x),
    order_vars = op_sort(x),
    frame = op_frame(x)
  )
}

update_join_vars <- function(vars, select) {
  idx <- vctrs::vec_match(select$name, vars)
  prev_vars <- purrr::map_chr(select$expr, as_string)
  vctrs::vec_assign(vars, idx, prev_vars)
}

join_needs_new_query <- function(x_lq, x_as, y_as, type) {
  if (!inherits(x_lq, "lazy_multi_join_query")) {
    return(TRUE)
  }

  if (!type %in% c("left", "inner")) {
    return(TRUE)
  }

  as_current <- x_lq$table_names$as
  if (!is.na(x_as)) {
    x_as_current <- as_current[[1]]
    if (!is.na(x_as_current) && !identical(x_as, x_as_current)) {
      return(TRUE)
    }

    if (join_as_already_used(x_as, as_current[-1])) {
      return(TRUE)
    }
  }

  if (!is.na(y_as)) {
    if (join_as_already_used(y_as, as_current)) {
      return(TRUE)
    }
  }

  FALSE
}

join_as_already_used <- function(as, as_used) {
  if (all(is.na(as_used))) {
    return(FALSE)
  }

  any(as == as_used)
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

  vars <- set_names(op_vars(x))
  group_vars <- op_grps(x)
  order_vars <- op_sort(x)
  frame <- op_frame(x)

  x_lq <- x$lazy_query
  if (is_null(sql_on) && is_lazy_select_query_simple(x_lq, select = "projection")) {
    if (!is_select_identity(x_lq$select, op_vars(x_lq))) {
      by$x <- update_join_vars(by$x, x_lq$select)
      vars <- purrr::map_chr(x_lq$select$expr, as_name)
      vars <- purrr::set_names(vars, x_lq$select$name)
    }

    x_lq <- x_lq$x
  }

  # the table alias can only be determined after `select()` was inlined
  by[c("x_as", "y_as")] <- check_join_as(x_as, x_lq, y_as, y, sql_on = sql_on, type = "semi", call = call)
  table_names <- c(
    unclass(query_name(x_lq)) %||% NA,
    unclass(query_name(y)) %||% NA
  )
  by[c("x_as", "y_as")] <- join_simple_table_alias(table_names, c(by$x_as, by$y_as))
  by$x_as <- ident(by$x_as)
  by$y_as <- ident(by$y_as)

  lazy_semi_join_query(
    x_lq,
    y$lazy_query,
    vars = vars,
    anti = anti,
    by = by,
    na_matches = na_matches,
    group_vars = group_vars,
    order_vars = order_vars,
    frame = frame,
    call = call
  )
}

check_join_as <- function(x_as, x, y_as, y, sql_on, type, call) {
  if (!is_null(x_as)) {
    vctrs::vec_assert(x_as, character(), size = 1, arg = "x_as", call = call)
  }
  if (!is_null(y_as)) {
    vctrs::vec_assert(y_as, character(), size = 1, arg = "y_as", call = call)
  }

  if (is_null(sql_on)) {
    x_default_name <- NA_character_
    y_default_name <- NA_character_
  } else {
    # for backwards compatibility use "LHS" and "RHS" if `sql_on` is used
    # without a table alias
    x_as <- x_as %||% "LHS"
    y_as <- y_as %||% "RHS"
  }


  x_as <- x_as %||% NA_character_
  y_as <- y_as %||% NA_character_

  if (identical(x_as, y_as) && !is.na(x_as)) {
    cli_abort("{.arg y_as} must be different from {.arg x_as}.", call = call)
  }

  list(x_as = x_as, y_as = y_as)
}

join_simple_table_alias <- function(table_names, aliases) {
  stopifnot(length(table_names) == 2)
  stopifnot(length(aliases) == 2)

  out <- dplyr::coalesce(aliases, table_names)

  # if only joining two tables it is nicer to use `<tbl>_LHS`/`<tbl>_RHS`
  if (identical(out[[1]], out[[2]]) && !is.na(out[[1]])) {
    out <- paste0(out, c("_LHS", "_RHS"))
  } else {
    out <- dplyr::coalesce(out, c("LHS", "RHS"))
  }

  out
}

multi_join_vars <- function(x_lq,
                            y_lq,
                            by_x,
                            by_y,
                            by_on,
                            type,
                            table_id,
                            suffix = c(".x", ".y"),
                            call = caller_env()) {
  by_x_org <- by_x
  x_vars <- op_vars(x_lq)
  if (identical(table_id, 2L)) {
    if (is_empty(by_on) && is_lazy_select_query_simple(x_lq, select = "projection")) {
      prev_vars <- purrr::map_chr(x_lq$select$expr, as_string)
      x_rows <- tibble(
        name = x_vars,
        table = rep_along(x_vars, list(1L)),
        var = as.list(prev_vars)
      )

      idx <- vctrs::vec_match(x_lq$select$name, by_x)
      by_x <- vctrs::vec_assign(by_x, idx, prev_vars)
      x_lq <- x_lq$x
    } else {
      x_rows <- tibble(
        name = x_vars,
        table = rep_along(x_vars, list(1L)),
        var = as.list(x_vars)
      )
    }
  } else {
    x_rows <- x_lq$vars
  }

  y_names <- op_vars(y_lq)
  if (is_empty(by_on) && is_lazy_select_query_simple(y_lq, select = "projection")) {
    y_vars <- purrr::map_chr(y_lq$select$expr, as_string)

    idx <- vctrs::vec_match(y_lq$select$name, by_y)
    by_y <- vctrs::vec_assign(by_y, idx, y_vars)
    y_lq <- y_lq$x
  } else {
    y_vars <- y_names
  }

  # Remove join keys from y
  x_names <- x_vars
  y_join_idx <- vctrs::vec_match(by_y, y_vars)
  if (!is_empty(y_join_idx)) {
    y_names <- y_names[-y_join_idx]
    y_vars <- y_vars[-y_join_idx]
  }

  # Add suffix where needed
  suffix <- check_suffix(suffix, call)
  x_new <- add_suffixes(x_names, y_names, suffix$x)
  y_new <- add_suffixes(y_names, x_names, suffix$y)

  x_rows$name <- x_new
  y_rows <- tibble(
    name = y_new,
    table = list(table_id),
    var = as.list(y_vars)
  )

  if (type %in% c("left", "inner")) {
    # use all variables from `x` as is
    # use non-join variables from `y`
  } else if (type == "right") {
    # Careful: this relies on the assumption that right_join()` starts a new query
    # `x`: non-join variables; `y`: all variables
    # -> must update table id of `x` join vars
    x_rows$table[x_rows$name %in% by_x_org] <- list(table_id)
    idx <- vctrs::vec_match(by_x_org, x_rows$name)
    x_rows$var[idx] <- as.list(by_y)
  } else if (type == "full") {
    # Careful: this relies on the assumption that `full_join()` starts a new query
    idx <- vctrs::vec_match(by_x_org, x_rows$name)
    x_rows$table[idx] <- purrr::map(
      x_rows$table[idx],
      ~ c(.x, table_id)
    )
    x_rows$var[idx] <- purrr::map2(
      x_rows$var[idx], by_y,
      ~ c(.x, .y)
    )
  } else if (type == "cross") {
    # -> simply append `y_rows`
  }

  list(
    vars = vctrs::vec_rbind(x_rows, y_rows),
    by_x = by_x,
    by_y = by_y,
    x_lq = x_lq,
    y_lq = y_lq
  )
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
