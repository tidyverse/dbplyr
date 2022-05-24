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

  if (!is_null(x_as)) {
    if (identical(x_as, y_as)) {
      cli_abort("{.arg y_as} must be different from {.arg x_as}.", call = call)
    }

    vctrs::vec_assert(x_as, character(), size = 1, arg = "x_as", call = call)
  }

  if (!is_null(y_as)) {
    vctrs::vec_assert(y_as, character(), size = 1, arg = "y_as", call = call)
  }

  na_matches <- arg_match(na_matches, c("na", "never"), error_call = call)
  suffix <- suffix %||% sql_join_suffix(x$src$con, suffix)

  x_lq <- x$lazy_query
  y_lq <- y$lazy_query

  # type not "left" or "inner" -> use classical join
  vars <- join_vars(op_vars(x_lq), op_vars(y_lq), type = type, by = by, suffix = suffix, call = caller_env())
  if (!type %in% c("left", "inner")) {
    # TODO refactor this
    by[c("x_as", "y_as")] <- check_join_as(x_as, x, y_as, y, sql_on = sql_on, call = call)
    vars2 <- list(
      alias = colnames(vars),
      x = as.character(vars[1, , ]),
      y = as.character(vars[2, , ]),
      all_x = op_vars(x_lq),
      all_y = op_vars(y_lq)
    )

    out <- lazy_join_query(
      x_lq,
      y_lq,
      vars = vars2,
      type = type,
      by = by,
      suffix = suffix,
      na_matches = na_matches,
      call = call
    )

    return(out)
  }

  by$on <- by$on %||% NA_character_
  if (!is_null(sql_on)) {
    x_as <- x_as %||% "LHS"
    y_as <- y_as %||% "RHS"
  } else {
    x_as <- x_as %||% NA
    y_as <- y_as %||% NA
  }
  x_name <- as.character(query_name(x) %||% NA)
  y_name <- as.character(query_name(y) %||% NA)

  meta <- tibble(
    alias = tibble(
      as = c(x_as, y_as),
      name = c(x_name, y_name)
    ),
    vars = vars
  )

  joins <- tibble(
    table = list(y_lq),
    type,
    by_x = list(by$x),
    by_y = list(by$y),
    on = by$on,
    na_matches
  )

  out <- lazy_multi_join_query(
    x = x_lq,
    joins = joins,
    meta = meta
  )

  if (!inherits(x$lazy_query, "lazy_multi_join_query")) {
    return(out)
  }

  # `x` has a name and should get a different one -> start new join
  as_current <- x_lq$meta$alias$as
  if (!is.na(x_as)) {
    x_as_current <- as_current[[1]]
    if (!is.na(x_as_current) && !identical(x_as, x_as_current)) {
      return(out)
    }

    if (!all(is.na(as_current)) && any(x_as == as_current[-1])) {
      return(out)
    }

    x_lq$meta$alias$as[[1]] <- x_as
  }

  # `y_as` is already used as name
  if (!is.na(y_as)) {
    if (!all(is.na(as_current)) && any(y_as == as_current)) {
      return(out)
    }
  }

  joins <- vctrs::vec_rbind(
    x_lq$joins,
    tibble(table = list(y_lq), type, by_x = list(by$x), by_y = list(by$y), on = by$on, na_matches)
  )

  # TODO need to adapt vars!
  vars <- join_vars2(x_lq$meta$vars, op_vars(y_lq), type = type, by = by, suffix = suffix, call = caller_env())
  out_alias <- vctrs::vec_rbind(
    x_lq$meta$alias,
    tibble(as = y_as, name = y_name)
  )

  lazy_multi_join_query(
    x = x_lq$x,
    joins = joins,
    meta = tibble(
      alias = out_alias,
      vars = vars
    )
  )
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

  by[c("x_as", "y_as")] <- check_join_as(x_as, x, y_as, y, sql_on = sql_on, call = call)

  lazy_semi_join_query(
    x$lazy_query,
    y$lazy_query,
    anti = anti,
    by = by,
    na_matches = na_matches,
    call = call
  )
}

check_join_as <- function(x_as, x, y_as, y, sql_on, call) {
  x_name <- query_name(x)
  y_name <- query_name(y)
  if (is_null(x_as) && is_null(y_as)) {
    if (identical(x_name, y_name)) {
      return(list(x_as = ident("LHS"), y_as = ident("RHS")))
    }
  }

  x_as <- check_join_as1(x_as, x_name, sql_on, "LHS", arg = "x_as", call = call)
  y_as <- check_join_as1(y_as, y_name, sql_on, "RHS", arg = "y_as", call = call)

  if (identical(x_as, y_as)) {
    cli_abort("{.arg y_as} must be different from {.arg x_as}.", call = call)
  }

  list(x_as = x_as, y_as = y_as)
}

check_join_as1 <- function(as, tbl_name, sql_on, default, arg, call) {
  if (!is_null(as)) {
    vctrs::vec_assert(as, character(), size = 1, arg = arg, call = call)
    return(ident(as))
  }

  if (!is_null(sql_on)) {
    return(ident(default))
  }

  tbl_name %||% ident(default)
}

join_vars <- function(x_names, y_names, type, by, suffix = c(".x", ".y"), call = caller_env()) {
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
  alias <- c(x_new, y_new)

  dplyr::bind_rows(
    set_names(c(x_x, y_x), alias),
    set_names(c(x_y, y_y), alias)
  )
}

join_vars2 <- function(vars, y_names, type, by, suffix = c(".x", ".y"), call = caller_env()) {
  # TODO what if `by$on` is not NA?
  vars_out <- vars

  # Remove join keys from y
  x_names <- names(vars)
  y_names <- setdiff(y_names, by$y)

  # Add suffix where needed
  suffix <- check_suffix(suffix, call)
  x_new <- add_suffixes(x_names, y_names, suffix$x)
  y_new <- add_suffixes(y_names, x_names, suffix$y)
  vars_out <- vars_out %>%
    rename(!!!set_names(x_names, x_new))
  n <- vctrs::vec_size(vars_out)
  vars_out <- vctrs::vec_cbind(vars_out, !!!rep_named(y_new, list(rep_len(NA_character_, n))))

  # In left and inner joins, return key values only from x
  # In right joins, return key values only from y
  # In full joins, return key values by coalescing values from x and y
  x_y <- by$y[match(x_names, by$x)]
  x_y[type == "left" | type == "inner"] <- NA
  if (type == "right") {
    vars_out[-(1:2)] <- purrr::map(
      vars_out[-(1:2)],
      ~ {
        .x[!is.na(x_y)] <- NA
        .x
      }
    )
  }
  y_y <- y_names
  new_row <- set_names(as.list(c(x_y, y_y)), c(x_new, y_new))
  vctrs::vec_rbind(vars_out, vctrs::new_data_frame(new_row))
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
