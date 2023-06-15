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
#' @param multiple,unmatched Unsupported in database backends. As a workaround
#'   for multiple use a unique key and for unmatched a foreign key constraint.
#' @param relationship Unsupported in database backends.
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
inner_join.tbl_lazy <- function(x,
                                y,
                                by = NULL,
                                copy = FALSE,
                                suffix = NULL,
                                ...,
                                keep = NULL,
                                na_matches = c("never", "na"),
                                multiple = NULL,
                                unmatched = "drop",
                                relationship = NULL,
                                sql_on = NULL,
                                auto_index = FALSE,
                                x_as = NULL,
                                y_as = NULL) {
  check_dots_empty()
  x$lazy_query <- add_join(
    x,
    y,
    type = "inner",
    by = by,
    copy = copy,
    suffix = suffix,
    keep = keep,
    na_matches = na_matches,
    multiple = multiple,
    unmatched = unmatched,
    relationship = relationship,
    sql_on = sql_on,
    auto_index = auto_index,
    x_as = x_as,
    y_as = y_as
  )

  x
}

#' @rdname join.tbl_sql
#' @export
#' @importFrom dplyr left_join
left_join.tbl_lazy <- function(x,
                               y,
                               by = NULL,
                               copy = FALSE,
                               suffix = NULL,
                               ...,
                               keep = NULL,
                               na_matches = c("never", "na"),
                               multiple = NULL,
                               unmatched = "drop",
                               relationship = NULL,
                               sql_on = NULL,
                               auto_index = FALSE,
                               x_as = NULL,
                               y_as = NULL) {
  check_dots_empty()
  x$lazy_query <- add_join(
    x,
    y,
    type = "left",
    by = by,
    copy = copy,
    suffix = suffix,
    keep = keep,
    na_matches = na_matches,
    multiple = multiple,
    unmatched = unmatched,
    relationship = relationship,
    sql_on = sql_on,
    auto_index = auto_index,
    x_as = x_as,
    y_as = y_as
  )

  x
}

#' @rdname join.tbl_sql
#' @export
#' @importFrom dplyr right_join
right_join.tbl_lazy <- function(x,
                                y,
                                by = NULL,
                                copy = FALSE,
                                suffix = NULL,
                                ...,
                                keep = NULL,
                                na_matches = c("never", "na"),
                                multiple = NULL,
                                unmatched = "drop",
                                relationship = NULL,
                                sql_on = NULL,
                                auto_index = FALSE,
                                x_as = NULL,
                                y_as = NULL) {
  check_dots_empty()
  x$lazy_query <- add_join(
    x,
    y,
    type = "right",
    by = by,
    copy = copy,
    suffix = suffix,
    keep = keep,
    na_matches = na_matches,
    multiple = multiple,
    unmatched = unmatched,
    relationship = relationship,
    sql_on = sql_on,
    auto_index = auto_index,
    x_as = x_as,
    y_as = y_as
  )

  x
}

#' @rdname join.tbl_sql
#' @export
#' @importFrom dplyr full_join
full_join.tbl_lazy <- function(x,
                               y,
                               by = NULL,
                               copy = FALSE,
                               suffix = NULL,
                               ...,
                               keep = NULL,
                               na_matches = c("never", "na"),
                               multiple = NULL,
                               relationship = NULL,
                               sql_on = NULL,
                               auto_index = FALSE,
                               x_as = NULL,
                               y_as = NULL) {
  check_dots_empty()
  x$lazy_query <- add_join(
    x,
    y,
    type = "full",
    by = by,
    copy = copy,
    suffix = suffix,
    keep = keep,
    na_matches = na_matches,
    multiple = multiple,
    relationship = relationship,
    sql_on = sql_on,
    auto_index = auto_index,
    x_as = x_as,
    y_as = y_as
  )

  x
}

#' @rdname join.tbl_sql
#' @export
#' @importFrom dplyr cross_join
cross_join.tbl_lazy <- function(x,
                                y,
                                ...,
                                copy = FALSE,
                                suffix = c(".x", ".y"),
                                x_as = NULL,
                                y_as = NULL) {
  x$lazy_query <- add_join(
    x, y,
    "cross",
    by = character(),
    sql_on = NULL,
    copy = copy,
    suffix = suffix,
    auto_index = FALSE,
    keep = NULL,
    na_matches = "never",
    x_as = x_as,
    y_as = y_as,
    ...
  )

  x
}

#' @rdname join.tbl_sql
#' @export
#' @importFrom dplyr semi_join
semi_join.tbl_lazy <- function(x,
                               y,
                               by = NULL,
                               copy = FALSE,
                               ...,
                               na_matches = c("never", "na"),
                               sql_on = NULL,
                               auto_index = FALSE,
                               x_as = NULL,
                               y_as = NULL) {
  check_dots_empty()
  x$lazy_query <- add_semi_join(
    x,
    y,
    anti = FALSE,
    by = by,
    copy = copy,
    na_matches = na_matches,
    sql_on = sql_on,
    auto_index = auto_index,
    x_as = x_as,
    y_as = y_as
  )

  x
}

#' @rdname join.tbl_sql
#' @export
#' @importFrom dplyr anti_join
anti_join.tbl_lazy <- function(x,
                               y,
                               by = NULL,
                               copy = FALSE,
                               ...,
                               na_matches = c("never", "na"),
                               sql_on = NULL,
                               auto_index = FALSE,
                               x_as = NULL,
                               y_as = NULL) {
  check_dots_empty()
  x$lazy_query <- add_semi_join(
    x,
    y,
    anti = TRUE,
    by = by,
    copy = copy,
    na_matches = na_matches,
    sql_on = sql_on,
    auto_index = auto_index,
    x_as = x_as,
    y_as = y_as
  )

  x
}

add_join <- function(x,
                     y,
                     type,
                     by = NULL,
                     copy = FALSE,
                     suffix = NULL,
                     keep = NULL,
                     na_matches = "never",
                     multiple = NULL,
                     unmatched = "drop",
                     relationship = NULL,
                     sql_on = NULL,
                     auto_index = FALSE,
                     x_as = NULL,
                     y_as = NULL,
                     call = caller_env()) {
  x_names <- tbl_vars(x)
  y_names <- tbl_vars(y)

  by <- join_prepare_by(
    by = by,
    sql_on = sql_on,
    type = type,
    x_names = x_names,
    y_names = y_names,
    error_call = call
  )
  type <- by$type

  check_join_by_supported(by, call = call)

  check_join_multiple(multiple, by, call = call)
  check_join_unmatched(unmatched, by, call = call)
  check_join_relationship(relationship, call = call)

  y <- auto_copy(
    x, y,
    copy = copy,
    indexes = if (auto_index) list(by$y)
  )

  suffix <- suffix %||% sql_join_suffix(x$src$con, suffix)
  vars <- join_cols(
    x_names = x_names,
    y_names = y_names,
    by = by,
    suffix = suffix,
    keep = keep,
    error_call = call
  )

  na_matches <- arg_match(na_matches, c("na", "never"), error_call = call)

  # the table alias can only be determined after `select()` was inlined.
  # This works even though `by` is used in `join_inline_select()` and updated
  # because this does not touch `by$x_as` and `by$y_as`.
  join_alias <- make_join_aliases(x_as, y_as, sql_on, call)

  inline_result <- join_inline_select(x$lazy_query, by$x, by$on)
  x_lq <- inline_result$lq
  x_vars <- inline_result$vars
  by$x <- inline_result$by

  inline_result <- join_inline_select(y$lazy_query, by$y, by$on)
  y_lq <- inline_result$lq
  y_vars <- inline_result$vars
  by$y <- inline_result$by

  if (type %in% c("full", "right")) {
    vars <- rf_join_vars(
      x_vars = x_vars,
      y_vars = y_vars,
      vars_info = vars,
      keep = keep,
      condition = by$condition,
      by_y = by$y,
      type = type
    )

    table_names_x <- make_table_names(join_alias$x, x_lq)
    table_names_y <- make_table_names(join_alias$y, y_lq)
    out <- lazy_rf_join_query(
      x = x_lq,
      y = y_lq,
      type = type,
      by = list(
        x = by$x,
        y = by$y,
        condition = by$condition,
        on = sql(by$on),
        na_matches = na_matches
      ),
      table_names = vctrs::vec_rbind(table_names_x, table_names_y),
      vars = vars
    )

    return(out)
  }

  new_query <- join_needs_new_query(x$lazy_query, join_alias, type)
  vars <- multi_join_vars(
    x_lq = x_lq,
    x_vars = x_vars,
    y_vars = y_vars,
    new_query = new_query,
    vars_info = vars,
    error_call = call
  )

  joins_data <- new_joins_data(
    x_lq,
    y_lq,
    new_query = new_query,
    type = type,
    by = by,
    na_matches = na_matches
  )

  table_names_y <- make_table_names(join_alias$y, y_lq)

  if (new_query) {
    table_names_x <- make_table_names(join_alias$x, x_lq)
    out <- lazy_multi_join_query(
      x = x_lq,
      joins = joins_data,
      table_names = vctrs::vec_rbind(table_names_x, table_names_y),
      vars = vars
    )
    return(out)
  }

  # `x_lq` must be a `lazy_multi_join_query` so it can be modified directly
  if (!is_null(join_alias$x)) {
    x_lq$table_names$name[[1]] <- join_alias$x
    x_lq$table_names$from[[1]] <- "as"
  }

  x_lq$joins <- vctrs::vec_rbind(x_lq$joins, joins_data)
  x_lq$table_names <- vctrs::vec_rbind(x_lq$table_names, table_names_y)
  x_lq$vars <- vars

  x_lq
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

join_needs_new_query <- function(x_lq, join_alias, type) {
  if (!inherits(x_lq, "lazy_multi_join_query")) {
    return(TRUE)
  }

  x_as <- join_alias$x
  y_as <- join_alias$y

  names <- x_lq$table_names$name
  from <- x_lq$table_names$from
  if (!is_null(x_as)) {
    if (from[[1]] == "as" && !identical(x_as, names[[1]])) {
      return(TRUE)
    }

    if (x_as %in% names[-1][from[-1] == "as"]) {
      return(TRUE)
    }
  }

  if (!is_null(y_as)) {
    if (y_as %in% names[from == "as"]) {
      return(TRUE)
    }
  }

  FALSE
}

multi_join_vars <- function(x_lq,
                            x_vars,
                            y_vars,
                            new_query,
                            vars_info,
                            error_call) {
  if (new_query) {
    x_join_vars <- tibble(
      name = names(vars_info$x$out),
      table = 1L,
      var = vctrs::vec_slice(x_vars, vars_info$x$out)
    )
    table_id <- 2L
  } else {
    x_join_vars <- vctrs::vec_slice(x_lq$vars, vars_info$x$out)
    x_join_vars$name <- names(vars_info$x$out)
    table_id <- vctrs::vec_size(x_lq$table_names) + 1L
  }

  y_join_vars <- tibble(
    name = names(vars_info$y$out),
    table = table_id,
    var = vctrs::vec_slice(y_vars, vars_info$y$out)
  )

  vctrs::vec_rbind(x_join_vars, y_join_vars, .error_call = error_call)
}

rf_join_vars <- function(x_vars,
                         y_vars,
                         vars_info,
                         keep,
                         condition,
                         by_y,
                         type,
                         call) {
  x_join_vars <- tibble(
    name = names(vars_info$x$out),
    x = vctrs::vec_slice(x_vars, vars_info$x$out),
    y = NA_character_
  )

  y_join_vars <- tibble(
    name = names(vars_info$y$out),
    x = NA_character_,
    y = vctrs::vec_slice(y_vars, vars_info$y$out)
  )

  keep <- keep %||% (condition != "==")
  idx <- vars_info$x$key[!keep]
  if (type == "right") {
    # `x`: non-join variables; `y`: all variables
    x_join_vars$x[idx] <- NA
  }
  x_join_vars$y[idx] <- by_y[!keep]

  vctrs::vec_rbind(x_join_vars, y_join_vars)
}


join_prepare_by <- function(by,
                            sql_on,
                            type,
                            x_names,
                            y_names,
                            error_call,
                            env = caller_env(2),
                            user_env = caller_env(3)) {
  if (identical(by, character()) && is.null(sql_on)) {
    if (type != "cross") {
      lifecycle::deprecate_soft(
        when = "1.1.0",
        what = I("Using `by = character()` to perform a cross join"),
        with = "cross_join()",
        env = env,
        user_env = user_env
      )
    }
    type <- "cross"
  }

  if (!is.null(sql_on)) {
    by <- list(x = character(0), y = character(0), on = unclass(sql_on))
  } else if (type == "cross") {
    by <- list(x = character(0), y = character(0))
  } else if (is_null(by)) {
    by <- join_by_common(x_names, y_names, error_call = error_call)
  } else {
    by <- dbplyr_as_join_by(by, error_call = error_call)
  }

  by$type <- type
  by
}

new_joins_data <- function(x_lq, y_lq, new_query, type, by, na_matches) {
  if (new_query) {
    by_x_table_id <- rep_along(by$x, 1L)
  } else {
    idx <- vctrs::vec_match(by$x, x_lq$vars$name)

    # need to fix `by$x` in case it was renamed in an inlined select
    by$x <- x_lq$vars$var[idx]
    by_x_table_id <- x_lq$vars$table[idx]
  }

  tibble(
    table = list(y_lq),
    type = type,
    by_x_table_id = list(by_x_table_id),
    by = list(list(
      x = by$x,
      y = by$y,
      condition = by$condition,
      on = sql(by$on),
      na_matches = na_matches
    ))
  )
}

add_semi_join <- function(x,
                          y,
                          anti = FALSE,
                          by = NULL,
                          sql_on = NULL,
                          copy = FALSE,
                          auto_index = FALSE,
                          na_matches = "never",
                          x_as = NULL,
                          y_as = NULL,
                          call = caller_env()) {
  x_names <- tbl_vars(x)
  y_names <- tbl_vars(y)

  if (!is.null(sql_on)) {
    by <- list(x = character(0), y = character(0), on = sql(sql_on))
  } else if (is_null(by)) {
    by <- join_by_common(x_names, y_names, error_call = call)
  } else {
    by <- dbplyr_as_join_by(by, error_call = call)
  }

  check_join_by_supported(by, call = call)
  na_matches <- arg_match(na_matches, c("na", "never"), error_call = call)

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
  by$na_matches <- na_matches

  # the table alias can only be determined after `select()` was inlined
  join_alias <- make_join_aliases(x_as, y_as, sql_on, call)

  x_alias <- make_table_names(join_alias$x, x_lq)
  y_alias <- make_table_names(join_alias$y, y)
  by[c("x_as", "y_as")] <- join_two_table_alias(
    c(x_alias$name, y_alias$name),
    c(x_alias$from, y_alias$from)
  )

  lazy_semi_join_query(
    x_lq,
    y$lazy_query,
    vars = vars,
    anti = anti,
    by = by,
    call = call
  )
}

make_join_aliases <- function(x_as, y_as, sql_on, call) {
  x_as <- check_join_as1(x_as, arg = "x_as", sql_on, default = "LHS", call)
  y_as <- check_join_as1(y_as, arg = "y_as", sql_on, default = "RHS", call)

  if (identical(x_as, y_as) && !is.null(x_as)) {
    cli_abort("{.arg y_as} must be different from {.arg x_as}.", call = call)
  }

  list(x = x_as, y = y_as)
}

make_table_names <- function(as, lq) {
  name <- unclass(remote_name(lq, null_if_local = FALSE))

  if (!is.null(as)) {
    tibble(name = as, from = "as")
  } else if (!is.null(name)) {
    tibble(name = name, from = "name")
  } else {
    tibble(name = NA_character_, from = "")
  }
}

check_join_as1 <- function(as, arg, sql_on, default, call) {
  check_string(as, allow_null = TRUE, arg = arg, call = call)

  if (!is_null(sql_on)) {
    # for backwards compatibility use "LHS"/"RHS" if `sql_on` is used
    # without a table alias
    as <- as %||% default
  }

  as
}

join_vars <- function(x_names, y_names, type, by, suffix = c(".x", ".y"), call = caller_env()) {
  y_names_org <- y_names
  # Remove join keys from y
  y_names <- setdiff(y_names, by$y)

  # Add suffix where needed
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

join_two_table_alias <- function(names, from) {
  check_character(names)
  check_character(from)
  vctrs::vec_assert(names, size = 2L)

  out <- dplyr::coalesce(names, c("LHS", "RHS"))

  if (!identical(out[1], out[2])) {
    return(out)
  }
  # -> must rename

  if (from[1] != "as" && from[2] != "as") {
    # self join of named table
    if (!is.na(names[1]) && !is.na(names[2]) && identical(names[1], names[2])) {
      out <- c(
        paste0(names[1], "_LHS"),
        paste0(names[2], "_RHS")
      )
      return(out)
    }
  }

  out_repaired <- vctrs::vec_as_names(out, repair = "unique", quiet = TRUE)
  may_repair <- c(from[1] != "as", from[2] != "as")
  out[may_repair] <- out_repaired[may_repair]

  out
}

check_join_by_supported <- function(by, call = caller_env()) {
  if (any(by$filter != "none")) {
    cli_abort("Rolling joins aren't supported on database backends.", call = call)
  }
}

check_join_multiple <- function(multiple, by, call = caller_env()) {
  if (is.null(multiple) || identical(multiple, "all")) {
    return()
  }

  msg <- "Argument {.arg multiple} isn't supported on database backends."

  if (all(by$condition == "==")) {
    code <- glue::glue("
      db_create_index(
        con = remote_con(y),
        table = remote_name(y),
        columns = {deparse(by$y)},
        unique = TRUE
      )"
    )

    msg <- c(
      msg,
      i = c(
        "For equi joins you can instead add a unique index for the join columns in {.arg y}.",
        "{.code {code}}"
      )
    )
  }

  cli_abort(msg, call = call)
}

check_join_unmatched <- function(unmatched, by, call = caller_env()) {
  if (is.null(unmatched) || identical(unmatched, "drop")) {
    return()
  }

  cli_abort(c(
    "Argument {.arg unmatched} isn't supported on database backends.",
    i = "For equi joins you can instead add a foreign key from {.arg x} to {.arg y} for the join columns."
  ), call = call)
}

check_join_relationship <- function(relationship, call = caller_env()) {
  if (is.null(relationship) || identical(relationship, "many-to-many")) {
    return()
  }

  cli_abort("Argument {.arg relationship} isn't supported on database backends.", call = call)
}
