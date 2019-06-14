#' Join sql tbls.
#'
#' See [join] for a description of the general purpose of the
#' functions.
#'
#' @section Implementation notes:
#'
#' Semi-joins are implemented using `WHERE EXISTS`, and anti-joins with
#' `WHERE NOT EXISTS`.
#'
#' All joins use column equality by default.
#' An arbitrary join predicate can be specified by passing
#' an SQL expression to the `sql_on` argument.
#' Use `LHS` and `RHS` to refer to the left-hand side or
#' right-hand side table, respectively.
#'
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
#' @param sql_on A custom join predicate as an SQL expression. The SQL
#'   can refer to the `LHS` and `RHS` aliases to disambiguate
#'   column names.
#' @examples
#' \dontrun{
#' library(dplyr)
#' if (has_lahman("sqlite")) {
#'
#' # Left joins ----------------------------------------------------------------
#' lahman_s <- lahman_sqlite()
#' batting <- tbl(lahman_s, "Batting")
#' team_info <- select(tbl(lahman_s, "Teams"), yearID, lgID, teamID, G, R:H)
#'
#' # Combine player and whole team statistics
#' first_stint <- select(filter(batting, stint == 1), playerID:H)
#' both <- left_join(first_stint, team_info, type = "inner", by = c("yearID", "teamID", "lgID"))
#' head(both)
#' explain(both)
#'
#' # Join with a local data frame
#' grid <- expand.grid(
#'   teamID = c("WAS", "ATL", "PHI", "NYA"),
#'   yearID = 2010:2012)
#' top4a <- left_join(batting, grid, copy = TRUE)
#' explain(top4a)
#'
#' # Indices don't really help here because there's no matching index on
#' # batting
#' top4b <- left_join(batting, grid, copy = TRUE, auto_index = TRUE)
#' explain(top4b)
#'
#' # Semi-joins ----------------------------------------------------------------
#'
#' people <- tbl(lahman_s, "Master")
#'
#' # All people in half of fame
#' hof <- tbl(lahman_s, "HallOfFame")
#' semi_join(people, hof)
#'
#' # All people not in the hall of fame
#' anti_join(people, hof)
#'
#' # Find all managers
#' manager <- tbl(lahman_s, "Managers")
#' semi_join(people, manager)
#'
#' # Find all managers in hall of fame
#' famous_manager <- semi_join(semi_join(people, manager), hof)
#' famous_manager
#' explain(famous_manager)
#'
#' # Anti-joins ----------------------------------------------------------------
#'
#' # batters without person covariates
#' anti_join(batting, people)
#'
#' # Arbitrary predicates ------------------------------------------------------
#'
#' # Find all pairs of awards given to the same player
#' # with at least 18 years between the awards:
#' awards_players <- tbl(lahman_s, "AwardsPlayers")
#' inner_join(
#'   awards_players, awards_players,
#'   sql_on = paste0(
#'     "(LHS.playerID = RHS.playerID) AND ",
#'     "(LHS.yearID < RHS.yearID - 18)"
#'   )
#' )
#' }
#' }
#' @name join.tbl_sql
NULL

#' @rdname join.tbl_sql
#' @export
inner_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                                suffix = c(".x", ".y"),
                                auto_index = FALSE, ...,
                                sql_on = NULL) {

  add_op_join(
    x, y,
    "inner",
    by = by,
    sql_on = sql_on,
    copy = copy,
    suffix = suffix,
    auto_index = auto_index,
    ...
  )
}

#' @rdname join.tbl_sql
#' @export
left_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                               suffix = c(".x", ".y"),
                               auto_index = FALSE, ...,
                               sql_on = NULL) {

  add_op_join(
    x, y,
    "left",
    by = by,
    sql_on = sql_on,
    copy = copy,
    suffix = suffix,
    auto_index = auto_index,
    ...
  )
}

#' @rdname join.tbl_sql
#' @export
right_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                                suffix = c(".x", ".y"),
                                auto_index = FALSE, ...,
                                sql_on = NULL) {

  add_op_join(
    x, y,
    "right",
    by = by,
    sql_on = sql_on,
    copy = copy,
    suffix = suffix,
    auto_index = auto_index,
    ...
  )
}

#' @rdname join.tbl_sql
#' @export
full_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                               suffix = c(".x", ".y"),
                               auto_index = FALSE, ...,
                               sql_on = NULL) {

  add_op_join(
    x, y,
    "full",
    by = by,
    sql_on = sql_on,
    copy = copy,
    suffix = suffix,
    auto_index = auto_index,
    ...
  )
}

#' @rdname join.tbl_sql
#' @export
semi_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                               auto_index = FALSE, ...,
                               sql_on = NULL) {

  add_op_semi_join(
    x, y,
    anti = FALSE,
    by = by,
    sql_on = sql_on,
    copy = copy,
    auto_index = auto_index,
    ...
  )
}

#' @rdname join.tbl_sql
#' @export
anti_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                               auto_index = FALSE, ...,
                               sql_on = NULL) {

  add_op_semi_join(
    x, y,
    anti = TRUE,
    by = by,
    sql_on = sql_on,
    copy = copy,
    auto_index = auto_index,
    ...
  )
}


add_op_join <- function(x, y, type, by = NULL, sql_on = NULL, copy = FALSE,
                        suffix = c(".x", ".y"),
                        auto_index = FALSE, ...) {

  if (!is.null(sql_on)) {
    by <- list(x = character(0), y = character(0), on = sql(sql_on))
  } else if (identical(type, "full") && identical(by, character())) {
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

add_op_semi_join <- function(x, y, anti = FALSE, by = NULL, sql_on = NULL, copy = FALSE,
                             auto_index = FALSE, ...) {
  if (!is.null(sql_on)) {
    by <- list(x = character(0), y = character(0), on = sql(sql_on))
  } else {
    by <- common_by(by, x, y)
  }

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


#' @export
op_vars.op_join <- function(op) {
  op$args$vars$alias
}
#' @export
op_vars.op_semi_join <- function(op) {
  op_vars(op$x)
}

#' @export
sql_build.op_join <- function(op, con, ...) {
  join_query(
    op$x, op$y,
    vars = op$args$vars,
    type = op$args$type,
    by = op$args$by,
    suffix = op$args$suffix
  )
}

#' @export
sql_build.op_semi_join <- function(op, con, ...) {
  semi_join_query(op$x, op$y, anti = op$args$anti, by = op$args$by)
}
