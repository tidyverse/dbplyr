#' Create an SQL tbl (abstract)
#'
#' Generally, you should no longer need to provide a custom `tbl()`
#' method you you can default `tbl.DBIConnect` method.
#'
#' @keywords internal
#' @export
#' @param subclass name of subclass
#' @param ... needed for agreement with generic. Not otherwise used.
#' @param vars If known, the names of the variables in the tbl. This is
#'   relatively expensive to determine automatically, so is cached throughout
#'   dplyr. However, you should usually be able to leave this blank and it
#'   will be determined from the context.
tbl_sql <- function(subclass, src, from, ..., vars = NULL) {
  # If not literal sql, must be a table identifier
  from <- as.sql(from)

  vars <- db_query_fields(src$con, from)
  ops <- op_base_remote(from, vars)

  make_tbl(c(subclass, "sql", "lazy"), src = src, ops = ops)
}

#' @export
same_src.tbl_sql <- function(x, y) {
  if (!inherits(y, "tbl_sql")) return(FALSE)
  same_src(x$src, y$src)
}

# Grouping methods -------------------------------------------------------------

#' @export
group_size.tbl_sql <- function(x) {
  df <- x %>%
    summarise(n = n()) %>%
    collect()
  df$n
}

#' @export
n_groups.tbl_sql <- function(x) {
  if (length(groups(x)) == 0) return(1L)

  df <- x %>%
    summarise() %>%
    ungroup() %>%
    summarise(n = n()) %>%
    collect()
  df$n
}

# Standard data frame methods --------------------------------------------------

#' @export
as.data.frame.tbl_sql <- function(x, row.names = NULL, optional = NULL,
                                  ..., n = Inf) {
  as.data.frame(collect(x, n = n))
}

#' @export
tbl_sum.tbl_sql <- function(x) {
  grps <- op_grps(x$ops)
  sort <- op_sort(x$ops)
  c(
    "Source" = tbl_desc(x),
    "Database" = db_desc(x$src$con),
    if (length(grps) > 0) c("Groups" = commas(grps)),
    if (length(sort) > 0) c("Ordered by" = commas(deparse_all(sort)))
  )
}

#' @export
glimpse.tbl_sql <- function(.data, width = NULL, n = 25, ...) {
  .data <- collect(head(.data, n = n))
  glimpse(.data, width = width, ...)
}

#' @export
pull.tbl_sql <- function(.data, var = -1) {
  expr <- enquo(var)
  var <- dplyr:::find_var(expr, tbl_vars(.data))

  .data <- select(.data, !! sym(var))
  .data <- collect(.data)
  .data[[1]]
}

#' @export
dimnames.tbl_sql <- function(x) {
  list(NULL, op_vars(x$ops))
}

#' @export
dim.tbl_sql <- function(x) {
  c(NA, length(op_vars(x$ops)))
}

#' @export
tail.tbl_sql <- function(x, n = 6L, ...) {
  stop("tail() is not supported by sql sources", call. = FALSE)
}

# Joins ------------------------------------------------------------------------

#' Join sql tbls.
#'
#' See [join] for a description of the general purpose of the
#' functions.
#'
#' @section Implementation notes:
#'
#' Semi-joins are implemented using `WHERE EXISTS`, and anti-joins with
#' `WHERE NOT EXISTS`. Support for semi-joins is somewhat partial: you
#' can only create semi joins where the `x` and `y` columns are
#' compared with `=` not with more general operators.
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
#' }
#' }
#' @name join.tbl_sql
NULL

#' @rdname join.tbl_sql
#' @export
inner_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                                suffix = c(".x", ".y"),
                                auto_index = FALSE, ...) {
  add_op_join(
    x, y,
    "inner",
    by = by,
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
                               auto_index = FALSE, ...) {
  add_op_join(
    x, y,
    "left",
    by = by,
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
                                auto_index = FALSE, ...) {
  add_op_join(
    x, y,
    "right",
    by = by,
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
                               auto_index = FALSE, ...) {
  add_op_join(
    x, y,
    "full",
    by = by,
    copy = copy,
    suffix = suffix,
    auto_index = auto_index,
    ...
  )
}

#' @rdname join.tbl_sql
#' @export
semi_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                               auto_index = FALSE, ...) {
  add_op_semi_join(
    x, y,
    anti = FALSE,
    by = by,
    copy = copy,
    auto_index = auto_index,
    ...
  )
}

#' @rdname join.tbl_sql
#' @export
anti_join.tbl_lazy <- function(x, y, by = NULL, copy = FALSE,
                               auto_index = FALSE, ...) {
  add_op_semi_join(
    x, y,
    anti = TRUE,
    by = by,
    copy = copy,
    auto_index = auto_index,
    ...
  )
}

# Set operations ---------------------------------------------------------------

# registered onLoad
intersect.tbl_lazy <- function(x, y, copy = FALSE, ...) {
  add_op_set_op(x, y, "INTERSECT", copy = copy, ...)
}
# registered onLoad
union.tbl_lazy <- function(x, y, copy = FALSE, ...) {
  add_op_set_op(x, y, "UNION", copy = copy, ...)
}
#' @export
union_all.tbl_lazy <- function(x, y, copy = FALSE, ...) {
  add_op_set_op(x, y, "UNION ALL", copy = copy, ...)
}
# registered onLoad
setdiff.tbl_lazy <- function(x, y, copy = FALSE, ...) {
  add_op_set_op(x, y, "EXCEPT", copy = copy, ...)
}

# Copying ----------------------------------------------------------------------

#' @export
auto_copy.tbl_sql <- function(x, y, copy = FALSE, ...) {
  copy_to(x$src, as.data.frame(y), random_table_name(), ...)
}

#' Copy a local data frame to a DBI backend.
#'
#' This [copy_to()] method works for all DBI sources. It is useful for
#' copying small amounts of data to a database for examples, experiments,
#' and joins. By default, it creates temporary tables which are typically
#' only visible to the current connection to the database.
#'
#' @export
#' @param types a character vector giving variable types to use for the columns.
#'    See \url{http://www.sqlite.org/datatype3.html} for available types.
#' @param temporary if `TRUE`, will create a temporary table that is
#'   local to this connection and will be automatically deleted when the
#'   connection expires
#' @param unique_indexes a list of character vectors. Each element of the list
#'   will create a new unique index over the specified column(s). Duplicate rows
#'   will result in failure.
#' @param indexes a list of character vectors. Each element of the list
#'   will create a new index.
#' @param analyze if `TRUE` (the default), will automatically ANALYZE the
#'   new table so that the query optimiser has useful information.
#' @inheritParams dplyr::copy_to
#' @return A [tbl()] object (invisibly).
#' @examples
#' library(dplyr)
#' set.seed(1014)
#'
#' mtcars$model <- rownames(mtcars)
#' mtcars2 <- src_memdb() %>%
#'   copy_to(mtcars, indexes = list("model"), overwrite = TRUE)
#' mtcars2 %>% filter(model == "Hornet 4 Drive")
#'
#' # copy_to is called automatically if you set copy = TRUE
#' # in the join functions
#' df <- tibble(cyl = c(6, 8))
#' mtcars2 %>% semi_join(df, copy = TRUE)
copy_to.src_sql <- function(dest, df, name = deparse(substitute(df)),
                            overwrite = FALSE, types = NULL, temporary = TRUE,
                            unique_indexes = NULL, indexes = NULL,
                            analyze = TRUE, ...) {
  assert_that(is.data.frame(df), is_string(name), is.flag(temporary))
  class(df) <- "data.frame" # avoid S4 dispatch problem in dbSendPreparedQuery

  name <- db_copy_to(dest$con, name, df,
    overwrite = overwrite,
    types = types,
    temporary = temporary,
    unique_indexes = unique_indexes,
    indexes = indexes,
    analyze = analyze,
    ...
  )

  invisible(tbl(dest, name))
}

#' @export
collapse.tbl_sql <- function(x, vars = NULL, ...) {
  sql <- db_sql_render(x$src$con, x)

  tbl(x$src, sql) %>%
    group_by(!!! syms(op_grps(x))) %>%
    add_op_order(op_sort(x))
}

#' @export
compute.tbl_sql <- function(x, name = random_table_name(), temporary = TRUE,
                            unique_indexes = list(), indexes = list(),
                            ...) {

  vars <- op_vars(x)
  assert_that(all(unlist(indexes) %in% vars))
  assert_that(all(unlist(unique_indexes) %in% vars))

  x_aliased <- select(x, !!! syms(vars)) # avoids problems with SQLite quoting (#1754)
  sql <- db_sql_render(x$src$con, x_aliased$ops)

  name <- db_compute(x$src$con, name, sql,
    temporary = temporary,
    unique_indexes = unique_indexes,
    indexes = indexes,
    ...
  )

  tbl(x$src, name) %>%
    group_by(!!! syms(op_grps(x))) %>%
    add_op_order(op_sort(x))
}

#' @export
collect.tbl_sql <- function(x, ..., n = Inf, warn_incomplete = TRUE) {
  assert_that(length(n) == 1, n > 0L)
  if (n == Inf) {
    n <- -1
  } else {
    # Gives the query planner information that it might be able to take
    # advantage of
    x <- head(x, n)
  }

  sql <- db_sql_render(x$src$con, x)
  out <- db_collect(x$src$con, sql, n = n, warn_incomplete = warn_incomplete)
  grouped_df(out, intersect(op_grps(x), names(out)))
}


