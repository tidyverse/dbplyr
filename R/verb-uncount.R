#' "Uncount" a database table
#'
#' This is a method for the tidyr `uncount()` generic. It uses a temporary
#' table, so your database user needs permissions to create one.
#'
#' @inheritParams arrange.tbl_lazy
#' @param weights A vector of weights. Evaluated in the context of `data`;
#'   supports quasiquotation.
#' @param .id Supply a string to create a new variable which gives a unique
#'   identifier for each created row.
#' @param .remove If `TRUE`, and `weights` is the name of a column in `data`,
#'   then this column is removed.
#' @export
#' @examples
#' df <- memdb_frame(x = c("a", "b"), n = c(1, 2))
#' dbplyr_uncount(df, n)
#' dbplyr_uncount(df, n, .id = "id")
#'
#' # You can also use constants
#' dbplyr_uncount(df, 2)
#'
#' # Or expressions
#' dbplyr_uncount(df, 2 / n)
dbplyr_uncount <- function(data, weights, .remove = TRUE, .id = NULL) {
  # Overview of the strategy:
  # 1. calculate `n_max`, the max weight
  # 2. create a temporary db table with ids from 1 to `n_max`
  # 3. duplicate data by inner joining with this temporary table under the
  #    condition that data$weight <= tmp$id
  #
  # An alternative approach would be to use a recursive CTE but this requires
  # more code and testing across backends.
  # See https://stackoverflow.com/questions/33327837/repeat-rows-n-times-according-to-column-value

  weights_quo <- enquo(weights)
  weights_is_col <- quo_is_symbol(weights_quo) &&
    quo_name(weights_quo) %in% colnames(data)

  if (weights_is_col) {
    weights_col <- quo_name(weights_quo)
  } else {
    weights_col <- "..dbplyr_weight_col"
    data <- mutate(
      data,
      !!weights_col := !!weights_quo,
    )
  }

  n_max <- pull(summarise(ungroup(data), "max" = max(!!sym(weights_col), na.rm = TRUE)))
  n_max <- vctrs::vec_cast(n_max, integer(), x_arg = "weights")

  if (is_null(.id)) {
    row_id_col <- "..dbplyr_row_id"
  } else {
    row_id_col <- .id
  }
  tbl_name <- unclass(remote_name(data, null_if_local = FALSE)) %||% "LHS"
  sql_on_expr <- expr(RHS[[!!row_id_col]] <= (!!sym(tbl_name))[[!!weights_col]])

  con <- remote_con(data)
  # FIXME: Use generate_series() if available on database
  helper_table <- copy_inline(con, tibble(!!row_id_col := seq2(1, n_max)))

  data_uncounted <- inner_join(
    data,
    helper_table,
    by = character(),
    sql_on = translate_sql(!!sql_on_expr, con = con),
    x_as = tbl_name,
    y_as = "RHS"
  )

  cols_to_remove <- character()
  if (is_null(.id)) {
    cols_to_remove <- c(cols_to_remove, row_id_col)
  }

  grps <- group_vars(data_uncounted)
  if (!weights_is_col || (weights_is_col && .remove)) {
    cols_to_remove <- c(cols_to_remove, weights_col)
    # need to regroup to be able to remove weights_col
    grps <- setdiff(grps, weights_col)
  }

  data_uncounted %>%
    ungroup() %>%
    select(-all_of(cols_to_remove)) %>%
    group_by(!!!syms(grps))
}

utils::globalVariables(c("RHS", "LHS", "all_of"))
