#' "Uncount" a database table
#'
#' This is a method for the tidyr `uncount()` generic.
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
  weights_quo <- enquo(weights)
  weights_col <- "..dbplyr_weight_col"
  data <- mutate(
    data,
    !!weights_col := !!weights_quo,
  )

  n_max <- summarise(ungroup(data), max(!!sym(weights_col), na.rm = TRUE)) %>% pull()
  n_max <- vctrs::vec_cast(n_max, integer(), x_arg = "weights")

  if (is_null(.id)) {
    row_id_col <- "..dbplyr_row_id"
  } else {
    row_id_col <- .id
  }
  sql_on_expr <- expr(RHS[[!!row_id_col]] <= LHS[[!!weights_col]])

  data_uncounted <- inner_join(
    data,
    tibble(!!row_id_col := seq2(1, n_max)),
    by = character(),
    sql_on = translate_sql(!!sql_on_expr, con = remote_con(data)),
    copy = TRUE
  )

  cols_to_remove <- weights_col
  if (is_null(.id)) {
    cols_to_remove <- c(cols_to_remove, row_id_col)
  }

  if (is_true(.remove) && quo_is_symbol(weights_quo)) {
    cols_to_remove <- c(cols_to_remove, quo_name(weights_quo))
  }

  select(data_uncounted, -all_of(cols_to_remove))
}
