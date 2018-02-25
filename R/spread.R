#' @export
spread.tbl_sql <- function(data, key, value, fill = NA){
  key <- enexpr(key)
  value <- enexpr(value)
  fill <- enexpr(fill)

  # Obtains unique values
  keys <- data %>%
    group_by(!! key) %>%
    summarise() %>%
    pull()

  # Builds an `ifelse()` statement per unique value
  conds <- keys %>%
    purrr::map(~expr(ifelse((!! key) == (!! .x), !! value, (!! fill))))

  # Sets names
  nf <- keys %>%
    purrr::map(~expr(!! sym(.x)))

  # Adds a mutate per each unique value
  for(vals in 1:length(keys)){
    data <- mutate(data, !! expr_text(nf[[vals]]) := !! conds[[vals]])
  }
  data %>%
    select(- !!key, - !! value)
}
