#' @export
spread.tbl_sql <- function(data, key, value, fill = NA){
  key   <- enexpr(key)
  value <- enexpr(value)
  fill  <- enexpr(fill)

  # Obtains unique values
  keys <- data %>%
    group_by(!! key) %>%
    summarise() %>%
    pull()

  # Builds an `ifelse()` statement per unique value
  f <- keys %>%
    purrr::map(~expr(ifelse((!! key) == (!! .x), !! value, (!! fill)))) %>%
    purrr::set_names(keys)

  data %>%
    mutate(!!! f) %>%
    select(- !!key, - !! value)
}
