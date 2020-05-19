verify_lazy_output <- function(...) {
  with_options(dbplyr_table_num = 0, verify_output(...))
}
