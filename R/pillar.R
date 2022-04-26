#' @importFrom pillar tbl_format_header style_subtle align
#' @export
tbl_format_header.tbl_sql <- function(x, setup, ...) {
  # The setup object may know the total number of rows
  desc <- tbl_desc(x, rows_total = setup$rows_total)
  named_header <- tbl_sum_tbl_sql(x, desc = desc)

  # Adapted from pillar
  header <- paste0(
    align(paste0(names(named_header), ":")),
    " ",
    named_header
  )

  style_subtle(paste0("# ", header))
}
