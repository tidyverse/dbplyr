#' @importFrom pillar tbl_format_header style_subtle align
#' @export
tbl_format_header.tbl_sql <- function(x, setup, ...) {
  named_header <- tbl_sum(x)

  # The setup object may know the total number of rows
  named_header["Source"] <- tbl_desc(x, rows_total = setup$rows_total)

  # Adapted from pillar
  header <- paste0(
    align(paste0(names(named_header), ":")),
    " ",
    named_header
  )

  style_subtle(paste0("# ", header))
}
