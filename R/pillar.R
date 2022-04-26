#' @importFrom pillar tbl_format_header style_subtle align
#' @export
tbl_format_header.tbl_sql <- function(x, setup, ...) {
  # The setup object may know the total number of rows
  desc <- tbl_desc(x, rows_total = setup$rows_total)
  tbl_sum <- tbl_sum_tbl_sql(x, desc = desc)
  named_header <- tbl_sum

  # Copied from pillar
  if (all(names2(named_header) == "")) {
    header <- named_header
  } else {
    header <- paste0(
      align(paste0(names2(named_header), ":"), space = "\U00A0"),
      # We add a space after the NBSP inserted by align()
      # so that wrapping occurs at the right location for very narrow outputs
      " ",
      named_header
    )
  }

  style_subtle(paste0("# ", header))
}
