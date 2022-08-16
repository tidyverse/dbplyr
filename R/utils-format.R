# nocov start
wrap <- function(..., indent = 0) {
  x <- paste0(..., collapse = "")
  wrapped <- strwrap(
    x,
    indent = indent,
    exdent = indent + 2,
    width = getOption("width")
  )

  paste0(wrapped, collapse = "\n")
}
# nocov end

indent <- function(x) {
  x <- paste0(x, collapse = "\n")
  paste0("  ", gsub("\n", "\n  ", x))
}

indent_print <- function(x) {
  indent(utils::capture.output(print(x)))
}

style_kw <- function(x) {
  highlight <- dbplyr_highlight()
  if (is_false(highlight)) {
    return(x)
  }

  highlight(x)
}

# function for the thousand separator,
# returns "," unless it's used for the decimal point, in which case returns "."
'big_mark' <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark, ...)
}

dbplyr_highlight <- function() {
  highlight <- getOption("dbplyr_highlight", cli::combine_ansi_styles("blue"))

  if (is_true(highlight)) {
    highlight <- cli::combine_ansi_styles("blue")
  }

  if (is_false(highlight)) {
    return(FALSE)
  }

  if (!inherits(highlight, "cli_ansi_style")) {
    msg <- "{.envvar dbplyr_highlight} must be `NULL`, `FALSE` or a {.cls cli_ansi_style}."
    cli::cli_abort(msg)
  }

  highlight
}
