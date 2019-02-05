
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

rule <- function(pad = "-", gap = 2L) {
  paste0(rep(pad, getOption("width") - gap), collapse = "")
}

named_rule <- function(..., pad = "-") {
  if (nargs() == 0) {
    title <- ""
  } else {
    title <- paste0(...)
  }
  paste0(title, " ", rule(pad = pad, gap = nchar(title) - 1))
}

# function for the thousand separator,
# returns "," unless it's used for the decimal point, in which case returns "."
'big_mark' <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark, ...)
}
