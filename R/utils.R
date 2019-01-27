deparse_trunc <- function(x, width = getOption("width")) {
  text <- deparse(x, width.cutoff = width)
  if (length(text) == 1 && nchar(text) < width) return(text)

  paste0(substr(text[1], 1, width - 3), "...")
}

is.wholenumber <- function(x) {
  trunc(x) == x
}

deparse_all <- function(x) {
  x <- purrr::map_if(x, is_formula, f_rhs)
  purrr::map_chr(x, expr_text, width = 500L)
}

#' Provides comma-separated string out ot the parameters
#' @export
#' @keywords internal
#' @param ... Arguments to be constructed into the string
named_commas <- function(...) {
  x <- unlist(purrr::map(list2(...), as.character))
  if (is_null(names(x))) {
    paste0(x, collapse = ", ")
  } else {
    paste0(names(x), " = ", x, collapse = ", ")
  }
}

commas <- function(...) paste0(..., collapse = ", ")

in_travis <- function() identical(Sys.getenv("TRAVIS"), "true")

unique_name <- local({
  i <- 0

  function() {
    i <<- i + 1
    paste0("zzz", i)
  }
})

succeeds <- function(x, quiet = FALSE) {
  tryCatch(
    {
      x
      TRUE
    },
    error = function(e) {
      if (!quiet)
        message("Error: ", e$message)
      FALSE
    }
  )
}

c_character <- function(...) {
  x <- c(...)
  if (length(x) == 0) {
    return(character())
  }

  if (!is.character(x)) {
    stop("Character input expected", call. = FALSE)
  }

  x
}

cat_line <- function(...) cat(..., "\n", sep = "")

parse_period_unit <- function(unit){
  if (length(unit) > 1) {
    warning("Unit argument longer than 1. Taking first element.")
    unit <- unit[[1]]
  }
  m <- regexpr(" *(?<n>[0-9.,]+)? *(?<unit>[^ \t\n]+)",
               unit[[1]], perl = T)
  if (m > 0) {
    nms <- attr(m, "capture.names")
    nms <- nms[nzchar(nms)]
    start <- attr(m, "capture.start")
    end <- start + attr(m, "capture.length") - 1L
    n <- if (end[[1]] >= start[[1]]) {
      as.integer(str_sub(unit, start[[1]], end[[1]]))
    }
    else {
      1
    }
    unit <- str_sub(unit, start[[2]], end[[2]])
    list(n = n, unit = unit)
  }
  else {
    stop(sprintf("Invalid unit specification '%s'",
                 unit))
  }
}

standardise_period_names <- function(x){
  dates <- c("second", "minute", "hour", "day", "week", "month",
             "year", "bimonth", "quarter", "halfyear", "season")
  y <- gsub("(.)s$", "\\1", x)
  y <- substr(y, 1, 3)
  res <- dates[pmatch(y, dates)]
  if (any(is.na(res))) {
    stop("Invalid period name: ", paste(x[is.na(res)], collapse = ", "),
         call. = FALSE)
  }
  res
}
