#' Infrastructure for testing dplyr
#'
#' Register testing sources, then use `test_load()` to load an existing
#' data frame into each source. To create a new table in each source,
#' use `test_frame()`.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' test_register_src("sqlite", {
#'   DBI::dbConnect(RSQLite::SQLite(), ":memory:", create = TRUE)
#' })
#'
#' test_frame(x = 1:3, y = 3:1)
#' test_load(mtcars)
#' }
#' @name testing
NULL


#' @export
#' @rdname testing
test_register_src <- function(name, src) {
  message("Registering testing src: ", name, " ", appendLF = FALSE)
  tryCatch(
    {
      test_srcs$add(name, src)
      message("OK")
    },
    error = function(e) message("\n* ", conditionMessage(e))
  )
}

#' @export
#' @rdname testing
test_register_con <- function(name, ...) {
  test_register_src(name, DBI::dbConnect(...))
}

#' @export
#' @rdname testing
src_test <- function(name) {
  srcs <- test_srcs$get()
  if (!name %in% names(srcs)) {
    testthat::skip(paste0("No ", name))
  } else {
    srcs[[name]]
  }
}

#' @export
#' @rdname testing
test_load <- function(
  df,
  name = unique_table_name(),
  srcs = test_srcs$get(),
  ignore = character()
) {
  stopifnot(is.data.frame(df))
  stopifnot(is.character(ignore))

  srcs <- srcs[setdiff(names(srcs), ignore)]
  lapply(srcs, copy_to, df, name = name)
}

#' @export
#' @rdname testing
test_frame <- function(..., srcs = test_srcs$get(), ignore = character()) {
  df <- tibble(...)
  test_load(df, srcs = srcs, ignore = ignore)
}

# Manage cache of testing srcs
test_srcs <- local({
  list(
    get = function() env_get(cache(), "srcs", list()),

    has = function(x) {
      srcs <- env_get(cache(), "srcs", list())
      has_name(srcs, x)
    },

    add = function(name, src) {
      srcs <- env_get(cache(), "srcs", list())
      srcs[[name]] <- src
      env_poke(cache(), "srcs", srcs)
    },

    set = function(...) {
      env_poke(cache(), "src", list(...))
    },

    length = function() {
      length(cache()$srcs)
    }
  )
})


# Modern helpers ----------------------------------------------------------

copy_to_test <- function(src, df, ..., name = "test") {
  copy_to(src_test(src), df, name, ..., overwrite = TRUE)
}
