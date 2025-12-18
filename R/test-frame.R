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




# Manage cache of testing srcs
test_srcs <- local({
  list(
    get = \() env_get(cache(), "srcs", list()),

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
