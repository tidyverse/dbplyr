#' @importFrom R6 R6Class
NULL

Query <- R6::R6Class("Query",
  private = list(
    .vars = NULL
  ),
  public = list(
    con = NULL,
    sql = NULL,

    initialize = function(con, sql, vars) {
      self$con <- con
      self$sql <- sql
      private$.vars <- vars
    },

    # nocov start
    print = function(...) {
      cat_line("<Query> ", self$sql)
      print(self$con)
    },

    fetch = function(n = -1L) {
      res <- dbSendQuery(self$con, self$sql)
      on.exit(dbClearResult(res))

      out <- dbFetch(res, n)
      res_warn_incomplete(res)
      out
    },
    # nocov end

    fetch_paged = function(chunk_size = 1e4, callback) {
      qry <- dbSendQuery(self$con, self$sql)
      on.exit(dbClearResult(qry))

      while (!dbHasCompleted(qry)) {
        chunk <- dbFetch(qry, chunk_size)
        callback(chunk)
      }

      invisible(TRUE)
    },

    # nocov start
    vars = function() {
      private$.vars
    },

    ncol = function() {
      length(self$vars())
    }
    # nocov end
  )
)
