#' @importFrom R6 R6Class
NULL

Query <- R6::R6Class("Query",
  private = list(
    .nrow = NULL,
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

    print = function(...) {
      cat("<Query> ", self$sql, "\n", sep = "")
      print(self$con)
    },

    fetch = function(n = -1L) {
      res <- dbSendQuery(self$con, self$sql)
      on.exit(dbClearResult(res))

      out <- dbFetch(res, n)
      res_warn_incomplete(res)
      out
    },

    fetch_paged = function(chunk_size = 1e4, callback) {
      qry <- dbSendQuery(self$con, self$sql)
      on.exit(dbClearResult(qry))

      while (!dbHasCompleted(qry)) {
        chunk <- dbFetch(qry, chunk_size)
        callback(chunk)
      }

      invisible(TRUE)
    },

    vars = function() {
      private$.vars
    },

    nrow = function() {
      if (!is.null(private$.nrow)) return(private$.nrow)
      private$.nrow <- db_query_rows(self$con, self$sql)
      private$.nrow
    },

    ncol = function() {
      length(self$vars())
    }
  )
)
