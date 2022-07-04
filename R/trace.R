tracing_level <- function() {
  trace <- getOption("dbplyr.trace")
  if (is.null(trace)) {
    return(0L)
  }
  if (is.logical(trace)) {
    return(as.integer(trace))
  }
  if (!is_integerish(trace)) {
    warn("Invalid value for dbplyr.trace option, resetting.")
    options(dbplyr.trace = NULL)
    return(0)
  }
  as.integer(trace)
}

tracing_id <- function() {
  # Needs to use option to unique IDs across reloads while testing
  i <- getOption("dbplyr.trace_id", 0) + 1
  options(dbplyr.trace_id = i)
  i
}

dbGetQuery <- function(conn, statement, ...) {
  level <- tracing_level()
  id <- tracing_id()

  if (level >= 1) {
    message_base <- paste0("[", id, "]: dbGetQuery()")
    message_pre <- paste0(message_base, "\n", statement)
    message_post <- paste0(message_base, " done")
    class <- c("dplyr_message_trace_get_query", "dplyr_message_trace", "dplyr_message")
    inform(message_pre, class = class)
    on.exit({
      inform(message_post, class = class)
    })
  }

  DBI::dbGetQuery(conn, statement, ...)
}

dbSendQuery <- function(conn, statement, ...) {
  level <- tracing_level()
  id <- tracing_id()

  if (level >= 1) {
    message_base <- paste0("[", id, "]: dbSendQuery()")
    message_pre <- paste0(message_base, "\n", statement)
    message_post <- paste0(message_base, " done")
    class <- c("dplyr_message_trace_send_query", "dplyr_message_trace", "dplyr_message")
    inform(message_pre, class = class)
    on.exit({
      inform(message_post, class = class)
    })
  }

  DBI::dbSendQuery(conn, statement, ...)
}

dbExecute <- function(conn, statement, ...) {
  level <- tracing_level()
  id <- tracing_id()

  if (level >= 1) {
    message_base <- paste0("[", id, "]: dbExecute()")
    message_pre <- paste0(message_base, "\n", statement)
    message_post <- paste0(message_base, " done")
    class <- c("dplyr_message_trace_execute", "dplyr_message_trace", "dplyr_message")
    inform(message_pre, class = class)
    on.exit({
      inform(message_post, class = class)
    })
  }

  DBI::dbExecute(conn, statement, ...)
}
