# Vectors -----------------------------------------------------------------

check_list <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (vctrs::vec_is_list(x)) {
    return()
  }
  if (allow_null && is_null(x)) {
    return()
  }
  stop_input_type(
    x,
    c("a list"),
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_integer <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (is.integer(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    what = "an integer vector",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_logical <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (is.logical(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    what = "a logical vector",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}


# Scalars -----------------------------------------------------------------

check_con <- function(con, ..., arg = caller_arg(con), call = caller_env()) {
  if (is.null(con)) {
    cli_abort("{.arg {arg}} must not be NULL.", call = call)
  }
}

check_lazy_query <- function(x, ..., arg = caller_arg(x), call = caller_env()) {
  if (!inherits(x, "lazy_query")) {
    stop_input_type(
      x,
      what = "a lazy query",
      ...,
      arg = arg,
      call = call
    )
  }
}

check_scalar_sql <- function(x, ..., arg = caller_arg(x), call = caller_env()) {
  if ((is.sql(x) || is_string(x)) && length(x) == 1L) {
    return()
  }

  stop_input_type(
    x,
    what = c("a single SQL query"),
    call = call,
    arg = arg
  )
}


# Other checks ------------------------------------------------------------

#' Signal that a feature is not supported
#'
#' @description
#' These helpers produce consistent error messages when a feature is not
#' supported by a database backend. They are useful both when implementing
#' dplyr verbs and when writing SQL translations for a new backend.
#'
#' * `check_unsupported_arg()` errors if an argument has a value that can't
#'   be translated to SQL.
#' * `stop_unsupported_verb()` errors that a dplyr verb is not supported on
#'   database backends, optionally suggesting an alternative verb.
#'
#' @param x An argument supplied by the user.
#' @param allowed The only supported value for `x`. If `NULL`, the argument
#'   is not supported at all.
#' @param allow_null Is `NULL` an allowed value for `x`?
#' @param backend The name of the backend, used in the error message.
#' @param f The name of the unsupported verb or function, as a string.
#' @param with The name of an alternative verb to suggest instead.
#' @param arg An argument name as a string. This argument will be mentioned
#'   in error messages as the input that is at the origin of a problem.
#' @param call The execution environment of a currently running function,
#'   e.g. [caller_env()]. The function will be mentioned in error messages
#'   as the source of the error.
#' @param ... Reserved for future expansion; must be empty.
#' @seealso [sql_not_supported()] for signalling that a function has no SQL
#'   translation.
#' @name backend_errors
NULL

#' @rdname backend_errors
#' @export
check_unsupported_arg <- function(
  x,
  allowed = NULL,
  allow_null = FALSE,
  ...,
  backend = NULL,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (is_missing(x)) {
    return()
  }

  if (allow_null && is_null(x)) {
    return()
  }

  if (identical(x, allowed)) {
    return()
  }

  if (is_null(allowed)) {
    msg <- "Argument {.arg {arg}} isn't supported"
  } else {
    msg <- "{.code {arg} = {.val {x}}} isn't supported"
  }

  if (is.null(backend)) {
    msg <- paste0(msg, " on database backends.")
  } else {
    msg <- paste0(msg, " in {backend} translation.")
  }

  if (!is_null(allowed)) {
    if (allow_null) {
      allow_msg <- "It must be {.val {allowed}} or {.code NULL} instead."
    } else {
      allow_msg <- "It must be {.val {allowed}} instead."
    }

    msg <- c(msg, i = allow_msg)
  }
  cli_abort(msg, call = call, class = "dbplyr_error_unsupported_arg")
}

#' @rdname backend_errors
#' @export
stop_unsupported_verb <- function(
  f,
  ...,
  with = NULL,
  call = caller_env()
) {
  cli_abort(
    c(
      "{.fun {f}} is not supported on database backends.",
      i = if (!is_null(with)) "Please use {.fun {with}} instead."
    ),
    call = call,
    class = "dbplyr_error_unsupported_verb"
  )
}

check_named <- function(x, ..., arg = caller_arg(x), call = caller_env()) {
  if (!is_named2(x)) {
    cli_abort("All elements of {.arg {arg}} must be named.", call = call)
  }

  if (vctrs::vec_duplicate_any(names2(x))) {
    cli_abort("The names of {.arg {arg}} must be unique.", call = call)
  }
}

check_has_names <- function(
  x,
  names,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (is.data.frame(x)) {
    x_nms <- colnames(x)
  } else {
    x_nms <- names(x)
  }
  if (identical(x_nms, names)) {
    return()
  }

  cli_abort("{.arg {arg}} must have fields {.val {names}}", .internal = TRUE)
}


check_sql <- function(
  x,
  allow_null = FALSE,
  allow_names = TRUE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (is.sql(x)) {
    if (!allow_names && is_named(x)) {
      stop_input_type(x, "an unnamed <sql>", arg = arg, call = call)
    }
    return()
  }
  if (allow_null && is.null(x)) {
    return()
  }
  stop_input_type(x, "a <sql>", arg = arg, call = call, allow_null = allow_null)
}

with_indexed_errors <- function(
  expr,
  message,
  ...,
  .error_call = caller_env(),
  .frame = caller_env()
) {
  withCallingHandlers(
    expr,
    purrr_error_indexed = function(cnd) {
      message <- message(cnd)
      cli::cli_abort(
        message,
        ...,
        call = .error_call,
        parent = cnd$parent,
        .frame = .frame
      )
    }
  )
}
