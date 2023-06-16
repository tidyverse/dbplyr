
# Vectors -----------------------------------------------------------------

check_list <- function(x, ..., allow_null = FALSE, arg = caller_arg(x), call = caller_env()) {
  if (vctrs::vec_is_list(x)) {
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

check_integer <- function(x,
                          ...,
                          allow_null = FALSE,
                          arg = caller_arg(x),
                          call = caller_env()) {
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

check_logical <- function(x,
                          ...,
                          allow_null = FALSE,
                          arg = caller_arg(x),
                          call = caller_env()) {
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

check_scalar_sql <- function(x,
                             ...,
                             arg = caller_arg(x),
                             call = caller_env()) {
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

check_unsupported_arg <- function(x,
                                  allowed = NULL,
                                  allow_null = FALSE,
                                  ...,
                                  backend = NULL,
                                  arg = caller_arg(x),
                                  call = caller_env()) {
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
  cli_abort(msg, call = call)
}

stop_unsupported_function <- function(f, ..., with = NULL, call = caller_env()) {
  cli_abort(c(
    "{.fun {f}} is not supported on database backends.",
    i = if (!is_null(with)) "Please use {.fun {with}} instead."
  ), call = call)
}

check_named <- function(x, ..., arg = caller_arg(x), call = caller_env()) {
  if (!is_named2(x)) {
    cli_abort("All elements of {.arg {arg}} must be named.", call = call)
  }

  if (vctrs::vec_duplicate_any(names2(x))) {
    cli_abort("The names of {.arg {arg}} must be unique.", call = call)
  }
}

check_has_names <- function(x, names, ..., arg = caller_arg(x), call = caller_env()) {
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

with_indexed_errors <- function(expr,
                                message,
                                ...,
                                .error_call = caller_env(),
                                .frame = caller_env()) {
  try_fetch(
    expr,
    purrr_error_indexed = function(cnd) {
      message <- message(cnd)
      abort(message, ..., call = .error_call, parent = cnd$parent, .frame = .frame)
    }
  )
}
