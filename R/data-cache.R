cache <- function() {
  if (!is_attached("dbplyr_cache")) {
    get("attach")(
      new_environment(),
      name = "dbplyr_cache",
      pos = length(search()) - 1
    )
  }
  search_env("dbplyr_cache")
}

cache_computation <- function(name, computation) {
  cache <- cache()

  if (env_has(cache, name)) {
    env_get(cache, name)
  } else {
    res <- force(computation)
    env_poke(cache, name, res)
    res
  }
}

# nocov start
load_srcs <- function(f, src_names, quiet = NULL) {
  if (is.null(quiet)) {
    quiet <- !identical(Sys.getenv("NOT_CRAN"), "true")
  }

  srcs <- lapply(src_names, function(x) {
    out <- NULL
    try(out <- f(x), silent = TRUE)
    if (is.null(out) && !quiet) {
      message("Could not instantiate ", x, " src")
    }
    out
  })

  purrr::compact(setNames(srcs, src_names))
}


db_location <- function(path = NULL, filename) {
  if (!is.null(path)) {
    # Check that path is a directory and is writeable
    if (!file.exists(path) || !file.info(path)$isdir) {
      cli_abort("{.path {path}} is not a directory")
    }
    if (!is_writeable(path)) {
      cli_abort("Can not write to {.path {path}}")
    }
    return(file.path(path, filename))
  }

  pkg <- file.path(system.file("db", package = "dplyr"))
  if (is_writeable(pkg)) {
    return(file.path(pkg, filename))
  }

  tmp <- tempdir()
  if (is_writeable(tmp)) {
    return(file.path(tmp, filename))
  }

  cli_abort("Could not find writeable location to cache db")
}

is_writeable <- function(x) {
  unname(file.access(x, 2) == 0)
}
# nocov end
