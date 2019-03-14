# nocov start
.onLoad <- function(...) {
  register_s3_method("dplyr", "union", "tbl_lazy")
  register_s3_method("dplyr", "intersect", "tbl_lazy")
  register_s3_method("dplyr", "setdiff", "tbl_lazy")
  register_s3_method("dplyr", "setdiff", "tbl_Oracle")
  register_s3_method("dplyr", "setdiff", "OraConnection")
  register_s3_method("dplyr", "filter", "tbl_lazy")

  if (utils::packageVersion("dplyr") >= "0.8.0.9008") {
    register_s3_method("dplyr", "group_by_drop_default", "tbl_lazy")
  }

  # These are also currently defined in dplyr, and we want to avoid a warning
  # about double duplication. Conditional can be removed after update to
  # dplyr
  if (!methods::isClass("sql")) {
    setOldClass(c("sql", "character"), sql())
  }
}

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
# nocov end
