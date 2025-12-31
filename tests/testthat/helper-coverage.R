zero_coverage <- function(file) {
  test_file <- devtools:::find_test_file(file)

  pkg <- devtools::as.package(".")
  withr::local_envvar(devtools::r_env_vars())

  testthat::local_test_directory(pkg$path, pkg$package)
  reporter <- testthat::local_snapshotter()
  reporter$start_file(test_file, "test")

  env <- devtools::load_all(pkg$path)$env
  testthat::with_reporter(reporter, {
    suppressWarnings(coverage <- covr::environment_coverage(env, test_file))
  })

  coverage_name <- devtools:::name_source(covr::display_name(coverage))
  local_name <- devtools:::name_test(file)
  coverage <- coverage[coverage_name %in% local_name]
  attr(coverage, "relative") <- TRUE
  attr(coverage, "package") <- pkg

  out <- covr::zero_coverage(coverage)
  out$filename <- file.path(
    basename(dirname(out$filename)),
    basename(out$filename)
  )
  out$value <- NULL
  out
}
