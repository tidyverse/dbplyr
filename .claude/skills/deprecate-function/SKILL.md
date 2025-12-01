---
name: deprecate-function
description: Guide for deprecating R functions in dbplyr. Use when a user asks to deprecate a function or parameter, including adding lifecycle warnings, updating documentation, adding NEWS entries, and updating tests.
---

# Deprecate Function

Use this skill when deprecating functions or function parameters in dbplyr.

## Overview

This skill guides you through the complete process of deprecating a function or parameter, ensuring all necessary changes are made consistently:
1. Add deprecation warning using `lifecycle::deprecate_warn()`
2. Add lifecycle badge to documentation
3. Add bullet point to NEWS.md
4. Create test for deprecation warning
5. Silence deprecation warnings in existing tests

## Workflow

### Step 1: Determine deprecation version

Read the current version from DESCRIPTION and calculate the deprecation version:
- Current version format: `MAJOR.MINOR.PATCH.9000` (development)
- Deprecation version: Next minor release `MAJOR.(MINOR+1).0`
- Example: If current version is `2.5.1.9000`, deprecation version is `2.6.0`

```r
# Read DESCRIPTION to get current version
# Calculate: if version is X.Y.Z.9000, deprecation version is X.(Y+1).0
```

### Step 2: Add lifecycle::deprecate_warn() call

Add the deprecation warning to the function:

```r
# For a deprecated parameter:
function_name <- function(param1, deprecated_param = deprecated()) {
  if (lifecycle::is_present(deprecated_param)) {
    lifecycle::deprecate_warn("X.Y.0", "function_name(deprecated_param)")
  }
  # rest of function
}

# For a deprecated function:
function_name <- function(...) {
  lifecycle::deprecate_warn("X.Y.0", "function_name()", "replacement_function()")
  # rest of function
}
```

Key points:
- Use `lifecycle::is_present()` to check if a deprecated parameter was supplied
- First argument is the deprecation version string (e.g., "2.6.0")
- Second argument describes what is deprecated (e.g., "f
unction_name(param)")
- Optional third argument suggests replacement

### Step 3: Add lifecycle badge to documentation

Update the roxygen2 documentation with an inline lifecycle badge:

```r
#' @param deprecated_param `r lifecycle::badge("deprecated")`
```

For full function deprecation, add to the description or details section:

```r
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Please use [replacement_function()] instead.
```

### Step 3b: Add migration examples to @examples

When deprecating a function in favor of a replacement, add old/new examples
to the `@examples` section to help users migrate:

```r
#' @examples
#' # Existing examples...
#'
#' # Old:
#' old_function(arg1, arg2)
#' # New:
#' replacement_function(arg1, arg2)
#'
#' # Old:
#' x <- "value"
#' old_function("prefix", x, "suffix")
#' # New:
#' replacement_function("prefix {x} suffix")
```

Key points:
- Place migration examples at the end of the `@examples` section
- Use "# Old:" and "# New:" comments to clearly show the transition
- Include 2-3 practical examples covering common use cases
- Make examples runnable and self-contained
- Show how the new syntax differs from the old

### Step 4: Add NEWS entry

Add a bullet point under the "# dbplyr (development version)" section in NEWS.md:

```markdown
# dbplyr (development version)

* `function_name(parameter)` is deprecated and will be removed in a future
  version.
```

or for function deprecation:

```markdown
# dbplyr (development version)

* `function_name()` is deprecated. Use `replacement_function()` instead.
```

Place the entry:
- In the appropriate subsection if one exists
- Otherwise at the top level under development version
- Keep entries concise and actionable

### Step 5: Create deprecation test

In the appropriate test file (usually `tests/testthat/test-{name}.R`):

```r
test_that("function_name(deprecated_param) is deprecated", {
  expect_snapshot(error = TRUE, {
    function_name(deprecated_param = value)
  })
})
```

Use `expect_snapshot()` to capture the deprecation warning. The `error = TRUE` argument allows the test to pass even if the code generates warnings.

### Step 6: Silence warnings in existing tests

Find all existing tests that use the deprecated function or parameter and silence lifecycle warnings:

Add at the beginning of test blocks that use the deprecated feature:

```r
test_that("existing test with deprecated feature", {
  withr::local_options(lifecycle_verbosity = "quiet")

  # existing test code
})
```

## Implementation checklist

When deprecating a function or parameter, ensure you:

- [ ] Read DESCRIPTION to determine deprecation version
- [ ] Add `lifecycle::deprecate_warn()` call in the function
- [ ] Add lifecycle badge to roxygen documentation
- [ ] Add migration examples to `@examples` section (for function deprecation)
- [ ] Add bullet point to NEWS.md under development version
- [ ] Create new test for deprecation warning using `expect_snapshot()`
- [ ] Add `withr::local_options(lifecycle_verbosity = "quiet")` to existing tests
- [ ] Run `devtools::document()` to update documentation
- [ ] Run `air format .` to format code
- [ ] Run tests to verify everything works

## Examples

### Example 1: Deprecating a parameter

Deprecating a `check_from` parameter in `tbl_sql()`:

1. Version: DESCRIPTION shows `2.5.1.9000` → deprecation version is `2.6.0`

2. Function code (R/tbl-sql.R):
```r
tbl_sql <- function(
  subclass,
  src,
  from,
  ...,
  vars = NULL,
  check_from = deprecated()
) {
  if (lifecycle::is_present(check_from)) {
    lifecycle::deprecate_warn("2.6.0", "tbl_sql(check_from)")
  }
  # ... rest of function
}
```

3. Documentation (R/tbl-sql.R):
```r
#' @param check_from `r lifecycle::badge("deprecated")`
```

4. NEWS entry:
```markdown
# dbplyr (development version)

* `tbl_sql(check_from)` is deprecated and will be removed in a future version.
```

5. Test (tests/testthat/test-tbl-sql.R):
```r
test_that("tbl_sql(check_from) is deprecated", {
  expect_snapshot(error = TRUE, {
    tbl_sql("subclass", src, from, check_from = TRUE)
  })
})
```

6. Silence in existing tests - add to any test using `check_from`:
```r
test_that("existing test", {
  withr::local_options(lifecycle_verbosity = "quiet")
  # test code
})
```

### Example 2: Deprecating a function

Deprecating `build_sql()` in favor of `glue_sql2()`:

1. Version: DESCRIPTION shows `2.5.1.9000` → deprecation version is `2.6.0`

2. Function code (R/build-sql.R):
```r
build_sql <- function(..., .env = parent.frame(), con = sql_current_con()) {
  lifecycle::deprecate_warn("2.6.0", "build_sql()", "glue_sql2()")
  # ... rest of function
}
```

3. Documentation (R/build-sql.R):
```r
#' @description
#' `r lifecycle::badge("deprecated")`
#' `build_sql()` is deprecated in favor of `glue_sql2()`.
#'
#' @examples
#' # Existing examples...
#'
#' # Migrate to glue_sql2():
#'
#' # Before:
#' build_sql("SELECT * FROM ", ident("table"), con = con)
#' # After:
#' glue_sql2(con, "SELECT * FROM {.tbl 'table'}")
#'
#' # Before:
#' name <- "Robert"
#' build_sql("INSERT INTO students (name) VALUES (", name, ")", con = con)
#' # After:
#' glue_sql2(con, "INSERT INTO students (name) VALUES ({.val name})")
```

4. NEWS entry:
```markdown
# dbplyr (development version)

* `build_sql()` is deprecated. Use `glue_sql2()` instead.
```

5. Test (tests/testthat/test-build-sql.R):
```r
test_that("build_sql() is deprecated", {
  con <- simulate_dbi()
  expect_snapshot(
    build_sql("SELECT * FROM TABLE", con = con)
  )
})
```

6. Silence in existing tests:
```r
test_that("existing test", {
  withr::local_options(lifecycle_verbosity = "quiet")
  # test code
})
```

## Lifecycle stages

The lifecycle package supports a gradual deprecation process:

1. **deprecate_soft()**: Warns only when called directly from the global environment or by package developers. Use this for initial soft deprecation to avoid alerting end users who depend on packages using the deprecated feature.

2. **deprecate_warn()**: The standard deprecation warning shown to all users. Displays once every 8 hours to avoid spam. This is the default for dbplyr deprecations.

3. **deprecate_stop()**: Converts the warning to an error, used just before removing the function entirely in a future release.

For dbplyr, we use `deprecate_warn()` as the standard approach unless there's a specific reason to use a softer deprecation.

## Additional best practices

### Documentation improvements

- Add `@keywords internal` to fully deprecated functions to remove them from the package documentation index while keeping them accessible
- Include clear explanations for why the function/parameter was deprecated
- Show examples of how to convert from old to new usage
- For deprecated parameters, explain what users should use instead

### Testing strategy

- Use `expect_snapshot()` to verify both that the function still works AND that it generates the correct warning
- Suppress warnings in existing tests using `withr::local_options(lifecycle_verbosity = "quiet")` to keep test output clean
- Create separate tests specifically for the deprecation warnings

### Argument renaming

When renaming an argument, temporarily support both names:

```r
function_name <- function(new_name, old_name = deprecated()) {
  if (lifecycle::is_present(old_name)) {
    lifecycle::deprecate_warn("X.Y.0", "function_name(old_name)", "function_name(new_name)")
    new_name <- old_name
  }
  # rest of function
}
```

### Why lifecycle::is_present()?

Always use `lifecycle::is_present()` rather than `missing()` to check if a parameter was supplied. Unlike `missing()`, `is_present()` works correctly for both direct and indirect function calls.

## Notes

- The deprecation version should be the next minor release, not the current development version
- `lifecycle::deprecate_warn()` shows warnings once every 8 hours to avoid spamming users
- Deprecation warnings should be silenced in existing tests to keep test output clean
- New tests for deprecation should use `expect_snapshot()` to capture the exact warning message
- For more details, see https://lifecycle.r-lib.org/articles/communicate.html
