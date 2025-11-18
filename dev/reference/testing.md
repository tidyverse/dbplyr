# Infrastructure for testing dplyr

Register testing sources, then use `test_load()` to load an existing
data frame into each source. To create a new table in each source, use
`test_frame()`.

## Usage

``` r
test_register_src(name, src)

test_register_con(name, ...)

src_test(name)

test_load(
  df,
  name = unique_table_name(),
  srcs = test_srcs$get(),
  ignore = character()
)

test_frame(..., srcs = test_srcs$get(), ignore = character())
```

## Examples

``` r
if (FALSE) { # \dontrun{
test_register_src("sqlite", {
  DBI::dbConnect(RSQLite::SQLite(), ":memory:", create = TRUE)
})

test_frame(x = 1:3, y = 3:1)
test_load(mtcars)
} # }
```
