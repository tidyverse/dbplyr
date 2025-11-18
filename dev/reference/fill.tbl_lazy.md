# Fill in missing values with previous or next value

Fill in missing values with previous or next value

## Usage

``` r
# S3 method for class 'tbl_lazy'
fill(.data, ..., .direction = c("down", "up", "updown", "downup"))
```

## Arguments

- .data:

  A lazy data frame backed by a database query.

- ...:

  Columns to fill.

- .direction:

  Direction in which to fill missing values. Currently either "down"
  (the default) or "up". Note that "up" does not work when `.data` is
  sorted by non-numeric columns. As a workaround revert the order
  yourself beforehand; for example replace `arrange(x, desc(y))` by
  `arrange(desc(x), y)`.

## Examples

``` r
squirrels <- tibble::tribble(
  ~group,    ~name,     ~role,     ~n_squirrels, ~ n_squirrels2,
  1,      "Sam",    "Observer",   NA,                 1,
  1,     "Mara", "Scorekeeper",    8,                NA,
  1,    "Jesse",    "Observer",   NA,                NA,
  1,      "Tom",    "Observer",   NA,                 4,
  2,     "Mike",    "Observer",   NA,                NA,
  2,  "Rachael",    "Observer",   NA,                 6,
  2,  "Sydekea", "Scorekeeper",   14,                NA,
  2, "Gabriela",    "Observer",   NA,                NA,
  3,  "Derrick",    "Observer",   NA,                NA,
  3,     "Kara", "Scorekeeper",    9,                 10,
  3,    "Emily",    "Observer",   NA,                NA,
  3, "Danielle",    "Observer",   NA,                NA
)
squirrels$id <- 1:12

tbl_memdb(squirrels) %>%
  window_order(id) %>%
  tidyr::fill(
    n_squirrels,
    n_squirrels2,
  )
#> # Source:     SQL [?? x 6]
#> # Database:   sqlite 3.51.0 [:memory:]
#> # Ordered by: id
#>    group name     role        n_squirrels n_squirrels2    id
#>    <dbl> <chr>    <chr>             <dbl>        <dbl> <int>
#>  1     1 Sam      Observer             NA            1     1
#>  2     1 Mara     Scorekeeper           8            1     2
#>  3     1 Jesse    Observer              8            1     3
#>  4     1 Tom      Observer              8            4     4
#>  5     2 Mike     Observer              8            4     5
#>  6     2 Rachael  Observer              8            6     6
#>  7     2 Sydekea  Scorekeeper          14            6     7
#>  8     2 Gabriela Observer             14            6     8
#>  9     3 Derrick  Observer             14            6     9
#> 10     3 Kara     Scorekeeper           9           10    10
#> 11     3 Emily    Observer              9           10    11
#> 12     3 Danielle Observer              9           10    12
```
