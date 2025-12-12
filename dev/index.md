# dbplyr

## Overview

dbplyr is the database backend for [dplyr](https://dplyr.tidyverse.org).
It allows you to use remote database tables as if they are in-memory
data frames by automatically converting dplyr code into SQL.

To learn more about why you might use dbplyr instead of writing SQL, see
`vignette("sql")`. To learn more about the details of the SQL
translation, see
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/dev/articles/translation-verb.md)
and
[`vignette("translation-function")`](https://dbplyr.tidyverse.org/dev/articles/translation-function.md).

## Installation

``` r
# The easiest way to get dbplyr is to install the whole tidyverse:
install.packages("tidyverse")

# Alternatively, install just dbplyr:
install.packages("dbplyr")

# Or the development version from GitHub:
# install.packages("pak")
pak::pak("tidyverse/dbplyr")
```

## Usage

dbplyr is designed to work with database tables as if they were local
data frames. To demonstrate this I’ll first create an in-memory SQLite
database and copy over a dataset:

``` r
library(dplyr, warn.conflicts = FALSE)

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
copy_to(con, mtcars)
```

Note that you don’t actually need to load dbplyr with
[`library(dbplyr)`](https://dbplyr.tidyverse.org/); dplyr automatically
loads it for you when it sees you working with a database. Database
connections are coordinated by the DBI package. Learn more at
<https://dbi.r-dbi.org/>

Now you can retrieve a table using
[`tbl()`](https://dplyr.tidyverse.org/reference/tbl.html) (see
[`?tbl_dbi`](https://dbplyr.tidyverse.org/dev/reference/tbl.src_dbi.md)
for more details). Printing it just retrieves the first few rows:

``` r
mtcars2 <- tbl(con, "mtcars")
mtcars2
#> # Source:   table<`mtcars`> [?? x 11]
#> # Database: sqlite 3.50.4 [:memory:]
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> # ℹ more rows
```

All dplyr calls are evaluated lazily, generating SQL that is only sent
to the database when you request the data.

``` r
# lazily generates query
summary <- mtcars2 |> 
  group_by(cyl) |> 
  summarise(mpg = mean(mpg, na.rm = TRUE)) |> 
  arrange(desc(mpg))

# see query
summary |> show_query()
#> <SQL>
#> SELECT `cyl`, AVG(`mpg`) AS `mpg`
#> FROM `mtcars`
#> GROUP BY `cyl`
#> ORDER BY `mpg` DESC

# execute query and retrieve results
summary |> collect()
#> # A tibble: 3 × 2
#>     cyl   mpg
#>   <dbl> <dbl>
#> 1     4  26.7
#> 2     6  19.7
#> 3     8  15.1
```

## Code of Conduct

Please note that the dbplyr project is released with a [Contributor Code
of Conduct](https://dbplyr.tidyverse.org/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
