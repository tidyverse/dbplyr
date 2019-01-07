
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dbplyr <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/tidyverse/dbplyr.svg?branch=master)](https://travis-ci.org/tidyverse/dbplyr)
[![CRAN
status](https://www.r-pkg.org/badges/version/dbplyr)](https://cran.r-project.org/package=dbplyr)
[![Codecov test
coverage](https://codecov.io/gh/tidyverse/dbplyr/branch/master/graph/badge.svg)](https://codecov.io/gh/tidyverse/dbplyr?branch=master)
<!-- badges: end -->

## Overview

dbplyr is the database backend for dplyr. If you are using dplyr to
connect to databases, you generally will not need to use any functions
from dbplyr, but you will need to make sure it’s installed.

## Installation

``` r
# The easiest way to get dbplyr is to install the whole tidyverse:
install.packages("tidyverse")

# Alternatively, install just dbplyr:
install.packages("dbplyr")

# Or the the development version from GitHub:
# install.packages("devtools")
devtools::install_github("tidyverse/dbplyr")
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

Now you can retrieve a table using `tbl()` (see `?tbl_dbi` for more
details):

``` r
mtcars2 <- tbl(con, "mtcars")
mtcars2
#> # Source:   table<mtcars> [?? x 11]
#> # Database: sqlite 3.22.0 [:memory:]
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
#> # … with more rows
```

More complicated expressions are evaluated lazily:

``` r
# Lazily generates query
summary <- mtcars2 %>% 
  group_by(cyl) %>% 
  summarise(mpg = mean(mpg, na.rm = TRUE)) %>% 
  arrange(desc(mpg))

# see query
summary %>% show_query()
#> <SQL>
#> SELECT `cyl`, AVG(`mpg`) AS `mpg`
#> FROM `mtcars`
#> GROUP BY `cyl`
#> ORDER BY `mpg` DESC

# execute query and retrieve results
summary %>% collect()
#> # A tibble: 3 x 2
#>     cyl   mpg
#>   <dbl> <dbl>
#> 1     4  26.7
#> 2     6  19.7
#> 3     8  15.1
```
