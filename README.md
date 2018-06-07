
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dbplyr <img src="man/figures/logo.png" align="right" />

[![Build
Status](https://travis-ci.org/tidyverse/dbplyr.svg?branch=master)](https://travis-ci.org/tidyverse/dbplyr)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/dbplyr)](http://cran.r-project.org/package=dbplyr)
[![Coverage
Status](https://img.shields.io/codecov/c/github/tidyverse/dbplyr/master.svg)](https://codecov.io/github/tidyverse/dbplyr?branch=master)

## Overview

dbplyr is the database backend for dplyr. If you are using dplyr to
connect to databases, you generally will not need to use any functions
from dbplyr, but you will need to make sure it’s installed.

## Installation

``` r
# You can install the released version from CRAN
install.packages("dbplyr")

# Or the the development version from GitHub:
# install.packages("devtools")
devtools::install_github("tidyverse/dbplyr")
```

## Usage

``` r
library(dplyr, warn.conflicts = FALSE)

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
copy_to(con, mtcars)

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
#> # ... with more rows
```
