
<!-- README.md is generated from README.Rmd. Please edit that file -->
dbplyr <img src="man/figures/logo.png" align="right" />
=======================================================

[![Build Status](https://travis-ci.org/tidyverse/dbplyr.svg?branch=master)](https://travis-ci.org/tidyverse/dbplyr) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/dbplyr)](http://cran.r-project.org/package=dbplyr) [![Coverage Status](https://img.shields.io/codecov/c/github/tidyverse/dbplyr/master.svg)](https://codecov.io/github/tidyverse/dbplyr?branch=master)

Overview
--------

dbplyr is the database backend for dplyr. If you are using dplyr to connect to databases, you generally will not need to use any functions from dbplyr, but you will need to make sure it's installed.

Issues
------

If you find any bugs, please file in [dplyr](https://github.com/tidyverse/dplyr/issues).

Installation
------------

``` r
# You can install the released version from CRAN
install.packages("dbplyr")

# Or the the development version from GitHub:
# install.packages("devtools")
devtools::install_github("tidyverse/dbplyr")
```

Usage
-----

``` r
library(dplyr, warn.conflicts = FALSE)

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
copy_to(con, mtcars)

mtcars2 <- tbl(con, "mtcars")
mtcars2
#> # Source:   table<mtcars> [?? x 11]
#> # Database: sqlite 3.19.3 [:memory:]
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21.0     6 160.0   110  3.90 2.620 16.46     0     1     4     4
#>  2  21.0     6 160.0   110  3.90 2.875 17.02     0     1     4     4
#>  3  22.8     4 108.0    93  3.85 2.320 18.61     1     1     4     1
#>  4  21.4     6 258.0   110  3.08 3.215 19.44     1     0     3     1
#>  5  18.7     8 360.0   175  3.15 3.440 17.02     0     0     3     2
#>  6  18.1     6 225.0   105  2.76 3.460 20.22     1     0     3     1
#>  7  14.3     8 360.0   245  3.21 3.570 15.84     0     0     3     4
#>  8  24.4     4 146.7    62  3.69 3.190 20.00     1     0     4     2
#>  9  22.8     4 140.8    95  3.92 3.150 22.90     1     0     4     2
#> 10  19.2     6 167.6   123  3.92 3.440 18.30     1     0     4     4
#> # ... with more rows
```
