# Introduction to dbplyr

As well as working with local in-memory data stored in data frames,
dplyr also works with remote on-disk data stored in databases. This is
particularly useful in two scenarios:

- Your data is already in a database.

- You have so much data that it does not all fit into memory
  simultaneously and you need to use some external storage engine.

(If your data fits in memory there is no advantage to putting it in a
database: it will only be slower and more frustrating.)

This vignette focuses on the first scenario because it’s the most
common. If you’re using R to do data analysis inside a company, most of
the data you need probably already lives in a database (it’s just a
matter of figuring out which one!). However, you will learn how to load
data in to a local database in order to demonstrate dplyr’s database
tools. At the end, I’ll also give you a few pointers if you do need to
set up your own database.

## Getting started

To use databases with dplyr you need to first install dbplyr:

``` r
install.packages("dbplyr")
```

You’ll also need to install a DBI backend package. The DBI package
provides a common interface that allows dplyr to work with many
different databases using the same code. DBI is automatically installed
with dbplyr, but you need to install a specific backend for the database
that you want to connect to.

Five commonly used backends are:

- [RMariaDB](https://CRAN.R-project.org/package=RMariaDB) connects to
  MySQL and MariaDB

- [RPostgres](https://CRAN.R-project.org/package=RPostgres) connects to
  Postgres and Redshift.

- [RSQLite](https://github.com/r-dbi/RSQLite) embeds a SQLite database.

- [odbc](https://github.com/r-dbi/odbc#odbc) connects to many commercial
  databases via the open database connectivity protocol.

- [bigrquery](https://github.com/r-dbi/bigrquery) connects to Google’s
  BigQuery.

If the database you need to connect to is not listed here, you’ll need
to do some investigation (i.e. googling) yourself.

In this vignette, we’re going to use the RSQLite backend which is
automatically installed when you install dbplyr. SQLite is a great way
to get started with databases because it’s completely embedded inside an
R package. Unlike most other systems, you don’t need to setup a separate
database server. SQLite is great for demos, but is surprisingly
powerful, and with a little practice you can use it to easily work with
many gigabytes of data.

## Connecting to the database

To work with a database in dplyr, you must first connect to it, using
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).
We’re not going to go into the details of the DBI package here, but it’s
the foundation upon which dbplyr is built. You’ll need to learn more
about if you need to do things to the database that are beyond the scope
of dplyr.

``` r
library(dplyr)
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
```

The arguments to
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)
vary from database to database, but the first argument is always the
database backend. It’s
[`RSQLite::SQLite()`](https://rsqlite.r-dbi.org/reference/SQLite.html)
for RSQLite,
[`RMariaDB::MariaDB()`](https://rmariadb.r-dbi.org/reference/dbConnect-MariaDBDriver-method.html)
for RMariaDB,
[`RPostgres::Postgres()`](https://rpostgres.r-dbi.org/reference/Postgres.html)
for RPostgres,
[`odbc::odbc()`](https://odbc.r-dbi.org/reference/dbConnect-OdbcDriver-method.html)
for odbc, and `bigrquery::bigquery()` for BigQuery. SQLite only needs
one other argument: the path to the database. Here we use the special
string `":memory:"` which causes SQLite to make a temporary in-memory
database.

Most existing databases don’t live in a file, but instead live on
another server. That means in real-life that your code will look more
like this:

``` r
con <- DBI::dbConnect(RMariaDB::MariaDB(), 
  host = "database.rstudio.com",
  user = "hadley",
  password = rstudioapi::askForPassword("Database password")
)
```

(If you’re not using RStudio, you’ll need some other way to securely
retrieve your password. You should never record it in your analysis
scripts or type it into the console. [Securing
Credentials](https://db.rstudio.com/best-practices/managing-credentials)
provides some best practices.)

Our temporary database has no data in it, so we’ll start by copying over
[`nycflights13::flights`](https://rdrr.io/pkg/nycflights13/man/flights.html)
using the convenient
[`copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html)
function. This is a quick and dirty way of getting data into a database
and is useful primarily for demos and other small jobs.

``` r
copy_to(con, nycflights13::flights, "flights",
  temporary = FALSE, 
  indexes = list(
    c("year", "month", "day"), 
    "carrier", 
    "tailnum",
    "dest"
  )
)
```

As you can see, the
[`copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html)
operation has an additional argument that allows you to supply indexes
for the table. Here we set up indexes that will allow us to quickly
process the data by day, carrier, plane, and destination. Creating the
right indices is key to good database performance, but is unfortunately
beyond the scope of this article.

Now that we’ve copied the data, we can use
[`tbl()`](https://dplyr.tidyverse.org/reference/tbl.html) to take a
reference to it:

``` r
flights_db <- tbl(con, "flights")
```

When you print it out, you’ll notice that it mostly looks like a regular
tibble:

``` r
flights_db 
#> # Source:   table<`flights`> [?? x 19]
#> # Database: sqlite 3.51.0 [:memory:]
#>    year month   day dep_time sched_dep_time dep_delay arr_time
#>   <int> <int> <int>    <int>          <int>     <dbl>    <int>
#> 1  2013     1     1      517            515         2      830
#> 2  2013     1     1      533            529         4      850
#> 3  2013     1     1      542            540         2      923
#> 4  2013     1     1      544            545        -1     1004
#> 5  2013     1     1      554            600        -6      812
#> 6  2013     1     1      554            558        -4      740
#> # ℹ more rows
#> # ℹ 12 more variables: sched_arr_time <int>, arr_delay <dbl>,
#> #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>,
#> #   dest <chr>, air_time <dbl>, distance <dbl>, hour <dbl>,
#> #   minute <dbl>, time_hour <dbl>
```

The main difference is that you can see that it’s a remote source in a
SQLite database.

## Generating queries

To interact with a database you usually use SQL, the Structured Query
Language. SQL is over 40 years old, and is used by pretty much every
database in existence. The goal of dbplyr is to automatically generate
SQL for you so that you’re not forced to use it. However, SQL is a very
large language and dbplyr doesn’t do everything. It focusses on `SELECT`
statements, the SQL you write most often as an analyst.

Most of the time you don’t need to know anything about SQL, and you can
continue to use the dplyr verbs that you’re already familiar with:

``` r
flights_db %>% select(year:day, dep_delay, arr_delay)
#> # Source:   SQL [?? x 5]
#> # Database: sqlite 3.51.0 [:memory:]
#>    year month   day dep_delay arr_delay
#>   <int> <int> <int>     <dbl>     <dbl>
#> 1  2013     1     1         2        11
#> 2  2013     1     1         4        20
#> 3  2013     1     1         2        33
#> 4  2013     1     1        -1       -18
#> 5  2013     1     1        -6       -25
#> 6  2013     1     1        -4        12
#> # ℹ more rows

flights_db %>% filter(dep_delay > 240)
#> # Source:   SQL [?? x 19]
#> # Database: sqlite 3.51.0 [:memory:]
#>    year month   day dep_time sched_dep_time dep_delay arr_time
#>   <int> <int> <int>    <int>          <int>     <dbl>    <int>
#> 1  2013     1     1      848           1835       853     1001
#> 2  2013     1     1     1815           1325       290     2120
#> 3  2013     1     1     1842           1422       260     1958
#> 4  2013     1     1     2115           1700       255     2330
#> 5  2013     1     1     2205           1720       285       46
#> 6  2013     1     1     2343           1724       379      314
#> # ℹ more rows
#> # ℹ 12 more variables: sched_arr_time <int>, arr_delay <dbl>,
#> #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>,
#> #   dest <chr>, air_time <dbl>, distance <dbl>, hour <dbl>,
#> #   minute <dbl>, time_hour <dbl>

flights_db %>% 
  group_by(dest) %>%
  summarise(delay = mean(dep_delay))
#> Warning: Missing values are always removed in SQL aggregation functions.
#> Use `na.rm = TRUE` to silence this warning
#> This warning is displayed once every 8 hours.
#> # Source:   SQL [?? x 2]
#> # Database: sqlite 3.51.0 [:memory:]
#>   dest  delay
#>   <chr> <dbl>
#> 1 ABQ   13.7 
#> 2 ACK    6.46
#> 3 ALB   23.6 
#> 4 ANC   12.9 
#> 5 ATL   12.5 
#> 6 AUS   13.0 
#> # ℹ more rows
```

However, in the long-run, I highly recommend you at least learn the
basics of SQL. It’s a valuable skill for any data scientist, and it will
help you debug problems if you run into problems with dplyr’s automatic
translation. If you’re completely new to SQL you might start with this
[codeacademy tutorial](https://www.codecademy.com/learn/learn-sql). If
you have some familiarity with SQL and you’d like to learn more, I found
[how indexes work in SQLite](https://www.sqlite.org/queryplanner.html)
and [10 easy steps to a complete understanding of
SQL](https://blog.jooq.org/2016/03/17/10-easy-steps-to-a-complete-understanding-of-sql/)
to be particularly helpful.

The most important difference between ordinary data frames and remote
database queries is that your R code is translated into SQL and executed
in the database on the remote server, not in R on your local machine.
When working with databases, dplyr tries to be as lazy as possible:

- It never pulls data into R unless you explicitly ask for it.

- It delays doing any work until the last possible moment: it collects
  together everything you want to do and then sends it to the database
  in one step.

For example, take the following code:

``` r
tailnum_delay_db <- flights_db %>% 
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay),
    n = n()
  ) %>% 
  arrange(desc(delay)) %>%
  filter(n > 100)
```

Surprisingly, this sequence of operations never touches the database.
It’s not until you ask for the data (e.g. by printing `tailnum_delay`)
that dplyr generates the SQL and requests the results from the database.
Even then it tries to do as little work as possible and only pulls down
a few rows.

``` r
tailnum_delay_db
#> # Source:     SQL [?? x 3]
#> # Database:   sqlite 3.51.0 [:memory:]
#> # Ordered by: desc(delay)
#>   tailnum delay     n
#>   <chr>   <dbl> <int>
#> 1 N11119   30.3   148
#> 2 N16919   29.9   251
#> 3 N14998   27.9   230
#> 4 N15910   27.6   280
#> 5 N13123   26.0   121
#> 6 N11192   25.9   154
#> # ℹ more rows
```

Behind the scenes, dplyr is translating your R code into SQL. You can
see the SQL it’s generating with
[`show_query()`](https://dplyr.tidyverse.org/reference/explain.html):

``` r
tailnum_delay_db %>% show_query()
#> <SQL>
#> SELECT `tailnum`, AVG(`arr_delay`) AS `delay`, COUNT(*) AS `n`
#> FROM `flights`
#> GROUP BY `tailnum`
#> HAVING (COUNT(*) > 100.0)
#> ORDER BY `delay` DESC
```

If you’re familiar with SQL, this probably isn’t exactly what you’d
write by hand, but it does the job. You can learn more about the SQL
translation in
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/dev/articles/translation-verb.md)
and
[`vignette("translation-function")`](https://dbplyr.tidyverse.org/dev/articles/translation-function.md).

Typically, you’ll iterate a few times before you figure out what data
you need from the database. Once you’ve figured it out, use
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html) to
pull all the data down into a local tibble:

``` r
tailnum_delay <- tailnum_delay_db %>% collect()
tailnum_delay
#> # A tibble: 1,201 × 3
#>   tailnum delay     n
#>   <chr>   <dbl> <int>
#> 1 N11119   30.3   148
#> 2 N16919   29.9   251
#> 3 N14998   27.9   230
#> 4 N15910   27.6   280
#> 5 N13123   26.0   121
#> 6 N11192   25.9   154
#> # ℹ 1,195 more rows
```

[`collect()`](https://dplyr.tidyverse.org/reference/compute.html)
requires that database does some work, so it may take a long time to
complete. Otherwise, dplyr tries to prevent you from accidentally
performing expensive query operations:

- Because there’s generally no way to determine how many rows a query
  will return unless you actually run it,
  [`nrow()`](https://rdrr.io/r/base/nrow.html) is always `NA`.

- Because you can’t find the last few rows without executing the whole
  query, you can’t use [`tail()`](https://rdrr.io/r/utils/head.html).

``` r
nrow(tailnum_delay_db)
#> [1] NA

tail(tailnum_delay_db)
#> Error in `tail()`:
#> ! `tail()` is not supported on database backends.
```

You can also ask the database how it plans to execute the query with
[`explain()`](https://dplyr.tidyverse.org/reference/explain.html). The
output is database dependent, and can be esoteric, but learning a bit
about it can be very useful because it helps you understand if the
database can execute the query efficiently, or if you need to create new
indices.

## Creating your own database

If you don’t already have a database, here’s some advice from my
experiences setting up and running all of them. SQLite is by far the
easiest to get started with. PostgreSQL is not too much harder to use
and has a wide range of built-in functions. In my opinion, you shouldn’t
bother with MySQL/MariaDB: it’s a pain to set up, the documentation is
subpar, and it’s less featureful than Postgres. Google BigQuery might be
a good fit if you have very large data, or if you’re willing to pay (a
small amount of) money to someone who’ll look after your database.

All of these databases follow a client-server model - a computer that
connects to the database and the computer that is running the database
(the two may be one and the same but usually isn’t). Getting one of
these databases up and running is beyond the scope of this article, but
there are plenty of tutorials available on the web.

### MySQL/MariaDB

In terms of functionality, MySQL lies somewhere between SQLite and
PostgreSQL. It provides a wider range of [built-in
functions](http://dev.mysql.com/doc/refman/5.0/en/functions.md). It
gained support for window functions in 2018.

### PostgreSQL

PostgreSQL is a considerably more powerful database than SQLite. It has
a much wider range of [built-in
functions](http://www.postgresql.org/docs/current/static/functions.md),
and is generally a more featureful database.

### BigQuery

BigQuery is a hosted database server provided by Google. To connect, you
need to provide your `project`, `dataset` and optionally a project for
`billing` (if billing for `project` isn’t enabled).

It provides a similar set of functions to Postgres and is designed
specifically for analytic workflows. Because it’s a hosted solution,
there’s no setup involved, but if you have a lot of data, getting it to
Google can be an ordeal (especially because upload support from R is not
great currently). (If you have lots of data, you can [ship hard
drives](https://cloud.google.com/storage-transfer-service)!)
