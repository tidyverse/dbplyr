# Simulate database connections

These functions generate S3 objects that have been designed to simulate
the action of a database connection, without actually having the
database available. Obviously, this simulation can only be incomplete,
but most importantly it allows us to simulate SQL generation for any
database without actually connecting to it.

Simulated SQL quotes identifiers with `"x"` (double quotes) by default,
`` `x` `` (backticks) for MySQL/MariaDB/SQLite, and `[x]` (square
brackets) for SQL Server. Strings are quoted with `'x'`.

## Usage

``` r
simulate_dbi(class = character(), ...)
```
