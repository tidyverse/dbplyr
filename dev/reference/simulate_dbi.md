# Simulate database connections

These functions generate S3 objects that have been designed to simulate
the action of a database connection, without actually having the
database available. Obviously, this simulation can only be incomplete,
but most importantly it allows us to simulate SQL generation for any
database without actually connecting to it.

## Usage

``` r
simulate_dbi(class = character(), ...)
```

## Details

Simulated SQL always quotes identifies with `` `x` ``, and strings with
`'x'`.
