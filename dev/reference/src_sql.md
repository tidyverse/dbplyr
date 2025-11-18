# Create a "sql src" object

Deprecated: please use directly use a `DBIConnection` object instead.

## Usage

``` r
src_sql(subclass, con, ...)
```

## Arguments

- subclass:

  name of subclass. "src_sql" is an abstract base class, so you must
  supply this value. `src_` is automatically prepended to the class name

- con:

  the connection object

- ...:

  fields used by object
