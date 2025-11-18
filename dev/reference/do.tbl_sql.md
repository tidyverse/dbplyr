# Perform arbitrary computation on remote backend

Perform arbitrary computation on remote backend

## Usage

``` r
# S3 method for class 'tbl_sql'
do(.data, ..., .chunk_size = 10000L)
```

## Arguments

- .data:

  a tbl

- ...:

  Expressions to apply to each group. If named, results will be stored
  in a new column. If unnamed, must return a data frame. You can use `.`
  to refer to the current group. You can not mix named and unnamed
  arguments.

- .chunk_size:

  The size of each chunk to pull into R. If this number is too big, the
  process will be slow because R has to allocate and free a lot of
  memory. If it's too small, it will be slow, because of the overhead of
  talking to the database.
