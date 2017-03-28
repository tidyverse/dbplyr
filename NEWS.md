# dplyr 0.5.0.9000

* `win_over()` escapes expression using current database rules.

* You can create a `tbl` directly from a DBI connection (#2576).

* `mutate.tbl_sql()` will now generate as many subqueries as necessary so 
  that you can refer to variables that you just created (like in mutate
  with regular dataframes) (#2481, #2483).

* `print(df, n = Inf)` and `head(df, n = Inf)` now work when `df` is a
  SQL database (#2580).

* SQL set operations (`intersect()`, `union()`, and `setdiff())
  now match column names across inputs, filling in non-matching variables
  with NULL (#2556).

* `substr()` is now translated to SQL, correcting for the difference
  in the third argument. In R, it's the position of the last character,
  in SQL it's the length of the string (#2536).

* Databases and lazy tables are now compatible with `_if()` variants
  such as `mutate_if()`. The predicates are applied on the first 10000
  rows.

* The SQL translation of `n_distinct()` now accepts multiple variables
  (#2148).

* `copy_to()` now returns it's output invisibly (since you're often just
   calling for the side-effect).

* `Inf` and `-Inf` are now correctled escaped for databases.

* DBI tables print the table name if known.

* `copy_to()` gains an `overwrite` argument which allows you to overwrite
  an existing table. Use with care! (#2296)

* dplyr has gained a basic SQL optimiser, which collapses certain nested
  SELECT queries into a single query (#1979). This will improve query
  execution performance for databases with less sophisticated query optimisers,
  and fixes certain problems with ordering and limits in subqueries (#1979).
  A big thanks goes to @hhoeflin for figuring out this optimisation.

* `explain()` and `show_query()` now invisibly return the first argument,
  making them easier to use inside a pipeline.

* `collect()` will automatically LIMIT the result to the `n`, the number of 
  rows requested. This will provide the query planner with more information
  that it may be able to use to improve execution time (#2083).

* `distinct()` reports improved variable information for SQL backends. This
  means that it is more likely to work in the middle of a pipeline (#2359).

* `compute()` and `collapse()` now preserve the "ordering" of rows.
  This only affects the computation of window functions, as the rest
  of SQL does not care about row order (#2281).

* Database tables now display how they are ordered (#2287)

* [API] `op_vars()` now returns a list of quoted expressions. This
  enables escaping to happen at the correct time (i.e. when the connection
  is known).

* Database backends now understand how grouping is affected by renames
  (#1962)
  
* Database backends only regroup by variables present in the data (#2156)

* SQL joins have been improved:

  * They now generate SQL more similar to what you'd write by hand,
    eliminating a layer or two of subqueries (#2333)
    
  * [API] They now follow the same rules for including duplicated key variables
    that the data frame methods do, namely that key variables are only
    kept from `x`, and never from `y` (#2410)
    
  * [API] The `sql_join()` generic now gains a `vars` argument which lists
    the variables taken from the left and right sides of the join. If you
    have a custom `sql_join()` method, you'll need to update how your
    code generates joins, following the template in `sql_join.generic()`.

* `group_by()` can now perform an inline mutate for database backends (#2422).

* `full_join()` throws a clear error when you attempt to use it with a
  MySQL backend (#2045)

* The SQL generation set operations (`intersect()`, `setdiff()`, `union()`, and
  `union_all()`) on databases has been considerably improved. By default,
  the component SELECT are surrounded with parentheses, except on SQLite.
  The SQLite backend will now throw an error if you attempt a set operation
  on a query that contains a LIMIT, as that is not supported in SQLite (#2270).

* Ungrouped `do()` on database backends now collects all data locally first
  (#2392).

* `common_by()` gets a better error message for unexpected inputs (#2091)

* `copy_to()` no longer checks that the table doesn't exist before creation,
  intead preferring to fall back on the database for error messages. This
  should reduce both false positives and false negative (#1470)

* New `src_dbi()` is the new offiicial way to construct dplyr sources with
  a remote backend. `src_dbi()` is very similar to the existing `src_sql()`
  (which remains for backward compatibility), but it recognises that dplyr
  only works with database backends that support the DBI protocol (#2423).
  `src_desc.src_dbi()` dispatches on the connection, eliminating the
  last method that required a dispatch on the class of the src, rather
  than the underlying connection.

* [API] The signature of `op_base` has changed to `op_base(x, vars, class)`

* `x %in% 1` is now correctly translated to `x IN (1)` (#511).

* Export `win_over()`, `win_rank()`, `win_recycled()`, and `win_cumulative()`.
  Export `win_current_group()` and `win_current_order()` (#2051, #2126)

* `partial_eval()` now handles `::` and `:::` correct (#2321)

* Behind the scenes dplyr now uses `DBI::dbExecute()` for non-query
  SQL commands (#1912)

* Logical values are now translated to 0 and 1 rather than TRUE and FALSE
  this should work on a wider range of backends (#2052).

* Partial evaluation occurs immediately when you execute a `filter()`,
  `mutate()` etc, rather than happening when the query is executed (#2370).
  
* [API] `translate_sql()` and `partial_eval()` have been refined:

    * `translate_sql()` no longer takes a vars argument; instead call
      `partial_eval()` yourself. 
    
    * Because it no longer needs the environment `translate_sql()_` now
      works with a list of dots, rather than a `lazy_dots`.
      
    * `partial_eval()` now takes a character vector of variable names
      rather than a tbl.
      
    * This leads to a simplification of the `op` data structure: 
      dots is now a list of expressions rather than a `lazy_dots`.
    
* SQL translation contains a better test for whether or not a double
  is similar to an integer and hence needs a trailing 0.0 added (#2004).

* `ifelse()` and `if_else()` use correct argument names in SQL translation 
  (#2225).

* [API] `log(x, b)` is now correctly translated to the SQL `log(b, x)` (#2288).
  SQLite does not support the 2-argument log function so it is translated
  to `log(x) / log(b)`.

* `is.na()` and `is.null()` gain extra parens in SQL translation to preserve
  correct precedence (#2302).

* `collect()` once again defaults to return all rows in the data (#1968).
  This makes it behave the same as `as.data.frame()` and `as_tibble()`.

* Fix `copy_to()` for MySQL if a character column contains `NA` (#1975, #2256, #2263, #2381, @demorenoc, @eduardgrebe).

* SQL joins always use the `ON ...` syntax, avoiding `USING ...` even for natural joins. Improved handling of tables with columns of the same name (#1997, @javierluraschi).

* Replace faulty `c4$query` suggestion with `sql_render(c4)` in vignette (#2246, @itcarroll).

* Now calling `dbFetch()` instead of the deprecated `fetch()` (#2134).
