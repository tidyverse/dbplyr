# dbplyr 2.5.0

## Improved tools for qualified table names

* Specification of table names with schema/catalogs has been overhauled to
  make it simpler. This includes the following features and fixes:

  * The simplest way to refer to a qualified table is now to wrap it in 
    `I()`, e.g. `I("schema_name.table_name")`.
    
  * Use of `sql()` and `ident_q()` inside `in_catalog()` and `in_schema()`
    is once again supported (#1388).
    
  * It's ok to use `ident_q()` once again (#1413) and you should no longer
    see unsuppressable warnings about using `in_schema()` (#1408).
    
  * The names of the arguments to `Id()` no longer matter, only their order 
    (#1416). Additionally, thanks to changes to the DBI package, you no 
    longer need to name each argument.
  
  * If you accidentally pass a named vector to any of the database identifer
    functions, those names will be automatically stripped (#1404).
    
  * `tbl_sql(check_from)` is now deprecated.

* dbplyr now exports some tools to work with the internal `table_path` class
  which is useful for certain backends that need to work with this 
  data structure (#1300).

## Improved SQL

* New translations for clock functions `add_years()`, `add_days()`, 
  `date_build()`, `get_year()`, `get_month()`, `get_day()`, 
  and `base::difftime()` on SQL server, Redshift, Snowflake, and Postgres.

* `select()` will keep computed columns used to `arrange()` subqueries that are
  eliminated by a subsequent select (@ejneer, #1437).

* `semi_join()` will no longer inline away an aggregate filter (i.e. `HAVING`
  clause) that was followed by a `select()` (@ejneer, #1474)

* Improved function translations:

  * Functions qualified with the base namespace are now also translated, e.g.
    `base::paste0(x, "_1")` is now translated (@mgirlich, #1022).

  * `-1 + x` now generates a translation instead erroring (#1420).

  * `x$name` never attempts to evaluate `name` (#1368).

  * You can once again use `NULL` on the LHS of an infix operator in order
    to generate SQL with unusual syntax (#1345).
    
  * Namespaced calls now error if the function doesn't exist, or a translation 
    is not available (#1426).

  * `lead()` translation coerces `n` to an integer.

* Databricks: now supports creating non-temporary tables too (#1418).

* Oracle:
    
  * `db_explain()` now works (@thomashulst, #1353).

  * `as.Date()` works when applied to a string (#1389).

  * `head()` is once again translated to `FETCH FIRST`. This does require Oracle 
    12c or newer, but it actually works, compared to the approach using 
    `ROWNUM` from #1292 (#1436).
    
  * Added support for `str_replace()` and `str_replace_all()` via
    `REGEXP_REPLACE()` (@thomashulst, #1402).

* Snowflake (@nathanhaigh, #1406)

  * Added support for `str_starts()` and `str_ends()` via `REGEXP_INSTR()`

  * Refactored `str_detect()` to use `REGEXP_INSTR()` so now supports
    regular expressions.

  * Refactored `grepl()` to use `REGEXP_INSTR()` so now supports
    case-insensitive matching through `grepl(..., ignore.case = TRUE)`

* SQL server: 

  * Now products a clear error if you attempt to use `n_distinct()` in 
    `mutate()` or `filter()` (#1366).

  * `filter()` does a better job of converting logical vectors 
    from bit to boolean (@ejneer, #1288). 

* MySQL: `as.integer()` gets correct translation (@krlmlr, #1375).

## Minor improvements and bug fixes

* Deprecation status of functions deprecated in previous versions (at least
  2 years old) have been advanced. In particular, `src_sql()` is now defunct,
  as is the use of `partial_eval()` with character `data`.

* Database errors now show the generated SQL, which hopefully will make it
  faster to track down problems (#1401).

* When dbplyr creates an index on a table in a schema (e.g. `schema.table`), 
  it now only includes the table name in the index name, not the schema name.

* The class of remote sources now includes all S4 class names, not just
  the first (#918).

* `compute()` passes additional arguments all the way to 
  `sql_query_save()`-methods (@rsund).

* `db_sql_render()` correctly passes on `...` when re-calling with 
  `sql_options` set (#1394).

* `reframe()` now gives an informative error that it isn't supported (#1148).

* `rows_patch(in_place = FALSE)` now works when more than one column should be
  patched (@gorcha, #1443).

* New `simulate_mariadb()` (@krlmlr, #1375).

* `sql_translator()` now checks for duplicated definitions (@krlmlr, #1374).

# dbplyr 2.4.0

## Breaking changes

* Using `compute(temporary = FALSE)` without providing a name is now
  deprecated (@mgirlich, #1154).

* `ntile()`'s first argument has been renamed from `order_by` to `x` to
  match the interface of `dplyr::ntile()` (@mgirlich, #1242).

* `simulate_vars()` and `simulate_vars_is_typed()` were removed as they weren't
  used and tidyselect now offers `tidyselect_data_proxy()` and
  `tidyselect_data_has_predicates()` (@mgirllich, #1199).

* `sql_not_supported()` now expects a function name without parentheses.

* `sql_query_append()`, `sql_query_insert()`, `sql_query_update()`,
  `sql_query_upsert()`, and `sql_query_delete()` changed their arguments to
  make them more consistent to the other `sql_query_*()` functions:
  
  * `x_name` was renamed to `table`.
  * `y` was renamed to `from` and must now be a table identifier or SQL instead
    of a lazy table.
  * `sql_query_append()` and `sql_query_insert()` have gained the argument `cols`.

* `remote_name()` now returns a string with the name of the table. To get the
  qualified identifier use the newly added `remote_table()` (@mgirlich, #1280).

* `tbl_lazy()` loses `src` argument after it has been deprecated for years
  (@mgirlich, #1208).

* `translate_sql()` now requires the `con` argument (@mgirlich, #1311).
  The `vars` argument has been removed after it threw an error for the last 7
  years (@mgirlich).

## Improved SQL

* Preliminary databricks Spark SQL backend (#1377).
  
* Joins

  * `*_join()` after `full_join()` works again (@mgirlich, #1178).

  * `*_join()` now allows specifying the relationship argument. It must be
    `NULL` or `"many-to-many"` (@bairdj, #1305).

  * Queries now qualify `*` with the table alias for better compatibility 
    (@mgirlich, #1003).

  * `full_join()` can now handle column names that only differ in case 
    (@ejneer, #1255).
  
  * The `na_matches` argument of `semi_join()` and `anti_join()` works again
    (@mgirlich, #1211).
    
  * A `semi/anti_join()` on fitlered `y` is inlined when possible (@mgirlich, #884).

  * Joins now work again for Pool and Oracle connections (@mgirlich, #1177, #1181).

* A sequence of `union()` resp. `union_all()` now produces a flat query
  instead of subqueries (@mgirlich, #1269).

* Added translations for:

  * `nzchar()` (@MichaelChirico, @mgirlich, #1094).
  * `str_detect()`, `str_starts()` and `str_ends()` with fixed patterns 
     (@mgirlich, #1009).
  * `runif()` (@mgirlich, #1200).

* `if_any()` and `if_all()` translations are now wrapped in parentheses.
  This makes sure it can be combined via `&` with other conditions 
  (@mgirlich, #1153).

* `nth()`, `first()`, and `last()` now support the `na_rm` argument 
  (@mgirlich, #1193).

## Minor improvements and bug fixes

* `across()` now supports namespaced functions, e.g. 
  `across(x, dplyr::dense_rank)` (@mgirlich, #1231).

* `db_copy_to(overwrite = TRUE)` now actually works.

* `db_copy_to()`'s `...` are now passed to `db_write_table()` (@mgirlich, #1237).

* Added `db_supports_table_alias_with_as()` to customise whether a backend 
  supports specifying a table alias with `AS` or not (@mgirlich).

* `db_write_table()` and `db_save_query()` gain the `overwrite` argument.

* `dbplyr_pivot_wider_spec()` is now exported. Unlike `pivot_wider()` this can
  be lazy. Note that this will be removed soon after `pivot_wider_spec()`
  becomes a generic (@mgirlich).

* `filter()`ing with window functions now generates columns called `col01` 
  rather than `q01` (@mgirlich, #1258).

* `pivot_wider()` now matches tidyr `NA` column handling (@ejneer #1238).

* `select()` can once again be used after `arrange(desc(x))` (@ejneer, #1240).

* `show_query()` and `remote_query()` gain the argument `sql_options` that allows
  to control how the SQL is generated. It can be created via `sql_options()`
  which has the following arguments:
  
    * `cte`: use common table expressions?
    * `use_star`: use `SELECT *` or explicitly select every column?
    * `qualify_all_columns`: qualify all columns in a join or only the ambiguous ones?
  (@mgirlich, #1146).
  
  Consequently the `cte` argument of `show_query()` and `remote_query()` has 
  been deprecated (@mgirlich, #1146).

* `slice_min/max()` can now order by multiple variables like dplyr, e.g. use
  `slice_min(lf, tibble(x, y))` (@mgirlich, #1167).

* `slice_*()` now supports the data masking pronouns `.env` and `.data` (@mgirlich, #1294).

* `sql_join_suffix()` gains the argument `suffix` so that methods can check
  whether the suffix is valid for the backend (@mgirlich).

* `sql_random()` is now deprecated. It was used to power `slice_sample()` which
  is now done via the translation for `runif()` (@mgirlich, #1200).
  
* `tbl()` now informs when the user probably forgot to wrap the table identifier
  with `in_schema()` or `sql()` (@mgirlich, #1287).

## Backend specific improvements

* Access
  * Added translation for `!=` to `<>` (@erikvona, #1219).

* DuckDB
  * now supports the `returning` argument of `rows_*()`.

* MySQL/MariaDB:
  * `rows_update()` and `rows_patch()` now give an informative error when the
    unsupported `returning` argument is used (@mgirlich, #1279).
  * `rows_upsert()` now gives an informative error that it isn't supported
    (@mgirlich, #1279).
  * `rows_*()` use the column types of `x` when auto copying `y` (@mgirlich, #1327).
  * `copy_inline()` now works (@mgirlich, #1188).
  * Fix translation of `as.numeric()`, `as.POSIXct()`, `as_datetime()`, and
    `as.integer64()` (@avsdev-cw, #1189).

* MS SQL:
  * `row_number()` now works when no order is specified (@ejneer, @fh-mthomson, #1332)

* Oracle:
  * Fix translation of `rows_upsert()` (@mgirlich, @TBlackmore, #1286)
  * `head(n)` is now translated to `WHERE ROWNUM <= n` to also support old
    versions <= 11.2 (@JeremyPasco, #1292).

* Postgres
  * The `rows_*()` functions now also work inside a transaction (@mgirlich, #1183).

* SQLite
  * Subqueries now also get an alias. This makes it consistent with other
    backends and simplifies the implementation.

* SQL Server
  * `distinct(.keep_all = TRUE)` now works (@mgirlich, #1053).
  * The translation of `between()` now also works when used in `mutate()`
    (@mgirlich, #1241).
  * `any()` and `all()` now work (@ejneer, #1273).
  * Fixed negation of bit (boolean) fields (@ejneer, #1239)

* Snowflake: 
  * `na.rm = TRUE` is now respected in `pmin()` and `pmax()` instead of being silently ignored (@fh-mthomson, #1329)
  * `row_number()` now works when no order is specified (@fh-mthomson, #1332)

* Teradata
  * `distinct()` + `head()` now work (@mgirlich, #685).
  * `as.Date(x)` is now translate to `CAST(x AS DATE)` again unless `x` is a
    string (@mgirlich, #1285).
  * `row_number()` no longer defaults to partitioning by groups (now aligned with other databases when no order is specified: `ROW_NUMBER()` defaults to `ORDER BY (SELECT NULL)`) (@fh-mthomson, #1331)

# dbplyr 2.3.4

* Hot patch release to resolve R CMD check failures.

# dbplyr 2.3.3

* Hot patch to fix R CMD check issues

# dbplyr 2.3.2

* Hot patch to fix R CMD check issues

# dbplyr 2.3.1

## Breaking changes

* `window_order()` now only accepts bare symbols or symbols wrapped in `desc()`.
  This breaking change is necessary to allow `select()` to drop and rename
  variables used in `window_order()` (@mgirlich, #1103).

## Improved error messages

* `quantile()` and `median()` now error for SQL Server when used in `summarise()`
  and for PostgreSQL when used in `mutate()` as they can't be properly
  translated (@mgirlich, #1110).

* Added an informative error for unsupported join arguments `unmatched` and
  `multiple` (@mgirlich).

* Using predicates, e.g. `where(is.integer)`, in `across()` now produces an
  error as they never worked anyway (@mgirlich, #1169).

* Catch unsupported argument `pivot_wider(id_expand = TRUE)` and
  `pivot_longer(cols_vary)` (@mgirlich, #1109).

## Bug fixes in SQL generation

* Fixed an issue when using a window function after a `summarise()` and
  `select()` (@mgirlich, #1104).

* Fixed an issue when there where at least 3 joins and renamed variables
  (@mgirlich, #1101).

* `mutate()` and `select()` after `distinct()` now again produce a subquery to
  generate the correct translation (@mgirlich, #1119, #1141).

* Fixed an issue when using `filter()` on a summarised variable (@mgirlich, #1128).

* `mutate()` + `filter()` now again produces a new query if the `mutate()`
  uses a window function or SQL (@mgirlich, #1135).

* `across()` and `pick()` can be used (again) in `distinct()` (@mgirlich, #1125).

* The `rows_*()` function work again for tables in a schema in PostgreSQL
  (@mgirlich, #1133).

## Minor improvements and bug fixes

* `sql()` now evaluates its arguments locally also when used in `across()` (@mgirlich, #1039).

* The rank functions (`row_number()`, `min_rank()`, `rank()`, `dense_rank()`,
  `percent_rank()`, and `cume_dist()`) now support multiple variables by
  wrapping them in `tibble()`, e.g. `rank(tibble(x, y))` (@mgirlich, #1118).

* `pull()` now supports the argument `name` (@mgirlich, #1136).

* Added support for `join_by()` added in dplyr 1.1.0 (@mgirlich, #1074).

* Using `by = character()` to perform a cross join is now soft-deprecated in
  favor of `cross_join()`.

* `full_join()` and `right_join()` are now translated directly to `FULL JOIN`
  and `RIGHT JOIN` for SQLite as native support was finally added (@mgirlich, #1150).

* `case_match()` now works with strings on the left hand side (@mgirlich, #1143).

* The rank functions (`row_number()`, `min_rank()`, `rank()`, `dense_rank()`,
  `percent_rank()`, and `cume_dist()`) now work again for variables wrapped in
  `desc()`, e.g. `row_number(desc(x))` (@mgirlich, #1118).

* Moved argument `auto_index` after `...` in `*_join()` (@mgirlich, #1115).

* Removed dependency on assertthat (@mgirlich, #1112).

* `across()` now uses the original value when a column is overridden to match
  the behaviour of dplyr. For example `mutate(df, across(c(x, y), ~ .x / x))`
  now produces
  
  ```
  SELECT `x` / `x` AS `x`, `y` / `x` AS `y`
  FROM `df`
  ```
  
  instead of
  
  ```
  SELECT `x`, `y` / `x` AS `y`
  FROM (
    SELECT `x` / `x` AS `x`, `y`
    FROM `df`
  ) 
  ```
  
  (@mgirlich, #1015).
  
* Restricted length of table aliases to avoid truncation on certain backends (e.g., Postgres) (@fh-mthomson, #1096)

# dbplyr 2.3.0

* Compatibility with purrr 1.0.0 (@mgirlich, #1085).

## New features

* `stringr::str_like()` (new in 1.5.0) is translated to the closest `LIKE` 
  equivalent (@rjpat, #509)

* In preparation for dplyr 1.1.0:

  * The `.by` argument is supported (@mgirlich, #1051).
  * Passing `...` to `across()` is deprecated because the evaluation timing 
    of `...` is ambiguous. Now instead of (e.g.) 
    `across(a:b, mean, na.rm = TRUE)` use  `across(a:b, \(x) mean(x, na.rm = TRUE)`
  * `pick()` is translated (@mgirlich, #1044).
  * `case_match()` is translated (@mgirlich, #1020).
  * `case_when()` now supports the `.default` argument (@mgirlich, #1017).

* Variables that aren't found in either the data or in the environment now
  produce an error (@mgirlich, #907).

## SQL optimisation

* dbplyr now produces fewer subqueries resulting in shorter, more readable, and,
  in some cases, faster SQL. The following combination of verbs now avoids a
  subquery if possible:

  * `*_join()` + `select()` (@mgirlich, #876).
  * `select()` + `*_join()` (@mgirlich, #875).
  * `mutate()` + `filter()` and `filter()` + `filter()` (@mgirlich, #792).
  * `distinct()` (@mgirlich, #880).
  * `summarise()` + `filter()` now translates to `HAVING` (@mgirlich, #877).
  * `left/inner_join()` + `left/inner_join()` (@mgirlich, #865).

* dbplyr now uses `SELECT *` after a join instead of explicitly selecting every 
  column, where possible (@mgirlich, #898).

* Joins only use the table aliases ("LHS" and "RHS") if necessary (@mgirlich).

* When using common table expressions, the results of joins and set operations
  are now reused (@mgirlich, #978).

## Improved error messages

* Many errors have been improved and now show the function where the error
  happened instead of a helper function (@mgirlich, #907).

* Errors produced by the database, e.g. in `collect()` or `rows_*()`, now show
  the verb where the error happened (@mgirlich).
  
* `window_order()` now produces a better error message when applied to a data
  frame (@mgirlich, #947).

* Using a named `across()` now gives a clear error message (@mgirlich, #761).

## Minor improvements and bug fixes

* Keyword highlighting can now be customised via the option `dbplyr_highlight`.
  Turn it off via `options(dbplyr_highlight = FALSE)` or pass a custom ansi
  style, e.g. `options(dbplyr_highlight = cli::combine_ansi_styles("bold", "cyan"))`
  (@mgirlich, #974).

* The rank functions (`row_number()`, `min_rank()`, `rank()`, `dense_rank()`,
  `percent_rank()`, and `cume_dist()`) now give missing values the rank NA to
  match the behaviour of dplyr (@mgirlich, #991).

* `NA`s in `blob()`s are correctly translated to `NULL` (#983).

* `copy_inline()` gains a `types` argument to specify the SQL column types
  (@mgirlich, #963).

* `cur_column()` is now supported (@mgirlich, #951).

* `distinct()` returns columns ordered the way you request, not the same
  as the input data (@mgirlich).

* `fill()` can now fill "downup" and "updown" (@mgirlich, #1057), and
  now order by non-numeric columns also in the up direction (@mgirlich, #1057).

* `filter()` now works when using a window function and an external vector
  (#1048).

* `group_by()` + renamed columns works once again (@mgirlich, #928).

* `last()` is correctly translated when no window frame is specified
  (@mgirlich, #1063).

* `setOldClass()` uses a namespace, fixing an installation issue (@mgirlich, #927).
  
* `sql()` is now translated differently. The `...` are now evaluated locally 
  instead of being translated with `translate_sql()` (@mgirlich, #952).


## Backend specific improvements

* HANA:
  * Correctly translates `as.character()` (#1027).
  * `copy_inline()` now works for Hana (#950)

* MySQL:
  * `str_flatten()` uses `collapse = ""` by default (@fh-afrachioni, #993)

* Oracle:
  * `slice_sample()` now works for Oracle (@mgirlich, #986).
  * `copy_inline()` now works for Oracle (#972)

* PostgreSQL:
  * Generates correct literals for Dates (#727).
  * `str_flatten()` uses `collapse = ""` by default (@fh-afrachioni, #993)
  * `rows_*()` use the column types of `x` when auto copying (@mgirlich, #909).

* Redshift:
  * `round()` now respects the `digits` argument (@owenjonesuob, #1033).
  * No longer tries to use named windows anymore (@owenjonesuob, #1035).
  * `copy_inline()` now works for Redshift (#949, thanks to @ejneer for an 
    initial implementation).
  * `str_flatten()` uses `collapse = ""` by default (@fh-afrachioni, #993)

*  Snowflake:
  * numeric functions: `all()`, `any()`, `log10()`, `round()`, `cor()`, `cov()`
    and `sd()`.
  * date functions: `day()`, `mday()`, `wday()`, `yday()`, `week()`,
    `isoweek()`, `month()`, `quarter()`, `isoyear()`, `seconds()`, `minutes()`,
    `hours()`, `days()`, `weeks()`, `months()`, `years()` and `floor_date()`.
  * string functions: `grepl()`, `paste()`, `paste0()`, `str_c()`, `str_locate()`,
    `str_detect()`, `str_replace()`, `str_replace_all()`, `str_remove()`,
    `str_remove_all()`, `str_trim()`, `str_squish()` and `str_flatten()`
    (@fh-afrachioni, #860).
  * `str_flatten()` uses `collapse = ""` by default (@fh-afrachioni, #993)

* SQLite:
  * `quantile()` gives a better error saying that it is not supported 
    (@mgirlich, #1000).

* SQL server:
  * `as.POSIXct()` now translated correctly (@krlmlr, #1011).
  * `median()` now translated correctly (#1008).
  * `pivot_wider()` works again for MS SQL (@mgirlich, #929).
  * Always use 1 and 0 as literals for logicals (@krlmlr, #934).

* Teradata: 
  * Querying works again. Unfortunately, the fix requires every column to 
    once again by explicitly selected (@mgirlich, #966).
  * New translations for `as.Date()`, `week()`, `quarter()`, `paste()`,
    `startsWith()`, `row_number()`, `weighted.mean()`, `lead()`, `lag()`, and
    `cumsum()` (@overmar, #913).


# dbplyr 2.2.1

* Querying Oracle databases works again. Unfortunately, the fix requires every
  column to be explicitly selected again (@mgirlich, #908).

* `semi_join()` and `anti_join()` work again for Spark (@mgirlich, #915).

* `str_c()` is now translated to `||` in Oracle (@mgirlich, #921).

* `sd()`, `var()`, `cor()` and `cov()` now give clear error messages on 
  databases that don't support them.
  
* `any()` and `all()` gain default translations for all backends.

# dbplyr 2.2.0

## New features

* SQL formatting has been considerably improved with new wrapping and indenting. 
  `show_query()` creates more readable queries by printing the keywords in blue 
  (@mgirlich, #644). When possible dbplyr now uses `SELECT *` instead of 
  explicitly selecting every column (@mgirlich).
  
* Added support for `rows_insert()`, `rows_append()`, `rows_update()`, 
  `rows_patch()`, `rows_upsert()`, and `rows_delete()` (@mgirlich, #736).

* Added `copy_inline()` as a `copy_to()` equivalent that does not need write
  access (@mgirlich, #628).

* `remote_query()`, `show_query()`, `compute()` and `collect()` have an
  experimental `cte` argument. If `TRUE` the SQL query will use common table
  expressions instead of nested queries (@mgirlich, #638).

* New `in_catalog()`, which works like `in_schema()`, but allows creation of 
  table identifiers consisting of three components: catalog, schema, name 
  (#806, @krlmlr).

## Improvements to SQL generation

* When possible, dbplyr now uses `SELECT *` instead of explicitly selecting 
  every column (@mgirlich).

* New translation for `cut()` (@mgirlich, #697).

* Improved translations for specific backends:
  * `as.Date()` for Oracle (@mgirlich, #661).
  * `case_when()` with a final clause of the form `TRUE ~ ...` uses `ELSE ...` 
     for SQLite (@mgirlich, #754).
  * `day()`, `week()`, `isoweek()`, and `isoyear()` for Postgres (@mgirlich, #675).
  * `explain()` for ROracle (@mgirlich).
  * `fill()` for SQL Server (#651, @mgirlich) and RPostgreSQL (@mgirlich).
  * `quantile()` for SQL Server (@mgirlich, #620).
  * `str_flatten()` for Redshift (@hdplsa, #804) 
  * `slice_sample()` for MySQL/MariaDB and SQL Server (@mgirlich, #617).
  * `union()` for Hive (@mgirlich, #663).

* The backend function `dbplyr_fill0()` (used for databases that lack 
  `IGNORE NULLS` support) now respects database specific translations 
  (@rsund, #753).

* Calls of the form `stringr::foo()` or `lubridate::foo()` are now evaluated in
  the database, rather than locally (#197).

* Unary plus (e.g. `db %>% filter(x == +1)`) now works (@mgirlich, #674).

* `is.na()`, `ifelse()`, `if_else()`, `case_when()`, and `if()` 
  generate slightly more compact SQL (@mgirlich, #738).

* `if_else()` now supports the `missing` argument (@mgirlich, #641).

* `n()` now respects the window frame (@mgirlich, #700).

* `quantile()` no longer errors when using the `na.rm` argument (@mgirlich, #600).

* `remote_name()` now returns a name in more cases where it makes sense
  (@mgirlich, #850).

* The partial evaluation code is now more aligned with `dtplyr`. This makes it
  easier to transfer bug fixes and new features from one package to the other.
  In this process the second argument of `partial_eval()` was changed to a lazy
  frame instead of a character vector of variables (@mgirlich, #766).
  Partially evaluated expressions with infix operations are now correctly
  translated. For example `translate_sql(!!expr(2 - 1) * x)` now works
  (@mgirlich, #634).

## Minor improvements and bug fixes

* New `pillar::tbl_format_header()` method for lazy tables: Printing a lazy 
  table where all rows are displayed also shows the exact number of rows in the 
  header. The threshold is controlled by `getOption("pillar.print_min")`, 
  with a default of 10 (#796, @krlmlr).

* The 1st edition extension mechanism is formally deprecated (#507).

* `across()`, `if_any()` and `if_all()` now defaults to `.cols = everything()`
  (@mgirlich, #760). If `.fns` is not provided `if_any()` and `if_all()` work 
  like a parallel version of `any()`/`any()` (@mgirlich, #734).

* `across()`, `if_any()`, and `if_all()` can now translate evaluated lists
  and functions (@mgirlich, #796), and accept the name of a list of functions 
  (@mgirlich, #817).

* Multiple `across()` calls in `mutate()` and `transmute()` can now access
  freshly created variables (@mgirlich, #802).

* `add_count()` now doesn't change the groups of the input (@mgirlich, #614).

* `compute()` can now handle when `name` is named by unnaming it first
  (@mgirlich, #623), and now works when `temporary = TRUE` for Oracle 
  (@mgirlich, #621).

* `distinct()` now supports `.keep_all = TRUE` (@mgirlich, #756).

* `expand()` now works in DuckDB (@mgirlich, #712).

* `explain()` passes `...` to methods (@mgirlich, #783), and 
  works for Redshift (@mgirlich, #740).

* `filter()` throws an error if you supply a named argument (@mgirlich, #764).

* Joins disambiguates columns that only differ in case (@mgirlich, #702).
  New arguments `x_as` and `y_as` allow you to control the table alias 
  used in SQL query (@mgirlich, #637). Joins with `na_matches = "na"` now work 
  for DuckDB (@mgirlich, #704).

* `mutate()` and `transmute()` use named windows if a window definition is 
  used at least twice and the backend supports named windows (@mgirlich, #624).

* `mutate()` now supports the arguments `.keep`, `.before`, and `.after`
  (@mgirlich, #802).

* `na.rm = FALSE` only warns once every 8 hours across all functions (#899).

* `nesting()` now supports the `.name_repair` argument (@mgirlich, #654).

* `pivot_longer()` can now pivot a column named `name` (@mgirlich, #692),
  can repair names (@mgirlich, #694), and can work with multiple `names_from` 
  columns (@mgirlich, #693).

* `pivot_wider(values_fn = )` and `pivot_longer(values_transform = )`
  can now be formulas (@mgirlich, #745).

* `pivot_wider()` now supports the arguments `names_vary`, `names_expand`, and
  `unused_fn` (@mgirlich, #774).

* `remote_name()` now returns a name in more cases where it makes sense
  (@mgirlich, #850).

* `sql_random()` is now exported.

* `ungroup()` removes variables in `...` from grouping (@mgirlich, #689).

* `transmute()` now keeps grouping variables (@mgirlich, #802).

# dbplyr 2.1.1

* New support for Snowflake (@edgararuiz)

* `compute()`, `sql_table_index()`, and `sql_query_wrap()` now work with
  schemas (@mgirlich, #595).

* `if_any()` and `if_all()` are now translated.

* `group_by()` now ungroups when the dots argument is empty and `.add` is `FALSE`
  (@mgirlich, #615).

* `sql_escape_date()` and `sql_escape_datetime` gain methods for MS Access 
  (@erikvona, #608).

# dbplyr 2.1.0

## New features

* Thanks to @mgirlich, dbplyr gains support for key verbs from tidyr:
  `pivot_longer()` (#532), `pivot_wider()` (#543), `expand()` (#538), 
  `complete()` (#538), `replace_na()` (#538), `fill()` (#566).

* @mgirlich is now a dbplyr author in recognition of his significant and
  sustained contributions.

* `across()` implementation has been rewritten to support more inputs:
  it now translates formulas (#525), works with SQL functions that don't have
  R translations (#534), and work with `NULL` (#554)

* `summarise()` now supports argument `.groups` (@mgirlich, #584).

## SQL translation

* All backends: `str_sub()`, `substr()` and `substring()` get better 
  translations (#577). Most importantly, the results of using negative 
    locations should match the underlying R implementations more closely.

* MS SQL:

  * `as.integer()` and `as.integer64()` translations cast first to `NUMERIC` 
     to avoid CASTing weirdness (@DavidPatShuiFong, #496).
     
  * Assumes a boolean context inside of `[` (#546)
  
  * `str_sub()` with `end = -1` now works (#577).

* Redshift: `lag()` and `lead()` lose the `default` parameter  since it's 
  not supported (@hdplsa, #548).

* SQLite: custom translation of `full_join()` and `right_join()` 
  (@mgirlich, #536).
  

## Minor improvements and bug fixes

* RPostgreSQL backend warns if `temporary = TRUE` since temporary tables are 
  not supported by `RPostgreSQL::dbWriteTable()` (#574).

* `count()` method provides closer match to dplyr semantics (#347).

* `distinct()` now respects grouping (@mgirlich, #535).

* `db_connection_describe()` no longer uses partial matching (@mgirlich, #564).

* `pull()` no longer `select()`s the result when there's already only 
  one variable (#562).

* `select()` no longer relocates grouping variables to the front 
  (@mgirlich, #568). and informs when adding missing grouping variables 
  (@mgirlich, #559).

* `tbl.src_dbi(...)` now passed on to `tbl_sql()` (#530).

# dbplyr 2.0.0

## dplyr 1.0.0 compatibility

* `across()` is now translated into individual SQL statements (#480).

* `rename()` and `select()` support dplyr 1.0.0 tidyselect syntax (apart from
  predicate functions which can't easily work on computed queries) (#502).
  
* `relocate()` makes it easy to move columns (#494) and `rename_with()` makes
  it easy to rename columns programmatically (#502).

* `slice_min()`, `slice_max()`, and `slice_order()` are now supported.
  `slice_head()` and `slice_tail()` throw clear error messages (#394)

## SQL generation

* Documentation has been radically improved with new topics for each major 
  verb and each backend giving more details about the SQL translation.

* `intersect()`, `union()` and `setdiff()` gain an `all` argument to add the
   `ALL` argument (#414).

* Join functions gains a `na_matches` argument that allows you to control 
  whether or not `NA` (`NULL`) values match other `NA` values. The default is
  `"never"`, which is the usual behaviour in databases. You can set 
  `na_matches = "na"` to match R's usual join behaviour (#180). Additional
  arguments error (instead of being silently swallowed) (#382).
  
* Joins now only use aliases where needed to disambiguate columns; this should 
  make generated queries more readable.

* Subqueries no longer include an `ORDER BY` clause. This is not part of the 
  SQL spec, and has very limited support across databases. Now such queries
  generate a warning suggesting that you move your `arrange()` call later in 
  the pipeline (#276). (There's one exception: `ORDER BY` is still generated 
  if `LIMIT` is present; this tends to affect the returns rows but not
  necessarily their order).

* Subquery names are now scoped within the query. This makes query text 
  deterministic which helps some query optimisers/cachers (#336).

* `sql_optimise()` now can partially optimise a pipeline; due to an unfortunate
  bug it previously gave up too easily.

* `in_schema()` quotes each input individually (#287) (use `sql()` to opt out 
  of quoting, if needed). And `DBI::Id()` should work anywhere that 
  `in_schema()` does.

## SQL translation

* Experimental new SAP HANA backend (#233). Requires the latest version of odbc.

* All backends:

  * You can now use `::` in translations, so that (e.g.) `dbplyr::n()` is
    translated to `count(*)` (#207).
    
  * `[[` can now also translate numeric indices (#520).
  
  * `%/%` now generates a clear error message; previously it was translated to
    `/` which is not correct (#108).

  * `n()` is translated to `count(*)` instead of `count()` (#343). 

  * `sub_str()` translation is more consistent in edge cases (@ianmcook).
    
  * All `median()` (@lorenzwalthert, #483), `pmin()`, `pmax()` (#479), `sd()` 
    and `var()` functions have an `na.rm` argument that warns once when not
    `TRUE`. This makes them consistent with `mean()` and `sum()`.

  * `substring()` is now translated the same way as `substr()` (#378).
    
* [blob](https://blob.tidyverse.org/) vectors can now be used with `!!` and 
  `!!!` operators, for example in `filter()` (@okhoma, #433)

* MySQL uses standard SQL for index creation.

* MS SQL translation does better a distinguishing between bit and boolean 
  (#377, #318). `if` and `ifelse` once again generate `IIF`, creating
  simpler expressions. `as.*()` function uses `TRY_CAST()` instead
  of `CAST()` for version 11+ (2012+) (@DavidPatShuiFong, #380).

* odbc no longer translates `count()`; this was an accidental inclusion.

* Oracle translation now depends on Oracle 12c, and uses a "row-limiting" 
  clause for `head()`. It gains translations for `today()` and `now()`, and
  improved `as.Date()` translation (@rlh1994, #267).

* PostgreSQL: new translations for lubridate period functions `years()`,
  `months()`, `days()`, and `floor_date()` (@bkkkk, #333) and stringr functions
  `str_squish()`,  `str_remove()`, and `str_remove_all()` (@shosaco).

* New RedShift translations when used with `RPostgres::Redshift()`.

  * `str_replace()` errors since there's no Redshift translation, 
     and `str_replace_all()` uses `REGEXP_REPLACE()` (#446).
     
  * `paste()` and `paste0()` use `||` (#458).
  
  * `as.numeric()` and `as.double()` cast to `FLOAT` (#408).
  
  * `substr()` and `str_sub()` use `SUBSTRING()` (#327).

* SQLite gains translations for lubridate functions `today()`, `now()`, 
  `year()`, `month()`, `day()`, `hour()`, `minute()`, `second()`,`yday()`
  (#262), and correct translation for `median()` (#357).

## Extensibility

If you are the author of a dbplyr backend, please see `vignette("backend-2")` for details.

*   New `dbplyr_edition()` generic allows you to opt-in to the 2nd edition of 
    the dbplyr API.

*   `db_write_table()` now calls `DBI::dbWriteTable()` instead of nine generics
    that formerly each did a small part: `db_create_indexes()`, `db_begin()`,
    `db_rollback()`, `db_commit()`, `db_list_tables()`, `drop_drop_table()`,
    `db_has_table()`, `db_create_table()`, and `db_data_types()`. You can 
    now delete the methods for these generics.
    
    `db_query_rows()` is no longer used; it appears that it hasn't been used 
    for some time, so if you have a method, you can delete it.

*   `DBI::dbQuoteIdentifier()` is now used instead of `sql_escape_ident()` and
    `DBI::dbQuoteString()` instead of `sql_escape_string()`.  

*   A number of `db_*` generics have been replaced with new SQL generation
    generics:

    * `dplyr::db_analyze()` -> `dbplyr::sql_table_analyze()`
    * `dplyr::db_create_index()` -> `dbplyr::sql_table_index()`
    * `dplyr::db_explain()` -> `dbplyr::sql_queriy_explain()` 
    * `dplyr::db_query_fields()` -> `dbplyr::sql_query_fields()`
    * `dplyr::db_save_query()` -> `dbplyr::sql_query_save()`
  
    This makes them easier to test and is an important part of the process of
    moving all database generics in dbplyr (#284).

*   A number of other generics have been renamed to facilitate the move from
    dplyr to dbplyr:

    * `dplyr::sql_select()` -> `dbplyr::sql_query_select()`
    * `dplyr::sql_join()` -> `dbplyr::sql_query_join()`
    * `dplyr::sql_semi_join()` -> `dbplyr::sql_query_semi_join()`
    * `dplyr::sql_set_op()` -> `dbplyr::sql_query_set_op()`
    * `dplyr::sql_subquery()` -> `dbplyr::sql_query_wrap()`
    * `dplyr::db_desc()` -> `dbplyr::db_connection_describe()`
  
*   New `db_temporary_table()` generic makes it easier to work with databases
    that require temporary tables to be specially named.

*   New `sql_expr_matches()` generic allows databases to use more efficient
    alternatives when determine if two values "match" (i.e. like equality but 
    a pair of `NULL`s will also match). For more details, see
    <https://modern-sql.com/feature/is-distinct-from>

*   New `sql_join_suffix()` allows backends to control the default suffixes 
    used (#254).

## Minor improvements and bug fixes

* All old lazy eval shims have been removed. These have been deprecated for
  some time.

* Date-time escaping methods for Athena and Presto have moved to the packages
  where they belong.

* Attempting to embed a Shiny reactive in a query now gives a helpful error
  (#439).

* `copy_lahman()` and `copy_nycflights13()` (and hence `nycflights13_sqlite()`)
  and friends now return DBI connections rather than the now deprecated 
  `src_dbi()` (#440).

* `copy_to()` can now `overwrite` when table is specified with schema (#489),
  and gains an `in_transaction` argument used to optionally suppress the
  transaction wrapper (#368).

* `distinct()` no longer duplicates column if grouped (#354).

* `transmute()` now correctly tracks variables it needs when creating
  subqueries (#313).

* `mutate()` grouping variables no longer generates a downstream error (#396)

* `mutate()` correctly generates subqueries when you re-use the same variable
  three or more times (#412).

* `window_order()` overrides ordering, rather than appending to it.

# dbplyr 1.4.4

* Internally `DBI::dbExecute()` now uses `immediate = TRUE`; this improves
  support for session-scoped temporary tables in MS SQL (@krlmlr, #438).

* Subqueries with `ORDER BY` use `TOP 9223372036854775807` instead of 
  `TOP 100 PERCENT` on SQL Server for compatibility with Azure Data Warehouse 
  (#337, @alexkyllo).

* `escape()` now supports `blob` vectors using new `sql_escape_raw()` 
  generic. It enables using [blob](https://blob.tidyverse.org/) variables in 
  dplyr verbs, for example to filter nvarchar values by UTF-16 blobs
  (see https://github.com/r-dbi/DBI/issues/215#issuecomment-356376133). 
  (@okhoma, #433)

* Added `setOldClass()` calls for `"ident"` and `"ident_q"` classes for 
  compatibility with dplyr 1.0.0 (#448, @krlmlr).

* Postgres `str_detect()` translation uses same argument names as stringr,
  and gains a `negate` argument (#444).

* `semi_join()` and `anti_join()` now correctly support the `sql_on` argument 
  (#443, @krlmlr).

# dbplyr 1.4.3

* dbplyr now uses RPostgres (instead of RPostgreSQL) and RMariaDB (instead of 
  RMySQL) for its internal tests and data functions (#427).

* The Date and POSIXt methods for `escape()` now use exported 
  `sql_escape_date()` and `sql_escape_datetime()` generics to allow backend
  specific formatting of date and datetime literals. These are used to
  provide methods for Athena and Presto backends (@OssiLehtinen, #384, #391).

* `first()`, `last()`, `nth()`, `lead()` and `lag()` now respect the
  `window_frame()` (@krlmlr, #366).

* SQL server: new translations for `str_flatten()` (@PauloJhonny, #405).

* SQL server: temporary datasets are now session-local, not global (#401).

* Postgres: correct `str_detect()`, `str_replace()` and `str_replace_all()` 
  translation (@shosaco, #362).

# dbplyr 1.4.2

* Fix bug when partially evaluating unquoting quosure containing a single 
  symbol (#317)

* Fixes for rlang and dpylr compatibility.

# dbplyr 1.4.1

Minor improvements to SQL generation

* `x %in% y` strips names of `y` (#269).

* Enhancements for scoped verbs (`mutate_all()`, `summarise_if()`,
  `filter_at()` etc) (#296, #306).

* MS SQL use `TOP 100 PERCENT` as stop-gap to allow subqueries with 
  `ORDER BY` (#277).

* Window functions now translated correctly for Hive (#293, @cderv).

# dbplyr 1.4.0

## Breaking changes

* ``Error: `con` must not be NULL``: If you see this error, it probably means 
  that you have forgotten to pass `con` down to a dbplyr function. 
  Previously, dbplyr defaulted to using `simulate_dbi()` which introduced
  subtle escaping bugs. (It's also possible I have forgotten to pass it 
  somewhere that the dbplyr tests don't pick up, so if you can't figure it 
  out, please let me know).

* Subsetting (`[[`, `$`, and `[`) functions are no longer evaluated locally. 
  This makes the translation more consistent and enables useful new idioms 
  for modern databases (#200).

## New features

* MySQL/MariaDB (https://mariadb.com/kb/en/library/window-functions/) 
  and SQLite (https://www.sqlite.org/windowfunctions.html) translations gain 
  support for window functions, available in Maria DB 10.2, MySQL 8.0, and 
  SQLite 3.25 (#191).

* Overall, dplyr generates many fewer subqueries:

  * Joins and semi-joins no longer add an unneeded subquery (#236). This is
    facilitated by the new `bare_identifier_ok` argument to `sql_render()`;
    the previous argument was called `root` and confused me.
  
  * Many sequences of `select()`, `rename()`, `mutate()`, and `transmute()` can
    be collapsed into a single query, instead of always generating a subquery
    (#213).

* New `vignette("sql")` describes some advantages of dbplyr over SQL (#205) and 
  gives some advice about writing literal SQL inside of dplyr, when you need 
  to (#196).

* New `vignette("reprex")` gives some hints on creating reprexes that work 
  anywhere (#117). This is supported by a new `tbl_memdb()` that matches the 
  existing `tbl_lazy()`.

* All `..._join()` functions gain an `sql_on` argument that allows specifying
  arbitrary join predicates in SQL code (#146, @krlmlr).

## SQL translations

* New translations for some lubridate functions: `today()`, `now()`, 
  `year()`, `month()`, `day()`, `hour()`, `minute()`,
  `second()`, `quarter()`, `yday()` (@colearendt, @derekmorr). Also added new 
  translation for `as.POSIXct()`.

* New translations for stringr functions: `str_c()`, `str_sub()`, 
  `str_length()`, `str_to_upper()`, `str_to_lower()`, and `str_to_title()`
  (@colearendt). Non-translated stringr functions throw a clear error.

* New translations for bitwise operations: `bitwNot()`, `bitwAnd()`, `bitwOr()`,
  `bitwXor()`, `bitwShiftL()`, and `bitwShiftR()`. Unlike the base R functions, 
  the translations do not coerce arguments to integers (@davidchall, #235).

* New translation for `x[y]` to `CASE WHEN y THEN x END`. This enables 
  `sum(a[b == 0])` to work as you expect from R (#202). `y` needs to be
  a logical expression; if not you will likely get a type error from your 
  database.

* New translations for `x$y` and `x[["y"]]` to `x.y`, enabling you to index
  into nested fields in databases that provide them (#158).

* The `.data` and `.env` pronouns of tidy evaluation are correctly translated 
  (#132).

* New translation for `median()` and `quantile()`. Works for all ANSI compliant
  databases (SQL Server, Postgres, MariaDB, Teradata) and has custom 
  translations for Hive. Thanks to @edavidaja for researching the SQL variants! 
  (#169)
  
* `na_if()` is correct translated to `NULLIF()` (rather than `NULL_IF`) (#211).

* `n_distinct()` translation throws an error when given more than one argument.
  (#101, #133).

* New default translations for `paste()`, `paste0()`, and the hyperbolic 
  functions (these previously were only available for ODBC databases).

* Corrected translations of `pmin()` and `pmax()` to `LEAST()` and `GREATEST()` 
  for ANSI compliant databases (#118), to `MIN()` and `MAX()` for SQLite, and 
  to an error for SQL server.

* New translation for `switch()` to the simple form of `CASE WHEN` (#192).

### SQL simulation

SQL simulation makes it possible to see what dbplyr will translate SQL to,
without having an active database connection, and is used for testing and
generating reprexes.

* SQL simulation has been overhauled. It now works reliably, is better 
  documented, and always uses ANSI escaping (i.e. `` ` `` for field 
  names and `'` for strings).

* `tbl_lazy()` now actually puts a `dbplyr::src` in the `$src` field. This
  shouldn't affect any downstream code unless you were previously working
  around this weird difference between `tbl_lazy` and `tbl_sql` classes.
  It also includes the `src` class in its class, and when printed,
  shows the generated SQL (#111).

## Database specific improvements

* MySQL/MariaDB

  * Translations also applied to connections via the odbc package 
    (@colearendt, #238)
  
  * Basic support for regular expressions via `str_detect()` and  
    `str_replace_all()` (@colearendt, #168).

  * Improved translation for `as.logical(x)` to `IF(x, TRUE, FALSE)`.

* Oracle

  * New custom translation for `paste()` and `paste0()` (@cderv, #221)

* Postgres

  * Basic support for regular expressions via `str_detect()` and  
    `str_replace_all()` (@colearendt, #168).

* SQLite

  * `explain()` translation now generates `EXPLAIN QUERY PLAN` which
    generates a higher-level, more human friendly explanation.

* SQL server

  * Improved translation for `as.logical(x)` to `CAST(x as BIT)` (#250).
    
  * Translates `paste()`, `paste0()`, and `str_c()` to `+`.

  * `copy_to()` method applies temporary table name transformation
    earlier so that you can now overwrite temporary tables (#258).

  * `db_write_table()` method uses correct argument name for 
    passing along field types (#251).

## Minor improvements and bug fixes

* Aggregation functions only warn once per session about the use of 
  `na.rm = TRUE` (#216).

* table names generated by `random_table_name()` have the prefix 
  "dbplyr_", which makes it easier to find them programmatically 
  (@mattle24, #111)

* Functions that are only available in a windowed (`mutate()`) query now
  throw an error when called in a aggregate (`summarise()`) query (#129)

* `arrange()` understands the `.by_group` argument, making it possible
  sort by groups if desired. The default is `FALSE` (#115)

* `distinct()` now handles computed variables like `distinct(df, y = x + y)` 
  (#154).

* `escape()`, `sql_expr()` and `build_sql()` no longer accept `con = NULL` as 
  a shortcut for `con = simulate_dbi()`. This made it too easy to forget to 
  pass `con` along, introducing extremely subtle escaping bugs. `win_over()`
  gains a `con` argument for the same reason.
  
* New `escape_ansi()` always uses ANSI SQL 92 standard escaping (for use 
  in examples and documentation).

* `mutate(df, x = NULL)` drops `x` from the output, just like when working with
  local data frames (#194).

* `partial_eval()` processes inlined functions (including rlang lambda 
  functions). This makes dbplyr work with more forms of scoped verbs like
  `df %>% summarise_all(~ mean(.))`, `df %>% summarise_all(list(mean))` (#134).

* `sql_aggregate()` now takes an optional argument `f_r` for passing to
  `check_na_rm()`. This allows the warning to show the R function name rather 
  than the SQL function name (@sverchkov, #153).

* `sql_infix()` gains a `pad` argument for the rare operator that doesn't
  need to be surrounded by spaces.

* `sql_prefix()` no longer turns SQL functions into uppercase, allowing for 
  correct translation of case-sensitive SQL functions (#181, @mtoto).

* `summarise()` gives a clear error message if you refer to a variable 
  created in that same `summarise()` (#114).

* New `sql_call2()` which is to `rlang::call2()` as `sql_expr()` is to 
  `rlang::expr()`.

* `show_query()` and `explain()` use `cat()` rather than message.

* `union()`, `union_all()`, `setdiff()` and `intersect()` do a better job
  of matching columns across backends (#183).

# dbplyr 1.3.0

* Now supports for dplyr 0.8.0 (#190) and R 3.1.0

## API changes

* Calls of the form `dplyr::foo()` are now evaluated in the database, 
  rather than locally (#197).

* `vars` argument to `tbl_sql()` has been formally deprecated; it hasn't 
  actually done anything for a while (#3254).

* `src` and `tbl` objects now include a class generated from the class of 
  the underlying connection object. This makes it possible for dplyr backends 
  to implement different behaviour at the dplyr level, when needed. (#2293)

## SQL translation

* `x %in% y` is now translated to `FALSE` if `y` is empty (@mgirlich, #160).

* New `as.integer64(x)` translation to `CAST(x AS BIGINT)` (#3305)

* `case_when` now translates with a ELSE clause if a formula of the form 
  `TRUE~<RHS>` is provided . (@cderv, #112)

* `cummean()` now generates `AVG()` not `MEAN()` (#157)

* `str_detect()` now uses correct parameter order (#3397)

* MS SQL
    * Cumulative summary functions now work (#157)
    * `ifelse()` uses `CASE WHEN` instead of `IIF`; this allows more complex 
       operations, such as `%in%`, to work properly (#93)

* Oracle
    * Custom `db_drop_table()` now only drops tables if they exist (#3306)
    * Custom `setdiff()` translation  (#3493)
    * Custom `db_explain()` translation (#3471)

* SQLite
  * Correct translation for `as.numeric()`/`as.double()` (@chris-park, #171).

* Redshift 
  * `substr()` translation improved (#3339)

## Minor improvements and bug fixes

* `copy_to()` will only remove existing table when `overwrite = TRUE` and the
  table already exists, eliminating a confusing "NOTICE" from PostgreSQL 
  (#3197).

* `partial_eval()` handles unevaluated formulas (#184).

* `pull.tbl_sql()` now extracts correctly from grouped tables (#3562).

* `sql_render.op()` now correctly forwards the `con` argument (@kevinykuo, #73).

# dbplyr 1.2.2

* R CMD check fixes

# dbplyr 1.2.1

* Forward compatibility fixes for rlang 0.2.0

# dbplyr 1.2.0

## New top-level translations

* New translations for 
  
    * MS Access (#2946) (@DavisVaughan)
    * Oracle, via odbc or ROracle (#2928, #2732, @edgararuiz)
    * Teradata. 
    * Redshift.

* dbplyr now supplies appropriate translations for the RMariaDB and 
  RPostgres packages (#3154). We generally recommend using these packages
  in favour of the older RMySQL and RPostgreSQL packages as they are
  fully DBI compliant and tested with DBItest.

## New features

* `copy_to()` can now "copy" tbl_sql in the same src, providing another
  way to cache a query into a temporary table (#3064). You can also 
  `copy_to` tbl_sqls from another source, and `copy_to()` will automatically
  collect then copy.

* Initial support for stringr functions: `str_length()`, `str_to_upper()`,
  `str_to_lower()`, `str_replace_all()`, `str_detect()`, `str_trim()`. 
  Regular expression support varies from database to database, but most 
  simple regular expressions should be ok.

## Tools for developers

* `db_compute()` gains an `analyze` argument to match `db_copy_to()`.

* New `remote_name()`, `remote_con()`, `remote_src()`, `remote_query()` and 
  `remote_query_plan()` provide a standard API for get metadata about a 
  remote tbl (#3130, #2923, #2824).

* New `sql_expr()` is a more convenient building block for low-level SQL
  translation (#3169).

* New `sql_aggregate()` and `win_aggregate()` for generating SQL and windowed
  SQL functions for aggregates. These take one argument, `x`, and warn if 
  `na.rm` is not `TRUE` (#3155). `win_recycled()` is equivalent to 
  `win_aggregate()` and has been soft-deprecated.
  
* `db_write_table` now needs to return the table name

## Minor improvements and bug fixes

* Multiple `head()` calls in a row now collapse to a single call. This avoids 
  a printing problem with MS SQL (#3084).

* `escape()` now works with integer64 values from the bit64 package (#3230)

* `if`, `ifelse()`, and `if_else()` now correctly scope the false condition
  so that it only applies to non-NULL conditions (#3157)

* `ident()` and `ident_q()` handle 0-length inputs better, and should
  be easier to use with S3 (#3212)

* `in_schema()` should now work in more places, particularly in `copy_to()` 
   (#3013, @baileych)

* SQL generation for joins no longer gets stuck in a endless loop if you
  request an empty suffix (#3220).

* `mutate()` has better logic for splitting a single mutate into multiple
  subqueries (#3095).

* Improved `paste()` and `paste0()` support in MySQL, PostgreSQL (#3168),
  and RSQLite (#3176). MySQL and PostgreSQL gain support for `str_flatten()` 
  which behaves like `paste(x, collapse = "-")` (but for technical reasons 
  can't be implemented as a straightforward translation of `paste()`).

* `same_src.tbl_sql()` now performs correct comparison instead of always 
  returning `TRUE`. This means that `copy = TRUE` once again allows you to
  perform cross-database joins (#3002).

* `select()` queries no longer alias column names unnecessarily 
  (#2968, @DavisVaughan).

* `select()` and `rename()` are now powered by tidyselect, 
  fixing a few renaming bugs (#3132, #2943, #2860).

* `summarise()` once again performs partial evaluation before database 
  submission (#3148).

* `test_src()` makes it easier to access a single test source.

## Database specific improvements

*   MS SQL
  
    * Better support for temporary tables (@Hong-Revo)
    
    * Different translations for filter/mutate contexts for: `NULL` evaluation
      (`is.na()`, `is.null()`), logical operators (`!`, `&`, `&&`, `|`, `||`),
      and comparison operators (`==`, `!=`, `<`, `>`, `>=`, `<=`)

*   MySQL: `copy_to()` (via `db_write_table()`) correctly translates logical 
    variables to integers (#3151).
  
*   odbc: improved `n()` translation in windowed context.

*   SQLite: improved `na_if` translation (@cwarden)

*   PostgreSQL: translation for `grepl()` added (@zozlak)

*   Oracle: changed VARVHAR to VARCHAR2 datatype (@washcycle, #66)

# dbplyr 1.1.0

## New features

* `full_join()` over non-overlapping columns `by = character()` translated to
  `CROSS JOIN` (#2924).

* `case_when()` now translates to SQL "CASE WHEN" (#2894)

* `x %in% c(1)` now generates the same SQL as `x %in% 1` (#2898).

* New `window_order()` and `window_frame()` give you finer control over 
  the window functions that dplyr creates (#2874, #2593).

* Added SQL translations for Oracle (@edgararuiz).

## Minor improvements and bug fixes

* `x %in% c(1)` now generates the same SQL as `x %in% 1` (#2898).

* `head(tbl, 0)` is now supported (#2863). 

* `select()`ing zero columns gives a more information error message (#2863).

* Variables created in a join are now disambiguated against other variables
  in the same table, not just variables in the other table (#2823).

* PostgreSQL gains a better translation for `round()` (#60).

* Added custom `db_analyze_table()` for MS SQL, Oracle, Hive and Impala (@edgararuiz)

* Added support for `sd()` for aggregate and window functions (#2887) (@edgararuiz) 

* You can now use the magrittr pipe within expressions,
  e.g. `mutate(mtcars, cyl %>% as.character())`.

* If a translation was supplied for a summarise function, but not for the
  equivalent windowed variant, the expression would be translated to `NULL`
  with a warning. Now `sql_variant()` checks that all aggregate functions 
  have matching window functions so that correct translations or clean errors
  will be generated (#2887)

# dbplyr 1.0.0

## New features

* `tbl()` and `copy_to()` now work directly with DBI connections (#2423, #2576), 
  so there is no longer a need to generate a dplyr src. 
  
    ```R
    library(dplyr)

    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    copy_to(con, mtcars)
    
    mtcars2 <- tbl(con, "mtcars")
    mtcars2
    ```

* `glimpse()` now works with remote tables (#2665)

* dplyr has gained a basic SQL optimiser, which collapses certain nested
  SELECT queries into a single query (#1979). This will improve query
  execution performance for databases with less sophisticated query optimisers,
  and fixes certain problems with ordering and limits in subqueries (#1979).
  A big thanks goes to @hhoeflin for figuring out this optimisation.

* `compute()` and `collapse()` now preserve the "ordering" of rows.
  This only affects the computation of window functions, as the rest
  of SQL does not care about row order (#2281).

* `copy_to()` gains an `overwrite` argument which allows you to overwrite
  an existing table. Use with care! (#2296)

* New `in_schema()` function makes it easy to refer to tables in schema:
  `in_schema("my_schema_name", "my_table_name")`.

## Deprecated and defunct

* `query()` is no longer exported. It hasn't been useful for a while
  so this shouldn't break any code.

## Verb-level SQL generation

* Partial evaluation occurs immediately when you execute a verb (like 
  `filter()` or `mutate()`) rather than happening when the query is executed 
  (#2370).

* `mutate.tbl_sql()` will now generate as many subqueries as necessary so 
  that you can refer to variables that you just created (like in mutate
  with regular dataframes) (#2481, #2483).

* SQL joins have been improved:

    * SQL joins always use the `ON ...` syntax, avoiding `USING ...` even for 
      natural joins. Improved handling of tables with columns of the same name 
      (#1997, @javierluraschi). They now generate SQL more similar to what you'd
      write by hand, eliminating a layer or two of subqueries (#2333)
      
    * [API] They now follow the same rules for including duplicated key variables
      that the data frame methods do, namely that key variables are only
      kept from `x`, and never from `y` (#2410)
      
    * [API] The `sql_join()` generic now gains a `vars` argument which lists
      the variables taken from the left and right sides of the join. If you
      have a custom `sql_join()` method, you'll need to update how your
      code generates joins, following the template in `sql_join.generic()`.
      
    * `full_join()` throws a clear error when you attempt to use it with a
      MySQL backend (#2045)
      
    * `right_join()` and `full_join()` now return results consistent with
      local data frame sources when there are records in the right table with
      no match in the left table. `right_join()` returns values of `by` columns
      from the right table. `full_join()` returns coalesced values of `by` 
      columns from the left and right tables (#2578, @ianmcook)

*   `group_by()` can now perform an inline mutate for database backends (#2422).

*   The SQL generation set operations (`intersect()`, `setdiff()`, `union()`, 
    and `union_all()`) have been considerably improved. 
  
    By default, the component SELECT are surrounded with parentheses, except on
    SQLite. The SQLite backend will now throw an error if you attempt a set operation
    on a query that contains a LIMIT, as that is not supported in SQLite (#2270).
    
    All set operations match column names across inputs, filling in non-matching
    variables with NULL (#2556).

*   `rename()` and `group_by()` now combine correctly (#1962)

*   `tbl_lazy()` and `lazy_tbl()` have been exported. These help you test
    generated SQL with out an active database connection.

*   `ungroup()` correctly resets grouping variables (#2704).

## Vector-level SQL generation

* New `as.sql()` safely coerces an input to SQL.

* More translators for `as.character()`, `as.integer()` and `as.double()` 
  (#2775).

* New `ident_q()` makes it possible to specifier identifiers that do not
  need to be quoted.

* Translation of inline scalars:

    * Logical values are now translated differently depending on the backend.
      The default is to use "true" and "false" which is the SQL-99 standard,
      but not widely support. SQLite translates to "0" and "1" (#2052).

    * `Inf` and `-Inf` are correctly escaped

    * Better test for whether or not a double is similar to an integer and 
      hence needs a trailing 0.0 added (#2004).

    * Quoting defaults to `DBI::dbEscapeString()` and `DBI::dbQuoteIdentifier()`
      respectively.

* `::` and `:::` are handled correctly (#2321)

* `x %in% 1` is now correctly translated to `x IN (1)` (#511).

* `ifelse()` and `if_else()` use correct argument names in SQL translation 
  (#2225).

* `ident()` now returns an object with class `c("ident", "character")`. It
   no longer contains "sql" to indicate that this is not already escaped.
   
* `is.na()` and `is.null()` gain extra parens in SQL translation to preserve
  correct precedence (#2302).

* [API] `log(x, b)` is now correctly translated to the SQL `log(b, x)` (#2288).
  SQLite does not support the 2-argument log function so it is translated
  to `log(x) / log(b)`.

* `nth(x, i)` is now correctly translated to `nth_value(x, i)`.

* `n_distinct()` now accepts multiple variables (#2148).

* [API] `substr()` is now translated to SQL, correcting for the difference
  in the third argument. In R, it's the position of the last character,
  in SQL it's the length of the string (#2536).

* `win_over()` escapes expression using current database rules.

## Backends

* `copy_to()` now uses `db_write_table()` instead of `db_create_table()` and 
  `db_insert_into()`. `db_write_table.DBIConnection()` uses `dbWriteTable()`.

* New `db_copy_to()`, `db_compute()` and `db_collect()` allow backends to 
  override the entire database process behind `copy_to()`, `compute()` and 
  `collect()`. `db_sql_render()` allow additional control over the SQL
  rendering process.

* All generics whose behaviour can vary from database to database now 
  provide a DBIConnection method. That means that you can easily scan
  the NAMESPACE to see the extension points.

* `sql_escape_logical()` allows you to control the translation of
  literal logicals (#2614).

* `src_desc()` has been replaced by `db_desc()` and  now dispatches on the 
  connection, eliminating the last method that required dispatch on the class 
  of the src.

* `win_over()`, `win_rank()`, `win_recycled()`, `win_cumulative()`,
  `win_current_group()` and `win_current_order()` are now exported. This
  should make it easier to provide customised SQL for window functions
  (#2051, #2126).
  
*  SQL translation for Microsoft SQL Server (@edgararuiz)

*  SQL translation for Apache Hive (@edgararuiz)

*  SQL translation for Apache Impala (@edgararuiz)


## Minor bug fixes and improvements

* `collect()` once again defaults to return all rows in the data (#1968).
  This makes it behave the same as `as.data.frame()` and `as_tibble()`.

* `collect()` only regroups by variables present in the data (#2156)

* `collect()` will automatically LIMIT the result to the `n`, the number of 
  rows requested. This will provide the query planner with more information
  that it may be able to use to improve execution time (#2083).

* `common_by()` gets a better error message for unexpected inputs (#2091)

* `copy_to()` no longer checks that the table doesn't exist before creation,
  instead preferring to fall back on the database for error messages. This
  should reduce both false positives and false negative (#1470)

* `copy_to()` now succeeds for MySQL if a character column contains `NA` 
   (#1975, #2256, #2263, #2381, @demorenoc, @eduardgrebe).

* `copy_to()` now returns its output invisibly (since you're often just
   calling for the side-effect).

* `distinct()` reports improved variable information for SQL backends. This
  means that it is more likely to work in the middle of a pipeline (#2359).

* Ungrouped `do()` on database backends now collects all data locally first
  (#2392).

* Call `dbFetch()` instead of the deprecated `fetch()` (#2134).
  Use `DBI::dbExecute()` for non-query SQL commands (#1912)

* `explain()` and `show_query()` now invisibly return the first argument,
  making them easier to use inside a pipeline.

* `print.tbl_sql()` displays ordering (#2287) and prints table name, if known.

* `print(df, n = Inf)` and `head(df, n = Inf)` now work with remote tables 
  (#2580).

* `db_desc()` and `sql_translate_env()` get defaults for DBIConnection.

* Formatting now works by overriding the `tbl_sum()` generic instead of `print()`. This means that the output is more consistent with tibble, and that `format()` is now supported also for SQL sources (tidyverse/dbplyr#14).

## Lazy ops

* [API] The signature of `op_base` has changed to `op_base(x, vars, class)`

* [API] `translate_sql()` and `partial_eval()` have been refined:

    * `translate_sql()` no longer takes a vars argument; instead call
      `partial_eval()` yourself. 
    
    * Because it no longer needs the environment `translate_sql()_` now
      works with a list of dots, rather than a `lazy_dots`.
      
    * `partial_eval()` now takes a character vector of variable names
      rather than a tbl.
      
    * This leads to a simplification of the `op` data structure: 
      dots is now a list of expressions rather than a `lazy_dots`.

* [API] `op_vars()` now returns a list of quoted expressions. This
  enables escaping to happen at the correct time (i.e. when the connection
  is known).
