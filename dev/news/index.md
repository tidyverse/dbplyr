# Changelog

## dbplyr (development version)

- All set operations now error if you pass extra arguments (instead of
  silently ignoring then)
  ([\#1585](https://github.com/tidyverse/dbplyr/issues/1585)).
- `str_flatten()`
  ([\#1540](https://github.com/tidyverse/dbplyr/issues/1540)) and
  [`n_distinct()`](https://dplyr.tidyverse.org/reference/n_distinct.html)
  ([\#1579](https://github.com/tidyverse/dbplyr/issues/1579)) now have
  an `na.rm` argument, which regularly warns when it’s not `TRUE`.
- [`semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)
  and
  [`anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)
  once again work with filtered windowed values
  ([\#1534](https://github.com/tidyverse/dbplyr/issues/1534),
  [\#1606](https://github.com/tidyverse/dbplyr/issues/1606)).
- [`window_order()`](https://dbplyr.tidyverse.org/dev/reference/window_order.md)
  works with
  [`dplyr::desc()`](https://dplyr.tidyverse.org/reference/desc.html)
  (not just [`desc()`](https://dplyr.tidyverse.org/reference/desc.html))
  ([\#1486](https://github.com/tidyverse/dbplyr/issues/1486)).
- [`sql_check_na_rm()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_agg.md)
  is now exported for use in other backends
  ([\#1483](https://github.com/tidyverse/dbplyr/issues/1483)).
- [`glue_sql2()`](https://dbplyr.tidyverse.org/dev/reference/glue_sql2.md)
  is now exported for building SQL strings with glue syntax and type
  markers.
  [`build_sql()`](https://dbplyr.tidyverse.org/dev/reference/build_sql.md)
  is deprecated in favor of
  [`glue_sql2()`](https://dbplyr.tidyverse.org/dev/reference/glue_sql2.md)
  ([\#1249](https://github.com/tidyverse/dbplyr/issues/1249)).
- dbplyr 1e interfaces are now deprecated
  ([\#1197](https://github.com/tidyverse/dbplyr/issues/1197)). Backend
  developers have had \>2 years to update.
- MySQL gains slightly better translation for
  [`as.integer()`](https://rdrr.io/r/base/integer.html) and
  `as.integer64()`
  ([\#1647](https://github.com/tidyverse/dbplyr/issues/1647)).
- Fixed snowflake translations that were being reported as unknown
  ([@edward-burn](https://github.com/edward-burn),
  [\#1570](https://github.com/tidyverse/dbplyr/issues/1570)).
- Deprecated `win_rank_tdata()` has been removed.
- [`compute()`](https://dplyr.tidyverse.org/reference/compute.html),
  [`collect()`](https://dplyr.tidyverse.org/reference/compute.html), and
  [`collapse()`](https://dplyr.tidyverse.org/reference/compute.html) now
  have their own documentation pages.
- dbplyr now uses the base pipe
  ([\#1626](https://github.com/tidyverse/dbplyr/issues/1626)).
- Defunct functions have been removed:
  - `src_sql()` deprecated in 1.4.0 (2019-04-23)
  - `partial_eval(var)` deprecated in 2.2.0 (2022-06-05).
  - `group_by(add = )` deprecated in dplyr 1.1.0 (2020-06-01).
- [`show_query()`](https://dbplyr.tidyverse.org/dev/reference/show_query.md)
  gains `use_colour` argument
  ([\#1590](https://github.com/tidyverse/dbplyr/issues/1590)).
- SQL server:
  [`slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html)
  returns different results each run
  ([@thomashulst](https://github.com/thomashulst),
  [\#1503](https://github.com/tidyverse/dbplyr/issues/1503))
- Corrected translation of
  [`stringr::str_like()`](https://stringr.tidyverse.org/reference/str_like.html)
  to use case-sensitive `LIKE` when argument `ignore_case` is set as
  `FALSE` ([@edward-burn](https://github.com/edward-burn),
  [\#1488](https://github.com/tidyverse/dbplyr/issues/1488)).
- Corrected translation of
  [`stringr::str_like()`](https://stringr.tidyverse.org/reference/str_like.html)
  to use case-sensitive `LIKE` when argument `ignore_case` is set as
  `FALSE` ([@edward-burn](https://github.com/edward-burn),
  [\#1488](https://github.com/tidyverse/dbplyr/issues/1488)).
- Fixed overwrite flag in
  [`copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html) to
  work when source is in the same DB as destination
  ([@liudvikasakelis](https://github.com/liudvikasakelis),
  [\#1535](https://github.com/tidyverse/dbplyr/issues/1535))
- Snowflake correctly translates `$` to `:`
  ([@jsowder](https://github.com/jsowder),
  [\#1608](https://github.com/tidyverse/dbplyr/issues/1608))
- [`dbplyr_uncount()`](https://dbplyr.tidyverse.org/dev/reference/dbplyr_uncount.md)
  now works with Redshift
  ([@owenjonesuob](https://github.com/owenjonesuob),
  [\#1601](https://github.com/tidyverse/dbplyr/issues/1601)).

## dbplyr 2.5.1

CRAN release: 2025-09-10

- Improved translations:

  - SQL Server, Redshift, Snowflake, and Postgres now correctly
    translate [`difftime()`](https://rdrr.io/r/base/difftime.html) (the
    sign is now correct)
    ([@edward-burn](https://github.com/edward-burn),
    [\#1532](https://github.com/tidyverse/dbplyr/issues/1532)).
  - SQL server, Redshift, Snowflake, Postgres, and Spark now translate
    `date_count_between()`
    ([@edward-burn](https://github.com/edward-burn),
    [\#1495](https://github.com/tidyverse/dbplyr/issues/1495)).
  - SQL Server now supports
    [`pmin()`](https://rdrr.io/r/base/Extremes.html) and
    [`pmax()`](https://rdrr.io/r/base/Extremes.html)
    ([@edward-burn](https://github.com/edward-burn),
    [\#1602](https://github.com/tidyverse/dbplyr/issues/1602)).
  - SQL server uses `COUNT_BIG` instead of `COUNT` so
    [`tally()`](https://dplyr.tidyverse.org/reference/count.html) and
    [`count()`](https://dplyr.tidyverse.org/reference/count.html) work
    regardless of data size
    ([@edward-burn](https://github.com/edward-burn),
    [\#1498](https://github.com/tidyverse/dbplyr/issues/1498)).
  - Spark correctly translates `clock::add_years()`
    ([@ablack3](https://github.com/ablack3),
    [\#1510](https://github.com/tidyverse/dbplyr/issues/1510)).
  - Teradata now supports
    [`as.double()`](https://rdrr.io/r/base/double.html) and
    [`as.character()`](https://rdrr.io/r/base/character.html)
    ([@rplsmn](https://github.com/rplsmn),
    [\#1545](https://github.com/tidyverse/dbplyr/issues/1545)).

- Tightened argument checks for SQL translations. These changes should
  result in more informative errors in cases where code already failed,
  possibly silently; if you see errors with code that used to run
  correctly, please report them to the package authors
  ([@simonpcouch](https://github.com/simonpcouch),
  [\#1554](https://github.com/tidyverse/dbplyr/issues/1554),
  [\#1555](https://github.com/tidyverse/dbplyr/issues/1555)).

- `across(everything())` doesn’t select grouping columns created via
  `.by` in
  [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
  ([@mgirlich](https://github.com/mgirlich),
  [\#1493](https://github.com/tidyverse/dbplyr/issues/1493)).

- Spark SQL backend now supports persisting tables with
  `compute(x, name = I("x.y.z"), temporary = FALSE)`
  ([@zacdav-db](https://github.com/zacdav-db),
  [\#1502](https://github.com/tidyverse/dbplyr/issues/1502)).

## dbplyr 2.5.0

CRAN release: 2024-03-19

### Improved tools for qualified table names

- Specification of table names with schema/catalogs has been overhauled
  to make it simpler. This includes the following features and fixes:

  - The simplest way to refer to a qualified table is now to wrap it in
    [`I()`](https://rdrr.io/r/base/AsIs.html),
    e.g. `I("schema_name.table_name")`.

  - Use of [`sql()`](https://dbplyr.tidyverse.org/dev/reference/sql.md)
    and
    [`ident_q()`](https://dbplyr.tidyverse.org/dev/reference/ident_q.md)
    inside
    [`in_catalog()`](https://dbplyr.tidyverse.org/dev/reference/in_schema.md)
    and
    [`in_schema()`](https://dbplyr.tidyverse.org/dev/reference/in_schema.md)
    is once again supported
    ([\#1388](https://github.com/tidyverse/dbplyr/issues/1388)).

  - It’s ok to use
    [`ident_q()`](https://dbplyr.tidyverse.org/dev/reference/ident_q.md)
    once again
    ([\#1413](https://github.com/tidyverse/dbplyr/issues/1413)) and you
    should no longer see unsuppressable warnings about using
    [`in_schema()`](https://dbplyr.tidyverse.org/dev/reference/in_schema.md)
    ([\#1408](https://github.com/tidyverse/dbplyr/issues/1408)).

  - The names of the arguments to
    [`Id()`](https://dbi.r-dbi.org/reference/Id.html) no longer matter,
    only their order
    ([\#1416](https://github.com/tidyverse/dbplyr/issues/1416)).
    Additionally, thanks to changes to the DBI package, you no longer
    need to name each argument.

  - If you accidentally pass a named vector to any of the database
    identifier functions, those names will be automatically stripped
    ([\#1404](https://github.com/tidyverse/dbplyr/issues/1404)).

  - `tbl_sql(check_from)` is now deprecated.

- dbplyr now exports some tools to work with the internal `table_path`
  class which is useful for certain backends that need to work with this
  data structure
  ([\#1300](https://github.com/tidyverse/dbplyr/issues/1300)).

### Improved SQL

- New translations for clock functions `add_years()`, `add_days()`,
  `date_build()`, `get_year()`, `get_month()`, `get_day()`, and
  [`base::difftime()`](https://rdrr.io/r/base/difftime.html) on SQL
  server, Redshift, Snowflake, and Postgres.

- [`select()`](https://dplyr.tidyverse.org/reference/select.html) will
  keep computed columns used to
  [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
  subqueries that are eliminated by a subsequent select
  ([@ejneer](https://github.com/ejneer),
  [\#1437](https://github.com/tidyverse/dbplyr/issues/1437)).

- [`semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)
  will no longer inline away an aggregate filter (i.e. `HAVING` clause)
  that was followed by a
  [`select()`](https://dplyr.tidyverse.org/reference/select.html)
  ([@ejneer](https://github.com/ejneer),
  [\#1474](https://github.com/tidyverse/dbplyr/issues/1474))

- Improved function translations:

  - Functions qualified with the base namespace are now also translated,
    e.g. `base::paste0(x, "_1")` is now translated
    ([@mgirlich](https://github.com/mgirlich),
    [\#1022](https://github.com/tidyverse/dbplyr/issues/1022)).

  - `-1 + x` now generates a translation instead erroring
    ([\#1420](https://github.com/tidyverse/dbplyr/issues/1420)).

  - `x$name` never attempts to evaluate `name`
    ([\#1368](https://github.com/tidyverse/dbplyr/issues/1368)).

  - You can once again use `NULL` on the LHS of an infix operator in
    order to generate SQL with unusual syntax
    ([\#1345](https://github.com/tidyverse/dbplyr/issues/1345)).

  - Namespaced calls now error if the function doesn’t exist, or a
    translation is not available
    ([\#1426](https://github.com/tidyverse/dbplyr/issues/1426)).

  - [`lead()`](https://dplyr.tidyverse.org/reference/lead-lag.html)
    translation coerces `n` to an integer.

- Databricks: now supports creating non-temporary tables too
  ([\#1418](https://github.com/tidyverse/dbplyr/issues/1418)).

- Oracle:

  - [`db_explain()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
    now works ([@thomashulst](https://github.com/thomashulst),
    [\#1353](https://github.com/tidyverse/dbplyr/issues/1353)).

  - [`as.Date()`](https://rdrr.io/r/base/as.Date.html) works when
    applied to a string
    ([\#1389](https://github.com/tidyverse/dbplyr/issues/1389)).

  - [`head()`](https://rdrr.io/r/utils/head.html) is once again
    translated to `FETCH FIRST`. This does require Oracle 12c or newer,
    but it actually works, compared to the approach using `ROWNUM` from
    [\#1292](https://github.com/tidyverse/dbplyr/issues/1292)
    ([\#1436](https://github.com/tidyverse/dbplyr/issues/1436)).

  - Added support for `str_replace()` and `str_replace_all()` via
    `REGEXP_REPLACE()` ([@thomashulst](https://github.com/thomashulst),
    [\#1402](https://github.com/tidyverse/dbplyr/issues/1402)).

- Snowflake ([@nathanhaigh](https://github.com/nathanhaigh),
  [\#1406](https://github.com/tidyverse/dbplyr/issues/1406))

  - Added support for `str_starts()` and `str_ends()` via
    `REGEXP_INSTR()`

  - Refactored `str_detect()` to use `REGEXP_INSTR()` so now supports
    regular expressions.

  - Refactored [`grepl()`](https://rdrr.io/r/base/grep.html) to use
    `REGEXP_INSTR()` so now supports case-insensitive matching through
    `grepl(..., ignore.case = TRUE)`

- SQL server:

  - Now products a clear error if you attempt to use
    [`n_distinct()`](https://dplyr.tidyverse.org/reference/n_distinct.html)
    in [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
    or [`filter()`](https://dplyr.tidyverse.org/reference/filter.html)
    ([\#1366](https://github.com/tidyverse/dbplyr/issues/1366)).

  - [`filter()`](https://dplyr.tidyverse.org/reference/filter.html) does
    a better job of converting logical vectors from bit to boolean
    ([@ejneer](https://github.com/ejneer),
    [\#1288](https://github.com/tidyverse/dbplyr/issues/1288)).

- MySQL: [`as.integer()`](https://rdrr.io/r/base/integer.html) gets
  correct translation ([@krlmlr](https://github.com/krlmlr),
  [\#1375](https://github.com/tidyverse/dbplyr/issues/1375)).

### Minor improvements and bug fixes

- Deprecation status of functions deprecated in previous versions (at
  least 2 years old) have been advanced. In particular, `src_sql()` is
  now defunct, as is the use of
  [`partial_eval()`](https://dbplyr.tidyverse.org/dev/reference/partial_eval.md)
  with character `data`.

- Database errors now show the generated SQL, which hopefully will make
  it faster to track down problems
  ([\#1401](https://github.com/tidyverse/dbplyr/issues/1401)).

- When dbplyr creates an index on a table in a schema
  (e.g. `schema.table`), it now only includes the table name in the
  index name, not the schema name.

- The class of remote sources now includes all S4 class names, not just
  the first ([\#918](https://github.com/tidyverse/dbplyr/issues/918)).

- [`compute()`](https://dplyr.tidyverse.org/reference/compute.html)
  passes additional arguments all the way to
  [`sql_query_save()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)-methods
  ([@rsund](https://github.com/rsund)).

- [`db_sql_render()`](https://dbplyr.tidyverse.org/dev/reference/db-misc.md)
  correctly passes on `...` when re-calling with `sql_options` set
  ([\#1394](https://github.com/tidyverse/dbplyr/issues/1394)).

- [`reframe()`](https://dplyr.tidyverse.org/reference/reframe.html) now
  gives an informative error that it isn’t supported
  ([\#1148](https://github.com/tidyverse/dbplyr/issues/1148)).

- `rows_patch(in_place = FALSE)` now works when more than one column
  should be patched ([@gorcha](https://github.com/gorcha),
  [\#1443](https://github.com/tidyverse/dbplyr/issues/1443)).

- New
  [`simulate_mariadb()`](https://dbplyr.tidyverse.org/dev/reference/backend-mysql.md)
  ([@krlmlr](https://github.com/krlmlr),
  [\#1375](https://github.com/tidyverse/dbplyr/issues/1375)).

- [`sql_translator()`](https://dbplyr.tidyverse.org/dev/reference/sql_variant.md)
  now checks for duplicated definitions
  ([@krlmlr](https://github.com/krlmlr),
  [\#1374](https://github.com/tidyverse/dbplyr/issues/1374)).

## dbplyr 2.4.0

CRAN release: 2023-10-26

### Breaking changes

- Using `compute(temporary = FALSE)` without providing a name is now
  deprecated ([@mgirlich](https://github.com/mgirlich),
  [\#1154](https://github.com/tidyverse/dbplyr/issues/1154)).

- [`ntile()`](https://dplyr.tidyverse.org/reference/ntile.html)’s first
  argument has been renamed from `order_by` to `x` to match the
  interface of
  [`dplyr::ntile()`](https://dplyr.tidyverse.org/reference/ntile.html)
  ([@mgirlich](https://github.com/mgirlich),
  [\#1242](https://github.com/tidyverse/dbplyr/issues/1242)).

- `simulate_vars()` and `simulate_vars_is_typed()` were removed as they
  weren’t used and tidyselect now offers `tidyselect_data_proxy()` and
  `tidyselect_data_has_predicates()`
  ([@mgirllich](https://github.com/mgirllich),
  [\#1199](https://github.com/tidyverse/dbplyr/issues/1199)).

- [`sql_not_supported()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_agg.md)
  now expects a function name without parentheses.

- [`sql_query_append()`](https://dbplyr.tidyverse.org/dev/reference/sql_query_insert.md),
  [`sql_query_insert()`](https://dbplyr.tidyverse.org/dev/reference/sql_query_insert.md),
  `sql_query_update()`,
  [`sql_query_upsert()`](https://dbplyr.tidyverse.org/dev/reference/sql_query_insert.md),
  and
  [`sql_query_delete()`](https://dbplyr.tidyverse.org/dev/reference/sql_query_insert.md)
  changed their arguments to make them more consistent to the other
  `sql_query_*()` functions:

  - `x_name` was renamed to `table`.
  - `y` was renamed to `from` and must now be a table identifier or SQL
    instead of a lazy table.
  - [`sql_query_append()`](https://dbplyr.tidyverse.org/dev/reference/sql_query_insert.md)
    and
    [`sql_query_insert()`](https://dbplyr.tidyverse.org/dev/reference/sql_query_insert.md)
    have gained the argument `cols`.

- [`remote_name()`](https://dbplyr.tidyverse.org/dev/reference/remote_name.md)
  now returns a string with the name of the table. To get the qualified
  identifier use the newly added
  [`remote_table()`](https://dbplyr.tidyverse.org/dev/reference/remote_name.md)
  ([@mgirlich](https://github.com/mgirlich),
  [\#1280](https://github.com/tidyverse/dbplyr/issues/1280)).

- [`tbl_lazy()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
  loses `src` argument after it has been deprecated for years
  ([@mgirlich](https://github.com/mgirlich),
  [\#1208](https://github.com/tidyverse/dbplyr/issues/1208)).

- [`translate_sql()`](https://dbplyr.tidyverse.org/dev/reference/translate_sql.md)
  now requires the `con` argument
  ([@mgirlich](https://github.com/mgirlich),
  [\#1311](https://github.com/tidyverse/dbplyr/issues/1311)). The `vars`
  argument has been removed after it threw an error for the last 7 years
  ([@mgirlich](https://github.com/mgirlich)).

### Improved SQL

- Preliminary databricks Spark SQL backend
  ([\#1377](https://github.com/tidyverse/dbplyr/issues/1377)).

- Joins

  - `*_join()` after
    [`full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
    works again ([@mgirlich](https://github.com/mgirlich),
    [\#1178](https://github.com/tidyverse/dbplyr/issues/1178)).

  - `*_join()` now allows specifying the relationship argument. It must
    be `NULL` or `"many-to-many"` ([@bairdj](https://github.com/bairdj),
    [\#1305](https://github.com/tidyverse/dbplyr/issues/1305)).

  - Queries now qualify `*` with the table alias for better
    compatibility ([@mgirlich](https://github.com/mgirlich),
    [\#1003](https://github.com/tidyverse/dbplyr/issues/1003)).

  - [`full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
    can now handle column names that only differ in case
    ([@ejneer](https://github.com/ejneer),
    [\#1255](https://github.com/tidyverse/dbplyr/issues/1255)).

  - The `na_matches` argument of
    [`semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)
    and
    [`anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)
    works again ([@mgirlich](https://github.com/mgirlich),
    [\#1211](https://github.com/tidyverse/dbplyr/issues/1211)).

  - A `semi/anti_join()` on filtered `y` is inlined when possible
    ([@mgirlich](https://github.com/mgirlich),
    [\#884](https://github.com/tidyverse/dbplyr/issues/884)).

  - Joins now work again for Pool and Oracle connections
    ([@mgirlich](https://github.com/mgirlich),
    [\#1177](https://github.com/tidyverse/dbplyr/issues/1177),
    [\#1181](https://github.com/tidyverse/dbplyr/issues/1181)).

- A sequence of
  [`union()`](https://generics.r-lib.org/reference/setops.html) resp.
  [`union_all()`](https://dplyr.tidyverse.org/reference/setops.html) now
  produces a flat query instead of subqueries
  ([@mgirlich](https://github.com/mgirlich),
  [\#1269](https://github.com/tidyverse/dbplyr/issues/1269)).

- Added translations for:

  - [`nzchar()`](https://rdrr.io/r/base/nchar.html)
    ([@MichaelChirico](https://github.com/MichaelChirico),
    [@mgirlich](https://github.com/mgirlich),
    [\#1094](https://github.com/tidyverse/dbplyr/issues/1094)).
  - `str_detect()`, `str_starts()` and `str_ends()` with fixed patterns
    ([@mgirlich](https://github.com/mgirlich),
    [\#1009](https://github.com/tidyverse/dbplyr/issues/1009)).
  - [`runif()`](https://rdrr.io/r/stats/Uniform.html)
    ([@mgirlich](https://github.com/mgirlich),
    [\#1200](https://github.com/tidyverse/dbplyr/issues/1200)).

- [`if_any()`](https://dplyr.tidyverse.org/reference/across.html) and
  [`if_all()`](https://dplyr.tidyverse.org/reference/across.html)
  translations are now wrapped in parentheses. This makes sure it can be
  combined via `&` with other conditions
  ([@mgirlich](https://github.com/mgirlich),
  [\#1153](https://github.com/tidyverse/dbplyr/issues/1153)).

- [`nth()`](https://dplyr.tidyverse.org/reference/nth.html),
  [`first()`](https://dplyr.tidyverse.org/reference/nth.html), and
  [`last()`](https://dplyr.tidyverse.org/reference/nth.html) now support
  the `na_rm` argument ([@mgirlich](https://github.com/mgirlich),
  [\#1193](https://github.com/tidyverse/dbplyr/issues/1193)).

### Minor improvements and bug fixes

- [`across()`](https://dplyr.tidyverse.org/reference/across.html) now
  supports namespaced functions, e.g.  `across(x, dplyr::dense_rank)`
  ([@mgirlich](https://github.com/mgirlich),
  [\#1231](https://github.com/tidyverse/dbplyr/issues/1231)).

- `db_copy_to(overwrite = TRUE)` now actually works.

- [`db_copy_to()`](https://dbplyr.tidyverse.org/dev/reference/db-io.md)’s
  `...` are now passed to
  [`db_write_table()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  ([@mgirlich](https://github.com/mgirlich),
  [\#1237](https://github.com/tidyverse/dbplyr/issues/1237)).

- Added
  [`db_supports_table_alias_with_as()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  to customise whether a backend supports specifying a table alias with
  `AS` or not ([@mgirlich](https://github.com/mgirlich)).

- [`db_write_table()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  and
  [`db_save_query()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  gain the `overwrite` argument.

- [`dbplyr_pivot_wider_spec()`](https://dbplyr.tidyverse.org/dev/reference/pivot_wider.tbl_lazy.md)
  is now exported. Unlike `pivot_wider()` this can be lazy. Note that
  this will be removed soon after `pivot_wider_spec()` becomes a generic
  ([@mgirlich](https://github.com/mgirlich)).

- [`filter()`](https://dplyr.tidyverse.org/reference/filter.html)ing
  with window functions now generates columns called `col01` rather than
  `q01` ([@mgirlich](https://github.com/mgirlich),
  [\#1258](https://github.com/tidyverse/dbplyr/issues/1258)).

- `pivot_wider()` now matches tidyr `NA` column handling
  ([@ejneer](https://github.com/ejneer)
  [\#1238](https://github.com/tidyverse/dbplyr/issues/1238)).

- [`select()`](https://dplyr.tidyverse.org/reference/select.html) can
  once again be used after `arrange(desc(x))`
  ([@ejneer](https://github.com/ejneer),
  [\#1240](https://github.com/tidyverse/dbplyr/issues/1240)).

- [`show_query()`](https://dbplyr.tidyverse.org/dev/reference/show_query.md)
  and
  [`remote_query()`](https://dbplyr.tidyverse.org/dev/reference/remote_name.md)
  gain the argument `sql_options` that allows to control how the SQL is
  generated. It can be created via
  [`sql_options()`](https://dbplyr.tidyverse.org/dev/reference/sql_options.md)
  which has the following arguments:

  - `cte`: use common table expressions?
  - `use_star`: use `SELECT *` or explicitly select every column?
  - `qualify_all_columns`: qualify all columns in a join or only the
    ambiguous ones? ([@mgirlich](https://github.com/mgirlich),
    [\#1146](https://github.com/tidyverse/dbplyr/issues/1146)).

  Consequently the `cte` argument of
  [`show_query()`](https://dbplyr.tidyverse.org/dev/reference/show_query.md)
  and
  [`remote_query()`](https://dbplyr.tidyverse.org/dev/reference/remote_name.md)
  has been deprecated ([@mgirlich](https://github.com/mgirlich),
  [\#1146](https://github.com/tidyverse/dbplyr/issues/1146)).

- `slice_min/max()` can now order by multiple variables like dplyr,
  e.g. use `slice_min(lf, tibble(x, y))`
  ([@mgirlich](https://github.com/mgirlich),
  [\#1167](https://github.com/tidyverse/dbplyr/issues/1167)).

- `slice_*()` now supports the data masking pronouns `.env` and `.data`
  ([@mgirlich](https://github.com/mgirlich),
  [\#1294](https://github.com/tidyverse/dbplyr/issues/1294)).

- [`sql_join_suffix()`](https://dbplyr.tidyverse.org/dev/reference/db-misc.md)
  gains the argument `suffix` so that methods can check whether the
  suffix is valid for the backend
  ([@mgirlich](https://github.com/mgirlich)).

- [`sql_random()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  is now deprecated. It was used to power
  [`slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html)
  which is now done via the translation for
  [`runif()`](https://rdrr.io/r/stats/Uniform.html)
  ([@mgirlich](https://github.com/mgirlich),
  [\#1200](https://github.com/tidyverse/dbplyr/issues/1200)).

- [`tbl()`](https://dplyr.tidyverse.org/reference/tbl.html) now informs
  when the user probably forgot to wrap the table identifier with
  [`in_schema()`](https://dbplyr.tidyverse.org/dev/reference/in_schema.md)
  or [`sql()`](https://dbplyr.tidyverse.org/dev/reference/sql.md)
  ([@mgirlich](https://github.com/mgirlich),
  [\#1287](https://github.com/tidyverse/dbplyr/issues/1287)).

### Backend specific improvements

- Access
  - Added translation for `!=` to `<>`
    ([@erikvona](https://github.com/erikvona),
    [\#1219](https://github.com/tidyverse/dbplyr/issues/1219)).
- DuckDB
  - now supports the `returning` argument of `rows_*()`.
- MySQL/MariaDB:
  - [`rows_update()`](https://dplyr.tidyverse.org/reference/rows.html)
    and
    [`rows_patch()`](https://dplyr.tidyverse.org/reference/rows.html)
    now give an informative error when the unsupported `returning`
    argument is used ([@mgirlich](https://github.com/mgirlich),
    [\#1279](https://github.com/tidyverse/dbplyr/issues/1279)).
  - [`rows_upsert()`](https://dplyr.tidyverse.org/reference/rows.html)
    now gives an informative error that it isn’t supported
    ([@mgirlich](https://github.com/mgirlich),
    [\#1279](https://github.com/tidyverse/dbplyr/issues/1279)).
  - `rows_*()` use the column types of `x` when auto copying `y`
    ([@mgirlich](https://github.com/mgirlich),
    [\#1327](https://github.com/tidyverse/dbplyr/issues/1327)).
  - [`copy_inline()`](https://dbplyr.tidyverse.org/dev/reference/copy_inline.md)
    now works ([@mgirlich](https://github.com/mgirlich),
    [\#1188](https://github.com/tidyverse/dbplyr/issues/1188)).
  - Fix translation of
    [`as.numeric()`](https://rdrr.io/r/base/numeric.html),
    [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html),
    `as_datetime()`, and `as.integer64()`
    ([@avsdev-cw](https://github.com/avsdev-cw),
    [\#1189](https://github.com/tidyverse/dbplyr/issues/1189)).
- MS SQL:
  - [`row_number()`](https://dplyr.tidyverse.org/reference/row_number.html)
    now works when no order is specified
    ([@ejneer](https://github.com/ejneer),
    [@fh-mthomson](https://github.com/fh-mthomson),
    [\#1332](https://github.com/tidyverse/dbplyr/issues/1332))
- Oracle:
  - Fix translation of
    [`rows_upsert()`](https://dplyr.tidyverse.org/reference/rows.html)
    ([@mgirlich](https://github.com/mgirlich),
    [@TBlackmore](https://github.com/TBlackmore),
    [\#1286](https://github.com/tidyverse/dbplyr/issues/1286))
  - `head(n)` is now translated to `WHERE ROWNUM <= n` to also support
    old versions \<= 11.2
    ([@JeremyPasco](https://github.com/JeremyPasco),
    [\#1292](https://github.com/tidyverse/dbplyr/issues/1292)).
- Postgres
  - The `rows_*()` functions now also work inside a transaction
    ([@mgirlich](https://github.com/mgirlich),
    [\#1183](https://github.com/tidyverse/dbplyr/issues/1183)).
- SQLite
  - Subqueries now also get an alias. This makes it consistent with
    other backends and simplifies the implementation.
- SQL Server
  - `distinct(.keep_all = TRUE)` now works
    ([@mgirlich](https://github.com/mgirlich),
    [\#1053](https://github.com/tidyverse/dbplyr/issues/1053)).
  - The translation of
    [`between()`](https://dplyr.tidyverse.org/reference/between.html)
    now also works when used in
    [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
    ([@mgirlich](https://github.com/mgirlich),
    [\#1241](https://github.com/tidyverse/dbplyr/issues/1241)).
  - [`any()`](https://rdrr.io/r/base/any.html) and
    [`all()`](https://rdrr.io/r/base/all.html) now work
    ([@ejneer](https://github.com/ejneer),
    [\#1273](https://github.com/tidyverse/dbplyr/issues/1273)).
  - Fixed negation of bit (boolean) fields
    ([@ejneer](https://github.com/ejneer),
    [\#1239](https://github.com/tidyverse/dbplyr/issues/1239))
- Snowflake:
  - `na.rm = TRUE` is now respected in
    [`pmin()`](https://rdrr.io/r/base/Extremes.html) and
    [`pmax()`](https://rdrr.io/r/base/Extremes.html) instead of being
    silently ignored ([@fh-mthomson](https://github.com/fh-mthomson),
    [\#1329](https://github.com/tidyverse/dbplyr/issues/1329))
  - [`row_number()`](https://dplyr.tidyverse.org/reference/row_number.html)
    now works when no order is specified
    ([@fh-mthomson](https://github.com/fh-mthomson),
    [\#1332](https://github.com/tidyverse/dbplyr/issues/1332))
- Teradata
  - [`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html) +
    [`head()`](https://rdrr.io/r/utils/head.html) now work
    ([@mgirlich](https://github.com/mgirlich),
    [\#685](https://github.com/tidyverse/dbplyr/issues/685)).
  - `as.Date(x)` is now translate to `CAST(x AS DATE)` again unless `x`
    is a string ([@mgirlich](https://github.com/mgirlich),
    [\#1285](https://github.com/tidyverse/dbplyr/issues/1285)).
  - [`row_number()`](https://dplyr.tidyverse.org/reference/row_number.html)
    no longer defaults to partitioning by groups (now aligned with other
    databases when no order is specified: `ROW_NUMBER()` defaults to
    `ORDER BY (SELECT NULL)`)
    ([@fh-mthomson](https://github.com/fh-mthomson),
    [\#1331](https://github.com/tidyverse/dbplyr/issues/1331))

## dbplyr 2.3.4

CRAN release: 2023-09-26

- Hot patch release to resolve R CMD check failures.

## dbplyr 2.3.3

CRAN release: 2023-07-07

- Hot patch to fix R CMD check issues

## dbplyr 2.3.2

CRAN release: 2023-03-21

- Hot patch to fix R CMD check issues

## dbplyr 2.3.1

CRAN release: 2023-02-24

### Breaking changes

- [`window_order()`](https://dbplyr.tidyverse.org/dev/reference/window_order.md)
  now only accepts bare symbols or symbols wrapped in
  [`desc()`](https://dplyr.tidyverse.org/reference/desc.html). This
  breaking change is necessary to allow
  [`select()`](https://dplyr.tidyverse.org/reference/select.html) to
  drop and rename variables used in
  [`window_order()`](https://dbplyr.tidyverse.org/dev/reference/window_order.md)
  ([@mgirlich](https://github.com/mgirlich),
  [\#1103](https://github.com/tidyverse/dbplyr/issues/1103)).

### Improved error messages

- [`quantile()`](https://rdrr.io/r/stats/quantile.html) and
  [`median()`](https://rdrr.io/r/stats/median.html) now error for SQL
  Server when used in
  [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
  and for PostgreSQL when used in
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) as
  they can’t be properly translated
  ([@mgirlich](https://github.com/mgirlich),
  [\#1110](https://github.com/tidyverse/dbplyr/issues/1110)).

- Added an informative error for unsupported join arguments `unmatched`
  and `multiple` ([@mgirlich](https://github.com/mgirlich)).

- Using predicates, e.g. `where(is.integer)`, in
  [`across()`](https://dplyr.tidyverse.org/reference/across.html) now
  produces an error as they never worked anyway
  ([@mgirlich](https://github.com/mgirlich),
  [\#1169](https://github.com/tidyverse/dbplyr/issues/1169)).

- Catch unsupported argument `pivot_wider(id_expand = TRUE)` and
  `pivot_longer(cols_vary)` ([@mgirlich](https://github.com/mgirlich),
  [\#1109](https://github.com/tidyverse/dbplyr/issues/1109)).

### Bug fixes in SQL generation

- Fixed an issue when using a window function after a
  [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
  and [`select()`](https://dplyr.tidyverse.org/reference/select.html)
  ([@mgirlich](https://github.com/mgirlich),
  [\#1104](https://github.com/tidyverse/dbplyr/issues/1104)).

- Fixed an issue when there where at least 3 joins and renamed variables
  ([@mgirlich](https://github.com/mgirlich),
  [\#1101](https://github.com/tidyverse/dbplyr/issues/1101)).

- [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) and
  [`select()`](https://dplyr.tidyverse.org/reference/select.html) after
  [`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html)
  now again produce a subquery to generate the correct translation
  ([@mgirlich](https://github.com/mgirlich),
  [\#1119](https://github.com/tidyverse/dbplyr/issues/1119),
  [\#1141](https://github.com/tidyverse/dbplyr/issues/1141)).

- Fixed an issue when using
  [`filter()`](https://dplyr.tidyverse.org/reference/filter.html) on a
  summarised variable ([@mgirlich](https://github.com/mgirlich),
  [\#1128](https://github.com/tidyverse/dbplyr/issues/1128)).

- [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) +
  [`filter()`](https://dplyr.tidyverse.org/reference/filter.html) now
  again produces a new query if the
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) uses a
  window function or SQL ([@mgirlich](https://github.com/mgirlich),
  [\#1135](https://github.com/tidyverse/dbplyr/issues/1135)).

- [`across()`](https://dplyr.tidyverse.org/reference/across.html) and
  [`pick()`](https://dplyr.tidyverse.org/reference/pick.html) can be
  used (again) in
  [`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html)
  ([@mgirlich](https://github.com/mgirlich),
  [\#1125](https://github.com/tidyverse/dbplyr/issues/1125)).

- The `rows_*()` function work again for tables in a schema in
  PostgreSQL ([@mgirlich](https://github.com/mgirlich),
  [\#1133](https://github.com/tidyverse/dbplyr/issues/1133)).

### Minor improvements and bug fixes

- [`sql()`](https://dbplyr.tidyverse.org/dev/reference/sql.md) now
  evaluates its arguments locally also when used in
  [`across()`](https://dplyr.tidyverse.org/reference/across.html)
  ([@mgirlich](https://github.com/mgirlich),
  [\#1039](https://github.com/tidyverse/dbplyr/issues/1039)).

- The rank functions
  ([`row_number()`](https://dplyr.tidyverse.org/reference/row_number.html),
  [`min_rank()`](https://dplyr.tidyverse.org/reference/row_number.html),
  [`rank()`](https://rdrr.io/r/base/rank.html),
  [`dense_rank()`](https://dplyr.tidyverse.org/reference/row_number.html),
  [`percent_rank()`](https://dplyr.tidyverse.org/reference/percent_rank.html),
  and
  [`cume_dist()`](https://dplyr.tidyverse.org/reference/percent_rank.html))
  now support multiple variables by wrapping them in
  [`tibble()`](https://tibble.tidyverse.org/reference/tibble.html),
  e.g. `rank(tibble(x, y))` ([@mgirlich](https://github.com/mgirlich),
  [\#1118](https://github.com/tidyverse/dbplyr/issues/1118)).

- [`pull()`](https://dplyr.tidyverse.org/reference/pull.html) now
  supports the argument `name`
  ([@mgirlich](https://github.com/mgirlich),
  [\#1136](https://github.com/tidyverse/dbplyr/issues/1136)).

- Added support for
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html)
  added in dplyr 1.1.0 ([@mgirlich](https://github.com/mgirlich),
  [\#1074](https://github.com/tidyverse/dbplyr/issues/1074)).

- Using `by = character()` to perform a cross join is now
  soft-deprecated in favor of
  [`cross_join()`](https://dplyr.tidyverse.org/reference/cross_join.html).

- [`full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  and
  [`right_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  are now translated directly to `FULL JOIN` and `RIGHT JOIN` for SQLite
  as native support was finally added
  ([@mgirlich](https://github.com/mgirlich),
  [\#1150](https://github.com/tidyverse/dbplyr/issues/1150)).

- [`case_match()`](https://dplyr.tidyverse.org/reference/case_match.html)
  now works with strings on the left hand side
  ([@mgirlich](https://github.com/mgirlich),
  [\#1143](https://github.com/tidyverse/dbplyr/issues/1143)).

- The rank functions
  ([`row_number()`](https://dplyr.tidyverse.org/reference/row_number.html),
  [`min_rank()`](https://dplyr.tidyverse.org/reference/row_number.html),
  [`rank()`](https://rdrr.io/r/base/rank.html),
  [`dense_rank()`](https://dplyr.tidyverse.org/reference/row_number.html),
  [`percent_rank()`](https://dplyr.tidyverse.org/reference/percent_rank.html),
  and
  [`cume_dist()`](https://dplyr.tidyverse.org/reference/percent_rank.html))
  now work again for variables wrapped in
  [`desc()`](https://dplyr.tidyverse.org/reference/desc.html),
  e.g. `row_number(desc(x))` ([@mgirlich](https://github.com/mgirlich),
  [\#1118](https://github.com/tidyverse/dbplyr/issues/1118)).

- Moved argument `auto_index` after `...` in `*_join()`
  ([@mgirlich](https://github.com/mgirlich),
  [\#1115](https://github.com/tidyverse/dbplyr/issues/1115)).

- Removed dependency on assertthat
  ([@mgirlich](https://github.com/mgirlich),
  [\#1112](https://github.com/tidyverse/dbplyr/issues/1112)).

- [`across()`](https://dplyr.tidyverse.org/reference/across.html) now
  uses the original value when a column is overridden to match the
  behaviour of dplyr. For example
  `mutate(df, across(c(x, y), ~ .x / x))` now produces

      SELECT `x` / `x` AS `x`, `y` / `x` AS `y`
      FROM `df`

  instead of

      SELECT `x`, `y` / `x` AS `y`
      FROM (
        SELECT `x` / `x` AS `x`, `y`
        FROM `df`
      ) 

  ([@mgirlich](https://github.com/mgirlich),
  [\#1015](https://github.com/tidyverse/dbplyr/issues/1015)).

- Restricted length of table aliases to avoid truncation on certain
  backends (e.g., Postgres)
  ([@fh-mthomson](https://github.com/fh-mthomson),
  [\#1096](https://github.com/tidyverse/dbplyr/issues/1096))

## dbplyr 2.3.0

CRAN release: 2023-01-16

- Compatibility with purrr 1.0.0
  ([@mgirlich](https://github.com/mgirlich),
  [\#1085](https://github.com/tidyverse/dbplyr/issues/1085)).

### New features

- [`stringr::str_like()`](https://stringr.tidyverse.org/reference/str_like.html)
  (new in 1.5.0) is translated to the closest `LIKE` equivalent
  ([@rjpat](https://github.com/rjpat),
  [\#509](https://github.com/tidyverse/dbplyr/issues/509))

- In preparation for dplyr 1.1.0:

  - The `.by` argument is supported
    ([@mgirlich](https://github.com/mgirlich),
    [\#1051](https://github.com/tidyverse/dbplyr/issues/1051)).
  - Passing `...` to
    [`across()`](https://dplyr.tidyverse.org/reference/across.html) is
    deprecated because the evaluation timing of `...` is ambiguous. Now
    instead of (e.g.) `across(a:b, mean, na.rm = TRUE)` use
    `across(a:b, \(x) mean(x, na.rm = TRUE)`
  - [`pick()`](https://dplyr.tidyverse.org/reference/pick.html) is
    translated ([@mgirlich](https://github.com/mgirlich),
    [\#1044](https://github.com/tidyverse/dbplyr/issues/1044)).
  - [`case_match()`](https://dplyr.tidyverse.org/reference/case_match.html)
    is translated ([@mgirlich](https://github.com/mgirlich),
    [\#1020](https://github.com/tidyverse/dbplyr/issues/1020)).
  - [`case_when()`](https://dplyr.tidyverse.org/reference/case_when.html)
    now supports the `.default` argument
    ([@mgirlich](https://github.com/mgirlich),
    [\#1017](https://github.com/tidyverse/dbplyr/issues/1017)).

- Variables that aren’t found in either the data or in the environment
  now produce an error ([@mgirlich](https://github.com/mgirlich),
  [\#907](https://github.com/tidyverse/dbplyr/issues/907)).

### SQL optimisation

- dbplyr now produces fewer subqueries resulting in shorter, more
  readable, and, in some cases, faster SQL. The following combination of
  verbs now avoids a subquery if possible:

  - `*_join()` +
    [`select()`](https://dplyr.tidyverse.org/reference/select.html)
    ([@mgirlich](https://github.com/mgirlich),
    [\#876](https://github.com/tidyverse/dbplyr/issues/876)).
  - [`select()`](https://dplyr.tidyverse.org/reference/select.html) +
    `*_join()` ([@mgirlich](https://github.com/mgirlich),
    [\#875](https://github.com/tidyverse/dbplyr/issues/875)).
  - [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) +
    [`filter()`](https://dplyr.tidyverse.org/reference/filter.html) and
    [`filter()`](https://dplyr.tidyverse.org/reference/filter.html) +
    [`filter()`](https://dplyr.tidyverse.org/reference/filter.html)
    ([@mgirlich](https://github.com/mgirlich),
    [\#792](https://github.com/tidyverse/dbplyr/issues/792)).
  - [`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html)
    ([@mgirlich](https://github.com/mgirlich),
    [\#880](https://github.com/tidyverse/dbplyr/issues/880)).
  - [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html) +
    [`filter()`](https://dplyr.tidyverse.org/reference/filter.html) now
    translates to `HAVING` ([@mgirlich](https://github.com/mgirlich),
    [\#877](https://github.com/tidyverse/dbplyr/issues/877)).
  - `left/inner_join()` + `left/inner_join()`
    ([@mgirlich](https://github.com/mgirlich),
    [\#865](https://github.com/tidyverse/dbplyr/issues/865)).

- dbplyr now uses `SELECT *` after a join instead of explicitly
  selecting every column, where possible
  ([@mgirlich](https://github.com/mgirlich),
  [\#898](https://github.com/tidyverse/dbplyr/issues/898)).

- Joins only use the table aliases (“LHS” and “RHS”) if necessary
  ([@mgirlich](https://github.com/mgirlich)).

- When using common table expressions, the results of joins and set
  operations are now reused ([@mgirlich](https://github.com/mgirlich),
  [\#978](https://github.com/tidyverse/dbplyr/issues/978)).

### Improved error messages

- Many errors have been improved and now show the function where the
  error happened instead of a helper function
  ([@mgirlich](https://github.com/mgirlich),
  [\#907](https://github.com/tidyverse/dbplyr/issues/907)).

- Errors produced by the database, e.g. in
  [`collect()`](https://dplyr.tidyverse.org/reference/compute.html) or
  `rows_*()`, now show the verb where the error happened
  ([@mgirlich](https://github.com/mgirlich)).

- [`window_order()`](https://dbplyr.tidyverse.org/dev/reference/window_order.md)
  now produces a better error message when applied to a data frame
  ([@mgirlich](https://github.com/mgirlich),
  [\#947](https://github.com/tidyverse/dbplyr/issues/947)).

- Using a named
  [`across()`](https://dplyr.tidyverse.org/reference/across.html) now
  gives a clear error message ([@mgirlich](https://github.com/mgirlich),
  [\#761](https://github.com/tidyverse/dbplyr/issues/761)).

### Minor improvements and bug fixes

- Keyword highlighting can now be customised via the option
  `dbplyr_highlight`. Turn it off via
  `options(dbplyr_highlight = FALSE)` or pass a custom ansi style,
  e.g. `options(dbplyr_highlight = cli::combine_ansi_styles("bold", "cyan"))`
  ([@mgirlich](https://github.com/mgirlich),
  [\#974](https://github.com/tidyverse/dbplyr/issues/974)).

- The rank functions
  ([`row_number()`](https://dplyr.tidyverse.org/reference/row_number.html),
  [`min_rank()`](https://dplyr.tidyverse.org/reference/row_number.html),
  [`rank()`](https://rdrr.io/r/base/rank.html),
  [`dense_rank()`](https://dplyr.tidyverse.org/reference/row_number.html),
  [`percent_rank()`](https://dplyr.tidyverse.org/reference/percent_rank.html),
  and
  [`cume_dist()`](https://dplyr.tidyverse.org/reference/percent_rank.html))
  now give missing values the rank NA to match the behaviour of dplyr
  ([@mgirlich](https://github.com/mgirlich),
  [\#991](https://github.com/tidyverse/dbplyr/issues/991)).

- `NA`s in `blob()`s are correctly translated to `NULL`
  ([\#983](https://github.com/tidyverse/dbplyr/issues/983)).

- [`copy_inline()`](https://dbplyr.tidyverse.org/dev/reference/copy_inline.md)
  gains a `types` argument to specify the SQL column types
  ([@mgirlich](https://github.com/mgirlich),
  [\#963](https://github.com/tidyverse/dbplyr/issues/963)).

- [`cur_column()`](https://dplyr.tidyverse.org/reference/context.html)
  is now supported ([@mgirlich](https://github.com/mgirlich),
  [\#951](https://github.com/tidyverse/dbplyr/issues/951)).

- [`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html)
  returns columns ordered the way you request, not the same as the input
  data ([@mgirlich](https://github.com/mgirlich)).

- `fill()` can now fill “downup” and “updown”
  ([@mgirlich](https://github.com/mgirlich),
  [\#1057](https://github.com/tidyverse/dbplyr/issues/1057)), and now
  order by non-numeric columns also in the up direction
  ([@mgirlich](https://github.com/mgirlich),
  [\#1057](https://github.com/tidyverse/dbplyr/issues/1057)).

- [`filter()`](https://dplyr.tidyverse.org/reference/filter.html) now
  works when using a window function and an external vector
  ([\#1048](https://github.com/tidyverse/dbplyr/issues/1048)).

- [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html) +
  renamed columns works once again
  ([@mgirlich](https://github.com/mgirlich),
  [\#928](https://github.com/tidyverse/dbplyr/issues/928)).

- [`last()`](https://dplyr.tidyverse.org/reference/nth.html) is
  correctly translated when no window frame is specified
  ([@mgirlich](https://github.com/mgirlich),
  [\#1063](https://github.com/tidyverse/dbplyr/issues/1063)).

- [`setOldClass()`](https://rdrr.io/r/methods/setOldClass.html) uses a
  namespace, fixing an installation issue
  ([@mgirlich](https://github.com/mgirlich),
  [\#927](https://github.com/tidyverse/dbplyr/issues/927)).

- [`sql()`](https://dbplyr.tidyverse.org/dev/reference/sql.md) is now
  translated differently. The `...` are now evaluated locally instead of
  being translated with
  [`translate_sql()`](https://dbplyr.tidyverse.org/dev/reference/translate_sql.md)
  ([@mgirlich](https://github.com/mgirlich),
  [\#952](https://github.com/tidyverse/dbplyr/issues/952)).

### Backend specific improvements

- HANA:

  - Correctly translates
    [`as.character()`](https://rdrr.io/r/base/character.html)
    ([\#1027](https://github.com/tidyverse/dbplyr/issues/1027)).
  - [`copy_inline()`](https://dbplyr.tidyverse.org/dev/reference/copy_inline.md)
    now works for Hana
    ([\#950](https://github.com/tidyverse/dbplyr/issues/950))

- MySQL:

  - `str_flatten()` uses `collapse = ""` by default
    ([@fh-afrachioni](https://github.com/fh-afrachioni),
    [\#993](https://github.com/tidyverse/dbplyr/issues/993))

- Oracle:

  - [`slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html)
    now works for Oracle ([@mgirlich](https://github.com/mgirlich),
    [\#986](https://github.com/tidyverse/dbplyr/issues/986)).
  - [`copy_inline()`](https://dbplyr.tidyverse.org/dev/reference/copy_inline.md)
    now works for Oracle
    ([\#972](https://github.com/tidyverse/dbplyr/issues/972))

- PostgreSQL:

  - Generates correct literals for Dates
    ([\#727](https://github.com/tidyverse/dbplyr/issues/727)).
  - `str_flatten()` uses `collapse = ""` by default
    ([@fh-afrachioni](https://github.com/fh-afrachioni),
    [\#993](https://github.com/tidyverse/dbplyr/issues/993))
  - `rows_*()` use the column types of `x` when auto copying
    ([@mgirlich](https://github.com/mgirlich),
    [\#909](https://github.com/tidyverse/dbplyr/issues/909)).

- Redshift:

  - [`round()`](https://rdrr.io/r/base/Round.html) now respects the
    `digits` argument ([@owenjonesuob](https://github.com/owenjonesuob),
    [\#1033](https://github.com/tidyverse/dbplyr/issues/1033)).
  - No longer tries to use named windows anymore
    ([@owenjonesuob](https://github.com/owenjonesuob),
    [\#1035](https://github.com/tidyverse/dbplyr/issues/1035)).
  - [`copy_inline()`](https://dbplyr.tidyverse.org/dev/reference/copy_inline.md)
    now works for Redshift
    ([\#949](https://github.com/tidyverse/dbplyr/issues/949), thanks to
    [@ejneer](https://github.com/ejneer) for an initial implementation).
  - `str_flatten()` uses `collapse = ""` by default
    ([@fh-afrachioni](https://github.com/fh-afrachioni),
    [\#993](https://github.com/tidyverse/dbplyr/issues/993))

- Snowflake:

- numeric functions: [`all()`](https://rdrr.io/r/base/all.html),
  [`any()`](https://rdrr.io/r/base/any.html),
  [`log10()`](https://rdrr.io/r/base/Log.html),
  [`round()`](https://rdrr.io/r/base/Round.html),
  [`cor()`](https://rdrr.io/r/stats/cor.html),
  [`cov()`](https://rdrr.io/r/stats/cor.html) and
  [`sd()`](https://rdrr.io/r/stats/sd.html).

- date functions: `day()`, `mday()`, `wday()`, `yday()`, `week()`,
  `isoweek()`, `month()`, `quarter()`, `isoyear()`, `seconds()`,
  `minutes()`, `hours()`, `days()`, `weeks()`,
  [`months()`](https://rdrr.io/r/base/weekday.POSIXt.html), `years()`
  and `floor_date()`.

- string functions: [`grepl()`](https://rdrr.io/r/base/grep.html),
  [`paste()`](https://rdrr.io/r/base/paste.html),
  [`paste0()`](https://rdrr.io/r/base/paste.html), `str_c()`,
  `str_locate()`, `str_detect()`, `str_replace()`, `str_replace_all()`,
  `str_remove()`, `str_remove_all()`, `str_trim()`, `str_squish()` and
  `str_flatten()` ([@fh-afrachioni](https://github.com/fh-afrachioni),
  [\#860](https://github.com/tidyverse/dbplyr/issues/860)).

- `str_flatten()` uses `collapse = ""` by default
  ([@fh-afrachioni](https://github.com/fh-afrachioni),
  [\#993](https://github.com/tidyverse/dbplyr/issues/993))

- SQLite:

  - [`quantile()`](https://rdrr.io/r/stats/quantile.html) gives a better
    error saying that it is not supported
    ([@mgirlich](https://github.com/mgirlich),
    [\#1000](https://github.com/tidyverse/dbplyr/issues/1000)).

- SQL server:

  - [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html) now
    translated correctly ([@krlmlr](https://github.com/krlmlr),
    [\#1011](https://github.com/tidyverse/dbplyr/issues/1011)).
  - [`median()`](https://rdrr.io/r/stats/median.html) now translated
    correctly
    ([\#1008](https://github.com/tidyverse/dbplyr/issues/1008)).
  - `pivot_wider()` works again for MS SQL
    ([@mgirlich](https://github.com/mgirlich),
    [\#929](https://github.com/tidyverse/dbplyr/issues/929)).
  - Always use 1 and 0 as literals for logicals
    ([@krlmlr](https://github.com/krlmlr),
    [\#934](https://github.com/tidyverse/dbplyr/issues/934)).

- Teradata:

  - Querying works again. Unfortunately, the fix requires every column
    to once again by explicitly selected
    ([@mgirlich](https://github.com/mgirlich),
    [\#966](https://github.com/tidyverse/dbplyr/issues/966)).
  - New translations for
    [`as.Date()`](https://rdrr.io/r/base/as.Date.html), `week()`,
    `quarter()`, [`paste()`](https://rdrr.io/r/base/paste.html),
    [`startsWith()`](https://rdrr.io/r/base/startsWith.html),
    [`row_number()`](https://dplyr.tidyverse.org/reference/row_number.html),
    [`weighted.mean()`](https://rdrr.io/r/stats/weighted.mean.html),
    [`lead()`](https://dplyr.tidyverse.org/reference/lead-lag.html),
    [`lag()`](https://dplyr.tidyverse.org/reference/lead-lag.html), and
    [`cumsum()`](https://rdrr.io/r/base/cumsum.html)
    ([@overmar](https://github.com/overmar),
    [\#913](https://github.com/tidyverse/dbplyr/issues/913)).

## dbplyr 2.2.1

CRAN release: 2022-06-27

- Querying Oracle databases works again. Unfortunately, the fix requires
  every column to be explicitly selected again
  ([@mgirlich](https://github.com/mgirlich),
  [\#908](https://github.com/tidyverse/dbplyr/issues/908)).

- [`semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)
  and
  [`anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)
  work again for Spark ([@mgirlich](https://github.com/mgirlich),
  [\#915](https://github.com/tidyverse/dbplyr/issues/915)).

- `str_c()` is now translated to `||` in Oracle
  ([@mgirlich](https://github.com/mgirlich),
  [\#921](https://github.com/tidyverse/dbplyr/issues/921)).

- [`sd()`](https://rdrr.io/r/stats/sd.html),
  [`var()`](https://rdrr.io/r/stats/cor.html),
  [`cor()`](https://rdrr.io/r/stats/cor.html) and
  [`cov()`](https://rdrr.io/r/stats/cor.html) now give clear error
  messages on databases that don’t support them.

- [`any()`](https://rdrr.io/r/base/any.html) and
  [`all()`](https://rdrr.io/r/base/all.html) gain default translations
  for all backends.

## dbplyr 2.2.0

CRAN release: 2022-06-05

### New features

- SQL formatting has been considerably improved with new wrapping and
  indenting.
  [`show_query()`](https://dbplyr.tidyverse.org/dev/reference/show_query.md)
  creates more readable queries by printing the keywords in blue
  ([@mgirlich](https://github.com/mgirlich),
  [\#644](https://github.com/tidyverse/dbplyr/issues/644)). When
  possible dbplyr now uses `SELECT *` instead of explicitly selecting
  every column ([@mgirlich](https://github.com/mgirlich)).

- Added support for
  [`rows_insert()`](https://dplyr.tidyverse.org/reference/rows.html),
  [`rows_append()`](https://dplyr.tidyverse.org/reference/rows.html),
  [`rows_update()`](https://dplyr.tidyverse.org/reference/rows.html),
  [`rows_patch()`](https://dplyr.tidyverse.org/reference/rows.html),
  [`rows_upsert()`](https://dplyr.tidyverse.org/reference/rows.html),
  and [`rows_delete()`](https://dplyr.tidyverse.org/reference/rows.html)
  ([@mgirlich](https://github.com/mgirlich),
  [\#736](https://github.com/tidyverse/dbplyr/issues/736)).

- Added
  [`copy_inline()`](https://dbplyr.tidyverse.org/dev/reference/copy_inline.md)
  as a [`copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html)
  equivalent that does not need write access
  ([@mgirlich](https://github.com/mgirlich),
  [\#628](https://github.com/tidyverse/dbplyr/issues/628)).

- [`remote_query()`](https://dbplyr.tidyverse.org/dev/reference/remote_name.md),
  [`show_query()`](https://dbplyr.tidyverse.org/dev/reference/show_query.md),
  [`compute()`](https://dplyr.tidyverse.org/reference/compute.html) and
  [`collect()`](https://dplyr.tidyverse.org/reference/compute.html) have
  an experimental `cte` argument. If `TRUE` the SQL query will use
  common table expressions instead of nested queries
  ([@mgirlich](https://github.com/mgirlich),
  [\#638](https://github.com/tidyverse/dbplyr/issues/638)).

- New
  [`in_catalog()`](https://dbplyr.tidyverse.org/dev/reference/in_schema.md),
  which works like
  [`in_schema()`](https://dbplyr.tidyverse.org/dev/reference/in_schema.md),
  but allows creation of table identifiers consisting of three
  components: catalog, schema, name
  ([\#806](https://github.com/tidyverse/dbplyr/issues/806),
  [@krlmlr](https://github.com/krlmlr)).

### Improvements to SQL generation

- When possible, dbplyr now uses `SELECT *` instead of explicitly
  selecting every column ([@mgirlich](https://github.com/mgirlich)).

- New translation for [`cut()`](https://rdrr.io/r/base/cut.html)
  ([@mgirlich](https://github.com/mgirlich),
  [\#697](https://github.com/tidyverse/dbplyr/issues/697)).

- Improved translations for specific backends:

  - [`as.Date()`](https://rdrr.io/r/base/as.Date.html) for Oracle
    ([@mgirlich](https://github.com/mgirlich),
    [\#661](https://github.com/tidyverse/dbplyr/issues/661)).
  - [`case_when()`](https://dplyr.tidyverse.org/reference/case_when.html)
    with a final clause of the form `TRUE ~ ...` uses `ELSE ...` for
    SQLite ([@mgirlich](https://github.com/mgirlich),
    [\#754](https://github.com/tidyverse/dbplyr/issues/754)).
  - `day()`, `week()`, `isoweek()`, and `isoyear()` for Postgres
    ([@mgirlich](https://github.com/mgirlich),
    [\#675](https://github.com/tidyverse/dbplyr/issues/675)).
  - [`explain()`](https://dplyr.tidyverse.org/reference/explain.html)
    for ROracle ([@mgirlich](https://github.com/mgirlich)).
  - `fill()` for SQL Server
    ([\#651](https://github.com/tidyverse/dbplyr/issues/651),
    [@mgirlich](https://github.com/mgirlich)) and RPostgreSQL
    ([@mgirlich](https://github.com/mgirlich)).
  - [`quantile()`](https://rdrr.io/r/stats/quantile.html) for SQL Server
    ([@mgirlich](https://github.com/mgirlich),
    [\#620](https://github.com/tidyverse/dbplyr/issues/620)).
  - `str_flatten()` for Redshift ([@hdplsa](https://github.com/hdplsa),
    [\#804](https://github.com/tidyverse/dbplyr/issues/804))
  - [`slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html)
    for MySQL/MariaDB and SQL Server
    ([@mgirlich](https://github.com/mgirlich),
    [\#617](https://github.com/tidyverse/dbplyr/issues/617)).
  - [`union()`](https://generics.r-lib.org/reference/setops.html) for
    Hive ([@mgirlich](https://github.com/mgirlich),
    [\#663](https://github.com/tidyverse/dbplyr/issues/663)).

- The backend function `dbplyr_fill0()` (used for databases that lack
  `IGNORE NULLS` support) now respects database specific translations
  ([@rsund](https://github.com/rsund),
  [\#753](https://github.com/tidyverse/dbplyr/issues/753)).

- Calls of the form `stringr::foo()` or `lubridate::foo()` are now
  evaluated in the database, rather than locally
  ([\#197](https://github.com/tidyverse/dbplyr/issues/197)).

- Unary plus (e.g. `db %>% filter(x == +1)`) now works
  ([@mgirlich](https://github.com/mgirlich),
  [\#674](https://github.com/tidyverse/dbplyr/issues/674)).

- [`is.na()`](https://rdrr.io/r/base/NA.html),
  [`ifelse()`](https://rdrr.io/r/base/ifelse.html),
  [`if_else()`](https://dplyr.tidyverse.org/reference/if_else.html),
  [`case_when()`](https://dplyr.tidyverse.org/reference/case_when.html),
  and `if()` generate slightly more compact SQL
  ([@mgirlich](https://github.com/mgirlich),
  [\#738](https://github.com/tidyverse/dbplyr/issues/738)).

- [`if_else()`](https://dplyr.tidyverse.org/reference/if_else.html) now
  supports the `missing` argument
  ([@mgirlich](https://github.com/mgirlich),
  [\#641](https://github.com/tidyverse/dbplyr/issues/641)).

- [`n()`](https://dplyr.tidyverse.org/reference/context.html) now
  respects the window frame ([@mgirlich](https://github.com/mgirlich),
  [\#700](https://github.com/tidyverse/dbplyr/issues/700)).

- [`quantile()`](https://rdrr.io/r/stats/quantile.html) no longer errors
  when using the `na.rm` argument
  ([@mgirlich](https://github.com/mgirlich),
  [\#600](https://github.com/tidyverse/dbplyr/issues/600)).

- [`remote_name()`](https://dbplyr.tidyverse.org/dev/reference/remote_name.md)
  now returns a name in more cases where it makes sense
  ([@mgirlich](https://github.com/mgirlich),
  [\#850](https://github.com/tidyverse/dbplyr/issues/850)).

- The partial evaluation code is now more aligned with `dtplyr`. This
  makes it easier to transfer bug fixes and new features from one
  package to the other. In this process the second argument of
  [`partial_eval()`](https://dbplyr.tidyverse.org/dev/reference/partial_eval.md)
  was changed to a lazy frame instead of a character vector of variables
  ([@mgirlich](https://github.com/mgirlich),
  [\#766](https://github.com/tidyverse/dbplyr/issues/766)). Partially
  evaluated expressions with infix operations are now correctly
  translated. For example `translate_sql(!!expr(2 - 1) * x)` now works
  ([@mgirlich](https://github.com/mgirlich),
  [\#634](https://github.com/tidyverse/dbplyr/issues/634)).

### Minor improvements and bug fixes

- New
  [`pillar::tbl_format_header()`](https://pillar.r-lib.org/reference/tbl_format_header.html)
  method for lazy tables: Printing a lazy table where all rows are
  displayed also shows the exact number of rows in the header. The
  threshold is controlled by `getOption("pillar.print_min")`, with a
  default of 10
  ([\#796](https://github.com/tidyverse/dbplyr/issues/796),
  [@krlmlr](https://github.com/krlmlr)).

- The 1st edition extension mechanism is formally deprecated
  ([\#507](https://github.com/tidyverse/dbplyr/issues/507)).

- [`across()`](https://dplyr.tidyverse.org/reference/across.html),
  [`if_any()`](https://dplyr.tidyverse.org/reference/across.html) and
  [`if_all()`](https://dplyr.tidyverse.org/reference/across.html) now
  defaults to `.cols = everything()`
  ([@mgirlich](https://github.com/mgirlich),
  [\#760](https://github.com/tidyverse/dbplyr/issues/760)). If `.fns` is
  not provided
  [`if_any()`](https://dplyr.tidyverse.org/reference/across.html) and
  [`if_all()`](https://dplyr.tidyverse.org/reference/across.html) work
  like a parallel version of
  [`any()`](https://rdrr.io/r/base/any.html)/[`any()`](https://rdrr.io/r/base/any.html)
  ([@mgirlich](https://github.com/mgirlich),
  [\#734](https://github.com/tidyverse/dbplyr/issues/734)).

- [`across()`](https://dplyr.tidyverse.org/reference/across.html),
  [`if_any()`](https://dplyr.tidyverse.org/reference/across.html), and
  [`if_all()`](https://dplyr.tidyverse.org/reference/across.html) can
  now translate evaluated lists and functions
  ([@mgirlich](https://github.com/mgirlich),
  [\#796](https://github.com/tidyverse/dbplyr/issues/796)), and accept
  the name of a list of functions
  ([@mgirlich](https://github.com/mgirlich),
  [\#817](https://github.com/tidyverse/dbplyr/issues/817)).

- Multiple
  [`across()`](https://dplyr.tidyverse.org/reference/across.html) calls
  in [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) and
  [`transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
  can now access freshly created variables
  ([@mgirlich](https://github.com/mgirlich),
  [\#802](https://github.com/tidyverse/dbplyr/issues/802)).

- [`add_count()`](https://dplyr.tidyverse.org/reference/count.html) now
  doesn’t change the groups of the input
  ([@mgirlich](https://github.com/mgirlich),
  [\#614](https://github.com/tidyverse/dbplyr/issues/614)).

- [`compute()`](https://dplyr.tidyverse.org/reference/compute.html) can
  now handle when `name` is named by unnaming it first
  ([@mgirlich](https://github.com/mgirlich),
  [\#623](https://github.com/tidyverse/dbplyr/issues/623)), and now
  works when `temporary = TRUE` for Oracle
  ([@mgirlich](https://github.com/mgirlich),
  [\#621](https://github.com/tidyverse/dbplyr/issues/621)).

- [`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html)
  now supports `.keep_all = TRUE`
  ([@mgirlich](https://github.com/mgirlich),
  [\#756](https://github.com/tidyverse/dbplyr/issues/756)).

- `expand()` now works in DuckDB
  ([@mgirlich](https://github.com/mgirlich),
  [\#712](https://github.com/tidyverse/dbplyr/issues/712)).

- [`explain()`](https://dplyr.tidyverse.org/reference/explain.html)
  passes `...` to methods ([@mgirlich](https://github.com/mgirlich),
  [\#783](https://github.com/tidyverse/dbplyr/issues/783)), and works
  for Redshift ([@mgirlich](https://github.com/mgirlich),
  [\#740](https://github.com/tidyverse/dbplyr/issues/740)).

- [`filter()`](https://dplyr.tidyverse.org/reference/filter.html) throws
  an error if you supply a named argument
  ([@mgirlich](https://github.com/mgirlich),
  [\#764](https://github.com/tidyverse/dbplyr/issues/764)).

- Joins disambiguates columns that only differ in case
  ([@mgirlich](https://github.com/mgirlich),
  [\#702](https://github.com/tidyverse/dbplyr/issues/702)). New
  arguments `x_as` and `y_as` allow you to control the table alias used
  in SQL query ([@mgirlich](https://github.com/mgirlich),
  [\#637](https://github.com/tidyverse/dbplyr/issues/637)). Joins with
  `na_matches = "na"` now work for DuckDB
  ([@mgirlich](https://github.com/mgirlich),
  [\#704](https://github.com/tidyverse/dbplyr/issues/704)).

- [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) and
  [`transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
  use named windows if a window definition is used at least twice and
  the backend supports named windows
  ([@mgirlich](https://github.com/mgirlich),
  [\#624](https://github.com/tidyverse/dbplyr/issues/624)).

- [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) now
  supports the arguments `.keep`, `.before`, and `.after`
  ([@mgirlich](https://github.com/mgirlich),
  [\#802](https://github.com/tidyverse/dbplyr/issues/802)).

- `na.rm = FALSE` only warns once every 8 hours across all functions
  ([\#899](https://github.com/tidyverse/dbplyr/issues/899)).

- `nesting()` now supports the `.name_repair` argument
  ([@mgirlich](https://github.com/mgirlich),
  [\#654](https://github.com/tidyverse/dbplyr/issues/654)).

- `pivot_longer()` can now pivot a column named `name`
  ([@mgirlich](https://github.com/mgirlich),
  [\#692](https://github.com/tidyverse/dbplyr/issues/692)), can repair
  names ([@mgirlich](https://github.com/mgirlich),
  [\#694](https://github.com/tidyverse/dbplyr/issues/694)), and can work
  with multiple `names_from` columns
  ([@mgirlich](https://github.com/mgirlich),
  [\#693](https://github.com/tidyverse/dbplyr/issues/693)).

- `pivot_wider(values_fn = )` and `pivot_longer(values_transform = )`
  can now be formulas ([@mgirlich](https://github.com/mgirlich),
  [\#745](https://github.com/tidyverse/dbplyr/issues/745)).

- `pivot_wider()` now supports the arguments `names_vary`,
  `names_expand`, and `unused_fn`
  ([@mgirlich](https://github.com/mgirlich),
  [\#774](https://github.com/tidyverse/dbplyr/issues/774)).

- [`remote_name()`](https://dbplyr.tidyverse.org/dev/reference/remote_name.md)
  now returns a name in more cases where it makes sense
  ([@mgirlich](https://github.com/mgirlich),
  [\#850](https://github.com/tidyverse/dbplyr/issues/850)).

- [`sql_random()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  is now exported.

- [`ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html)
  removes variables in `...` from grouping
  ([@mgirlich](https://github.com/mgirlich),
  [\#689](https://github.com/tidyverse/dbplyr/issues/689)).

- [`transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
  now keeps grouping variables
  ([@mgirlich](https://github.com/mgirlich),
  [\#802](https://github.com/tidyverse/dbplyr/issues/802)).

## dbplyr 2.1.1

CRAN release: 2021-04-06

- New support for Snowflake
  ([@edgararuiz](https://github.com/edgararuiz))

- [`compute()`](https://dplyr.tidyverse.org/reference/compute.html),
  [`sql_table_index()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md),
  and
  [`sql_query_wrap()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  now work with schemas ([@mgirlich](https://github.com/mgirlich),
  [\#595](https://github.com/tidyverse/dbplyr/issues/595)).

- [`if_any()`](https://dplyr.tidyverse.org/reference/across.html) and
  [`if_all()`](https://dplyr.tidyverse.org/reference/across.html) are
  now translated.

- [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  now ungroups when the dots argument is empty and `.add` is `FALSE`
  ([@mgirlich](https://github.com/mgirlich),
  [\#615](https://github.com/tidyverse/dbplyr/issues/615)).

- [`sql_escape_date()`](https://dbplyr.tidyverse.org/dev/reference/db-quote.md)
  and `sql_escape_datetime` gain methods for MS Access
  ([@erikvona](https://github.com/erikvona),
  [\#608](https://github.com/tidyverse/dbplyr/issues/608)).

## dbplyr 2.1.0

CRAN release: 2021-02-03

### New features

- Thanks to [@mgirlich](https://github.com/mgirlich), dbplyr gains
  support for key verbs from tidyr: `pivot_longer()`
  ([\#532](https://github.com/tidyverse/dbplyr/issues/532)),
  `pivot_wider()`
  ([\#543](https://github.com/tidyverse/dbplyr/issues/543)), `expand()`
  ([\#538](https://github.com/tidyverse/dbplyr/issues/538)),
  `complete()`
  ([\#538](https://github.com/tidyverse/dbplyr/issues/538)),
  `replace_na()`
  ([\#538](https://github.com/tidyverse/dbplyr/issues/538)), `fill()`
  ([\#566](https://github.com/tidyverse/dbplyr/issues/566)).

- [@mgirlich](https://github.com/mgirlich) is now a dbplyr author in
  recognition of his significant and sustained contributions.

- [`across()`](https://dplyr.tidyverse.org/reference/across.html)
  implementation has been rewritten to support more inputs: it now
  translates formulas
  ([\#525](https://github.com/tidyverse/dbplyr/issues/525)), works with
  SQL functions that don’t have R translations
  ([\#534](https://github.com/tidyverse/dbplyr/issues/534)), and work
  with `NULL` ([\#554](https://github.com/tidyverse/dbplyr/issues/554))

- [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
  now supports argument `.groups`
  ([@mgirlich](https://github.com/mgirlich),
  [\#584](https://github.com/tidyverse/dbplyr/issues/584)).

### SQL translation

- All backends: `str_sub()`,
  [`substr()`](https://rdrr.io/r/base/substr.html) and
  [`substring()`](https://rdrr.io/r/base/substr.html) get better
  translations
  ([\#577](https://github.com/tidyverse/dbplyr/issues/577)). Most
  importantly, the results of using negative locations should match the
  underlying R implementations more closely.

- MS SQL:

  - [`as.integer()`](https://rdrr.io/r/base/integer.html) and
    `as.integer64()` translations cast first to `NUMERIC` to avoid
    CASTing weirdness
    ([@DavidPatShuiFong](https://github.com/DavidPatShuiFong),
    [\#496](https://github.com/tidyverse/dbplyr/issues/496)).

  - Assumes a boolean context inside of `[`
    ([\#546](https://github.com/tidyverse/dbplyr/issues/546))

  - `str_sub()` with `end = -1` now works
    ([\#577](https://github.com/tidyverse/dbplyr/issues/577)).

- Redshift:
  [`lag()`](https://dplyr.tidyverse.org/reference/lead-lag.html) and
  [`lead()`](https://dplyr.tidyverse.org/reference/lead-lag.html) lose
  the `default` parameter since it’s not supported
  ([@hdplsa](https://github.com/hdplsa),
  [\#548](https://github.com/tidyverse/dbplyr/issues/548)).

- SQLite: custom translation of
  [`full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  and
  [`right_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  ([@mgirlich](https://github.com/mgirlich),
  [\#536](https://github.com/tidyverse/dbplyr/issues/536)).

### Minor improvements and bug fixes

- RPostgreSQL backend warns if `temporary = TRUE` since temporary tables
  are not supported by `RPostgreSQL::dbWriteTable()`
  ([\#574](https://github.com/tidyverse/dbplyr/issues/574)).

- [`count()`](https://dplyr.tidyverse.org/reference/count.html) method
  provides closer match to dplyr semantics
  ([\#347](https://github.com/tidyverse/dbplyr/issues/347)).

- [`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html)
  now respects grouping ([@mgirlich](https://github.com/mgirlich),
  [\#535](https://github.com/tidyverse/dbplyr/issues/535)).

- [`db_connection_describe()`](https://dbplyr.tidyverse.org/dev/reference/db-misc.md)
  no longer uses partial matching
  ([@mgirlich](https://github.com/mgirlich),
  [\#564](https://github.com/tidyverse/dbplyr/issues/564)).

- [`pull()`](https://dplyr.tidyverse.org/reference/pull.html) no longer
  [`select()`](https://dplyr.tidyverse.org/reference/select.html)s the
  result when there’s already only one variable
  ([\#562](https://github.com/tidyverse/dbplyr/issues/562)).

- [`select()`](https://dplyr.tidyverse.org/reference/select.html) no
  longer relocates grouping variables to the front
  ([@mgirlich](https://github.com/mgirlich),
  [\#568](https://github.com/tidyverse/dbplyr/issues/568)). and informs
  when adding missing grouping variables
  ([@mgirlich](https://github.com/mgirlich),
  [\#559](https://github.com/tidyverse/dbplyr/issues/559)).

- `tbl.src_dbi(...)` now passed on to
  [`tbl_sql()`](https://dbplyr.tidyverse.org/dev/reference/tbl_sql.md)
  ([\#530](https://github.com/tidyverse/dbplyr/issues/530)).

## dbplyr 2.0.0

CRAN release: 2020-11-03

### dplyr 1.0.0 compatibility

- [`across()`](https://dplyr.tidyverse.org/reference/across.html) is now
  translated into individual SQL statements
  ([\#480](https://github.com/tidyverse/dbplyr/issues/480)).

- [`rename()`](https://dplyr.tidyverse.org/reference/rename.html) and
  [`select()`](https://dplyr.tidyverse.org/reference/select.html)
  support dplyr 1.0.0 tidyselect syntax (apart from predicate functions
  which can’t easily work on computed queries)
  ([\#502](https://github.com/tidyverse/dbplyr/issues/502)).

- [`relocate()`](https://dplyr.tidyverse.org/reference/relocate.html)
  makes it easy to move columns
  ([\#494](https://github.com/tidyverse/dbplyr/issues/494)) and
  [`rename_with()`](https://dplyr.tidyverse.org/reference/rename.html)
  makes it easy to rename columns programmatically
  ([\#502](https://github.com/tidyverse/dbplyr/issues/502)).

- [`slice_min()`](https://dplyr.tidyverse.org/reference/slice.html),
  [`slice_max()`](https://dplyr.tidyverse.org/reference/slice.html), and
  `slice_order()` are now supported.
  [`slice_head()`](https://dplyr.tidyverse.org/reference/slice.html) and
  [`slice_tail()`](https://dplyr.tidyverse.org/reference/slice.html)
  throw clear error messages
  ([\#394](https://github.com/tidyverse/dbplyr/issues/394))

### SQL generation

- Documentation has been radically improved with new topics for each
  major verb and each backend giving more details about the SQL
  translation.

- [`intersect()`](https://generics.r-lib.org/reference/setops.html),
  [`union()`](https://generics.r-lib.org/reference/setops.html) and
  [`setdiff()`](https://generics.r-lib.org/reference/setops.html) gain
  an `all` argument to add the `ALL` argument
  ([\#414](https://github.com/tidyverse/dbplyr/issues/414)).

- Join functions gains a `na_matches` argument that allows you to
  control whether or not `NA` (`NULL`) values match other `NA` values.
  The default is `"never"`, which is the usual behaviour in databases.
  You can set `na_matches = "na"` to match R’s usual join behaviour
  ([\#180](https://github.com/tidyverse/dbplyr/issues/180)). Additional
  arguments error (instead of being silently swallowed)
  ([\#382](https://github.com/tidyverse/dbplyr/issues/382)).

- Joins now only use aliases where needed to disambiguate columns; this
  should make generated queries more readable.

- Subqueries no longer include an `ORDER BY` clause. This is not part of
  the SQL spec, and has very limited support across databases. Now such
  queries generate a warning suggesting that you move your
  [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html) call
  later in the pipeline
  ([\#276](https://github.com/tidyverse/dbplyr/issues/276)). (There’s
  one exception: `ORDER BY` is still generated if `LIMIT` is present;
  this tends to affect the returns rows but not necessarily their
  order).

- Subquery names are now scoped within the query. This makes query text
  deterministic which helps some query optimisers/cachers
  ([\#336](https://github.com/tidyverse/dbplyr/issues/336)).

- [`sql_optimise()`](https://dbplyr.tidyverse.org/dev/reference/sql_build.md)
  now can partially optimise a pipeline; due to an unfortunate bug it
  previously gave up too easily.

- [`in_schema()`](https://dbplyr.tidyverse.org/dev/reference/in_schema.md)
  quotes each input individually
  ([\#287](https://github.com/tidyverse/dbplyr/issues/287)) (use
  [`sql()`](https://dbplyr.tidyverse.org/dev/reference/sql.md) to opt
  out of quoting, if needed). And
  [`DBI::Id()`](https://dbi.r-dbi.org/reference/Id.html) should work
  anywhere that
  [`in_schema()`](https://dbplyr.tidyverse.org/dev/reference/in_schema.md)
  does.

### SQL translation

- Experimental new SAP HANA backend
  ([\#233](https://github.com/tidyverse/dbplyr/issues/233)). Requires
  the latest version of odbc.

- All backends:

  - You can now use `::` in translations, so that (e.g.)
    [`dbplyr::n()`](https://dplyr.tidyverse.org/reference/context.html)
    is translated to `count(*)`
    ([\#207](https://github.com/tidyverse/dbplyr/issues/207)).

  - `[[` can now also translate numeric indices
    ([\#520](https://github.com/tidyverse/dbplyr/issues/520)).

  - `%/%` now generates a clear error message; previously it was
    translated to `/` which is not correct
    ([\#108](https://github.com/tidyverse/dbplyr/issues/108)).

  - [`n()`](https://dplyr.tidyverse.org/reference/context.html) is
    translated to `count(*)` instead of
    [`count()`](https://dplyr.tidyverse.org/reference/count.html)
    ([\#343](https://github.com/tidyverse/dbplyr/issues/343)).

  - `sub_str()` translation is more consistent in edge cases
    ([@ianmcook](https://github.com/ianmcook)).

  - All [`median()`](https://rdrr.io/r/stats/median.html)
    ([@lorenzwalthert](https://github.com/lorenzwalthert),
    [\#483](https://github.com/tidyverse/dbplyr/issues/483)),
    [`pmin()`](https://rdrr.io/r/base/Extremes.html),
    [`pmax()`](https://rdrr.io/r/base/Extremes.html)
    ([\#479](https://github.com/tidyverse/dbplyr/issues/479)),
    [`sd()`](https://rdrr.io/r/stats/sd.html) and
    [`var()`](https://rdrr.io/r/stats/cor.html) functions have an
    `na.rm` argument that warns once when not `TRUE`. This makes them
    consistent with [`mean()`](https://rdrr.io/r/base/mean.html) and
    [`sum()`](https://rdrr.io/r/base/sum.html).

  - [`substring()`](https://rdrr.io/r/base/substr.html) is now
    translated the same way as
    [`substr()`](https://rdrr.io/r/base/substr.html)
    ([\#378](https://github.com/tidyverse/dbplyr/issues/378)).

- [blob](https://blob.tidyverse.org/) vectors can now be used with `!!`
  and `!!!` operators, for example in
  [`filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  ([@okhoma](https://github.com/okhoma),
  [\#433](https://github.com/tidyverse/dbplyr/issues/433))

- MySQL uses standard SQL for index creation.

- MS SQL translation does better a distinguishing between bit and
  boolean ([\#377](https://github.com/tidyverse/dbplyr/issues/377),
  [\#318](https://github.com/tidyverse/dbplyr/issues/318)). `if` and
  `ifelse` once again generate `IIF`, creating simpler expressions.
  `as.*()` function uses `TRY_CAST()` instead of `CAST()` for version
  11+ (2012+) ([@DavidPatShuiFong](https://github.com/DavidPatShuiFong),
  [\#380](https://github.com/tidyverse/dbplyr/issues/380)).

- odbc no longer translates
  [`count()`](https://dplyr.tidyverse.org/reference/count.html); this
  was an accidental inclusion.

- Oracle translation now depends on Oracle 12c, and uses a
  “row-limiting” clause for
  [`head()`](https://rdrr.io/r/utils/head.html). It gains translations
  for `today()` and `now()`, and improved
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html) translation
  ([@rlh1994](https://github.com/rlh1994),
  [\#267](https://github.com/tidyverse/dbplyr/issues/267)).

- PostgreSQL: new translations for lubridate period functions `years()`,
  [`months()`](https://rdrr.io/r/base/weekday.POSIXt.html), `days()`,
  and `floor_date()` ([@bkkkk](https://github.com/bkkkk),
  [\#333](https://github.com/tidyverse/dbplyr/issues/333)) and stringr
  functions `str_squish()`, `str_remove()`, and `str_remove_all()`
  ([@shosaco](https://github.com/shosaco)).

- New RedShift translations when used with
  [`RPostgres::Redshift()`](https://rpostgres.r-dbi.org/reference/Redshift.html).

  - `str_replace()` errors since there’s no Redshift translation, and
    `str_replace_all()` uses `REGEXP_REPLACE()`
    ([\#446](https://github.com/tidyverse/dbplyr/issues/446)).

  - [`paste()`](https://rdrr.io/r/base/paste.html) and
    [`paste0()`](https://rdrr.io/r/base/paste.html) use `||`
    ([\#458](https://github.com/tidyverse/dbplyr/issues/458)).

  - [`as.numeric()`](https://rdrr.io/r/base/numeric.html) and
    [`as.double()`](https://rdrr.io/r/base/double.html) cast to `FLOAT`
    ([\#408](https://github.com/tidyverse/dbplyr/issues/408)).

  - [`substr()`](https://rdrr.io/r/base/substr.html) and `str_sub()` use
    `SUBSTRING()`
    ([\#327](https://github.com/tidyverse/dbplyr/issues/327)).

- SQLite gains translations for lubridate functions `today()`, `now()`,
  `year()`, `month()`, `day()`, `hour()`, `minute()`,
  `second()`,`yday()`
  ([\#262](https://github.com/tidyverse/dbplyr/issues/262)), and correct
  translation for [`median()`](https://rdrr.io/r/stats/median.html)
  ([\#357](https://github.com/tidyverse/dbplyr/issues/357)).

### Extensibility

If you are the author of a dbplyr backend, please see
`vignette("backend-2")` for details.

- New
  [`dbplyr_edition()`](https://dbplyr.tidyverse.org/dev/reference/db-misc.md)
  generic allows you to opt-in to the 2nd edition of the dbplyr API.

- [`db_write_table()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  now calls
  [`DBI::dbWriteTable()`](https://dbi.r-dbi.org/reference/dbWriteTable.html)
  instead of nine generics that formerly each did a small part:
  [`db_create_indexes()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html),
  [`db_begin()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html),
  [`db_rollback()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html),
  [`db_commit()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html),
  [`db_list_tables()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html),
  `drop_drop_table()`,
  [`db_has_table()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html),
  [`db_create_table()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html),
  and `db_data_types()`. You can now delete the methods for these
  generics.

  [`db_query_rows()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  is no longer used; it appears that it hasn’t been used for some time,
  so if you have a method, you can delete it.

- [`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html)
  is now used instead of
  [`sql_escape_ident()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  and
  [`DBI::dbQuoteString()`](https://dbi.r-dbi.org/reference/dbQuoteString.html)
  instead of
  [`sql_escape_string()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html).

- A number of `db_*` generics have been replaced with new SQL generation
  generics:

  - [`dplyr::db_analyze()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
    -\>
    [`dbplyr::sql_table_analyze()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  - [`dplyr::db_create_index()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
    -\>
    [`dbplyr::sql_table_index()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  - [`dplyr::db_explain()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
    -\> `dbplyr::sql_queriy_explain()`
  - [`dplyr::db_query_fields()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
    -\>
    [`dbplyr::sql_query_fields()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  - [`dplyr::db_save_query()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
    -\>
    [`dbplyr::sql_query_save()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)

  This makes them easier to test and is an important part of the process
  of moving all database generics in dbplyr
  ([\#284](https://github.com/tidyverse/dbplyr/issues/284)).

- A number of other generics have been renamed to facilitate the move
  from dplyr to dbplyr:

  - [`dplyr::sql_select()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
    -\>
    [`dbplyr::sql_query_select()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  - [`dplyr::sql_join()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
    -\>
    [`dbplyr::sql_query_join()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  - [`dplyr::sql_semi_join()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
    -\>
    [`dbplyr::sql_query_semi_join()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  - [`dplyr::sql_set_op()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
    -\>
    [`dbplyr::sql_query_set_op()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  - [`dplyr::sql_subquery()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
    -\>
    [`dbplyr::sql_query_wrap()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  - [`dplyr::db_desc()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
    -\>
    [`dbplyr::db_connection_describe()`](https://dbplyr.tidyverse.org/dev/reference/db-misc.md)

- New `db_temporary_table()` generic makes it easier to work with
  databases that require temporary tables to be specially named.

- New
  [`sql_expr_matches()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  generic allows databases to use more efficient alternatives when
  determine if two values “match” (i.e. like equality but a pair of
  `NULL`s will also match). For more details, see
  <https://modern-sql.com/feature/is-distinct-from>

- New
  [`sql_join_suffix()`](https://dbplyr.tidyverse.org/dev/reference/db-misc.md)
  allows backends to control the default suffixes used
  ([\#254](https://github.com/tidyverse/dbplyr/issues/254)).

### Minor improvements and bug fixes

- All old lazy eval shims have been removed. These have been deprecated
  for some time.

- Date-time escaping methods for Athena and Presto have moved to the
  packages where they belong.

- Attempting to embed a Shiny reactive in a query now gives a helpful
  error ([\#439](https://github.com/tidyverse/dbplyr/issues/439)).

- [`copy_lahman()`](https://dbplyr.tidyverse.org/dev/reference/lahman.md)
  and
  [`copy_nycflights13()`](https://dbplyr.tidyverse.org/dev/reference/nycflights13.md)
  (and hence
  [`nycflights13_sqlite()`](https://dbplyr.tidyverse.org/dev/reference/nycflights13.md))
  and friends now return DBI connections rather than the now deprecated
  [`src_dbi()`](https://dbplyr.tidyverse.org/dev/reference/src_dbi.md)
  ([\#440](https://github.com/tidyverse/dbplyr/issues/440)).

- [`copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html) can
  now `overwrite` when table is specified with schema
  ([\#489](https://github.com/tidyverse/dbplyr/issues/489)), and gains
  an `in_transaction` argument used to optionally suppress the
  transaction wrapper
  ([\#368](https://github.com/tidyverse/dbplyr/issues/368)).

- [`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html) no
  longer duplicates column if grouped
  ([\#354](https://github.com/tidyverse/dbplyr/issues/354)).

- [`transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
  now correctly tracks variables it needs when creating subqueries
  ([\#313](https://github.com/tidyverse/dbplyr/issues/313)).

- [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  grouping variables no longer generates a downstream error
  ([\#396](https://github.com/tidyverse/dbplyr/issues/396))

- [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  correctly generates subqueries when you re-use the same variable three
  or more times
  ([\#412](https://github.com/tidyverse/dbplyr/issues/412)).

- [`window_order()`](https://dbplyr.tidyverse.org/dev/reference/window_order.md)
  overrides ordering, rather than appending to it.

## dbplyr 1.4.4

CRAN release: 2020-05-27

- Internally
  [`DBI::dbExecute()`](https://dbi.r-dbi.org/reference/dbExecute.html)
  now uses `immediate = TRUE`; this improves support for session-scoped
  temporary tables in MS SQL ([@krlmlr](https://github.com/krlmlr),
  [\#438](https://github.com/tidyverse/dbplyr/issues/438)).

- Subqueries with `ORDER BY` use `TOP 9223372036854775807` instead of
  `TOP 100 PERCENT` on SQL Server for compatibility with Azure Data
  Warehouse ([\#337](https://github.com/tidyverse/dbplyr/issues/337),
  [@alexkyllo](https://github.com/alexkyllo)).

- [`escape()`](https://dbplyr.tidyverse.org/dev/reference/escape.md) now
  supports `blob` vectors using new
  [`sql_escape_raw()`](https://dbplyr.tidyverse.org/dev/reference/db-quote.md)
  generic. It enables using [blob](https://blob.tidyverse.org/)
  variables in dplyr verbs, for example to filter nvarchar values by
  UTF-16 blobs (see
  <https://github.com/r-dbi/DBI/issues/215#issuecomment-356376133>).
  ([@okhoma](https://github.com/okhoma),
  [\#433](https://github.com/tidyverse/dbplyr/issues/433))

- Added [`setOldClass()`](https://rdrr.io/r/methods/setOldClass.html)
  calls for `"ident"` and `"ident_q"` classes for compatibility with
  dplyr 1.0.0 ([\#448](https://github.com/tidyverse/dbplyr/issues/448),
  [@krlmlr](https://github.com/krlmlr)).

- Postgres `str_detect()` translation uses same argument names as
  stringr, and gains a `negate` argument
  ([\#444](https://github.com/tidyverse/dbplyr/issues/444)).

- [`semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)
  and
  [`anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)
  now correctly support the `sql_on` argument
  ([\#443](https://github.com/tidyverse/dbplyr/issues/443),
  [@krlmlr](https://github.com/krlmlr)).

## dbplyr 1.4.3

CRAN release: 2020-04-19

- dbplyr now uses RPostgres (instead of RPostgreSQL) and RMariaDB
  (instead of RMySQL) for its internal tests and data functions
  ([\#427](https://github.com/tidyverse/dbplyr/issues/427)).

- The Date and POSIXt methods for
  [`escape()`](https://dbplyr.tidyverse.org/dev/reference/escape.md) now
  use exported
  [`sql_escape_date()`](https://dbplyr.tidyverse.org/dev/reference/db-quote.md)
  and
  [`sql_escape_datetime()`](https://dbplyr.tidyverse.org/dev/reference/db-quote.md)
  generics to allow backend specific formatting of date and datetime
  literals. These are used to provide methods for Athena and Presto
  backends ([@OssiLehtinen](https://github.com/OssiLehtinen),
  [\#384](https://github.com/tidyverse/dbplyr/issues/384),
  [\#391](https://github.com/tidyverse/dbplyr/issues/391)).

- [`first()`](https://dplyr.tidyverse.org/reference/nth.html),
  [`last()`](https://dplyr.tidyverse.org/reference/nth.html),
  [`nth()`](https://dplyr.tidyverse.org/reference/nth.html),
  [`lead()`](https://dplyr.tidyverse.org/reference/lead-lag.html) and
  [`lag()`](https://dplyr.tidyverse.org/reference/lead-lag.html) now
  respect the
  [`window_frame()`](https://dbplyr.tidyverse.org/dev/reference/window_order.md)
  ([@krlmlr](https://github.com/krlmlr),
  [\#366](https://github.com/tidyverse/dbplyr/issues/366)).

- SQL server: new translations for `str_flatten()`
  ([@PauloJhonny](https://github.com/PauloJhonny),
  [\#405](https://github.com/tidyverse/dbplyr/issues/405)).

- SQL server: temporary datasets are now session-local, not global
  ([\#401](https://github.com/tidyverse/dbplyr/issues/401)).

- Postgres: correct `str_detect()`, `str_replace()` and
  `str_replace_all()` translation
  ([@shosaco](https://github.com/shosaco),
  [\#362](https://github.com/tidyverse/dbplyr/issues/362)).

## dbplyr 1.4.2

CRAN release: 2019-06-17

- Fix bug when partially evaluating unquoting quosure containing a
  single symbol
  ([\#317](https://github.com/tidyverse/dbplyr/issues/317))

- Fixes for rlang and dpylr compatibility.

## dbplyr 1.4.1

CRAN release: 2019-06-05

Minor improvements to SQL generation

- `x %in% y` strips names of `y`
  ([\#269](https://github.com/tidyverse/dbplyr/issues/269)).

- Enhancements for scoped verbs
  ([`mutate_all()`](https://dplyr.tidyverse.org/reference/mutate_all.html),
  [`summarise_if()`](https://dplyr.tidyverse.org/reference/summarise_all.html),
  [`filter_at()`](https://dplyr.tidyverse.org/reference/filter_all.html)
  etc) ([\#296](https://github.com/tidyverse/dbplyr/issues/296),
  [\#306](https://github.com/tidyverse/dbplyr/issues/306)).

- MS SQL use `TOP 100 PERCENT` as stop-gap to allow subqueries with
  `ORDER BY` ([\#277](https://github.com/tidyverse/dbplyr/issues/277)).

- Window functions now translated correctly for Hive
  ([\#293](https://github.com/tidyverse/dbplyr/issues/293),
  [@cderv](https://github.com/cderv)).

## dbplyr 1.4.0

CRAN release: 2019-04-23

### Breaking changes

- `` Error: `con` must not be NULL ``: If you see this error, it
  probably means that you have forgotten to pass `con` down to a dbplyr
  function. Previously, dbplyr defaulted to using
  [`simulate_dbi()`](https://dbplyr.tidyverse.org/dev/reference/simulate_dbi.md)
  which introduced subtle escaping bugs. (It’s also possible I have
  forgotten to pass it somewhere that the dbplyr tests don’t pick up, so
  if you can’t figure it out, please let me know).

- Subsetting (`[[`, `$`, and `[`) functions are no longer evaluated
  locally. This makes the translation more consistent and enables useful
  new idioms for modern databases
  ([\#200](https://github.com/tidyverse/dbplyr/issues/200)).

### New features

- MySQL/MariaDB (<https://mariadb.com/kb/en/library/window-functions/>)
  and SQLite (<https://www.sqlite.org/windowfunctions.html>)
  translations gain support for window functions, available in Maria DB
  10.2, MySQL 8.0, and SQLite 3.25
  ([\#191](https://github.com/tidyverse/dbplyr/issues/191)).

- Overall, dplyr generates many fewer subqueries:

  - Joins and semi-joins no longer add an unneeded subquery
    ([\#236](https://github.com/tidyverse/dbplyr/issues/236)). This is
    facilitated by the new `bare_identifier_ok` argument to
    [`sql_render()`](https://dbplyr.tidyverse.org/dev/reference/sql_build.md);
    the previous argument was called `root` and confused me.

  - Many sequences of
    [`select()`](https://dplyr.tidyverse.org/reference/select.html),
    [`rename()`](https://dplyr.tidyverse.org/reference/rename.html),
    [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html), and
    [`transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
    can be collapsed into a single query, instead of always generating a
    subquery ([\#213](https://github.com/tidyverse/dbplyr/issues/213)).

- New
  [`vignette("sql")`](https://dbplyr.tidyverse.org/dev/articles/sql.md)
  describes some advantages of dbplyr over SQL
  ([\#205](https://github.com/tidyverse/dbplyr/issues/205)) and gives
  some advice about writing literal SQL inside of dplyr, when you need
  to ([\#196](https://github.com/tidyverse/dbplyr/issues/196)).

- New
  [`vignette("reprex")`](https://dbplyr.tidyverse.org/dev/articles/reprex.md)
  gives some hints on creating reprexes that work anywhere
  ([\#117](https://github.com/tidyverse/dbplyr/issues/117)). This is
  supported by a new
  [`tbl_memdb()`](https://dbplyr.tidyverse.org/dev/reference/memdb_frame.md)
  that matches the existing
  [`tbl_lazy()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md).

- All `..._join()` functions gain an `sql_on` argument that allows
  specifying arbitrary join predicates in SQL code
  ([\#146](https://github.com/tidyverse/dbplyr/issues/146),
  [@krlmlr](https://github.com/krlmlr)).

### SQL translations

- New translations for some lubridate functions: `today()`, `now()`,
  `year()`, `month()`, `day()`, `hour()`, `minute()`, `second()`,
  `quarter()`, `yday()` ([@colearendt](https://github.com/colearendt),
  [@derekmorr](https://github.com/derekmorr)). Also added new
  translation for
  [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html).

- New translations for stringr functions: `str_c()`, `str_sub()`,
  `str_length()`, `str_to_upper()`, `str_to_lower()`, and
  `str_to_title()` ([@colearendt](https://github.com/colearendt)).
  Non-translated stringr functions throw a clear error.

- New translations for bitwise operations:
  [`bitwNot()`](https://rdrr.io/r/base/bitwise.html),
  [`bitwAnd()`](https://rdrr.io/r/base/bitwise.html),
  [`bitwOr()`](https://rdrr.io/r/base/bitwise.html),
  [`bitwXor()`](https://rdrr.io/r/base/bitwise.html),
  [`bitwShiftL()`](https://rdrr.io/r/base/bitwise.html), and
  [`bitwShiftR()`](https://rdrr.io/r/base/bitwise.html). Unlike the base
  R functions, the translations do not coerce arguments to integers
  ([@davidchall](https://github.com/davidchall),
  [\#235](https://github.com/tidyverse/dbplyr/issues/235)).

- New translation for `x[y]` to `CASE WHEN y THEN x END`. This enables
  `sum(a[b == 0])` to work as you expect from R
  ([\#202](https://github.com/tidyverse/dbplyr/issues/202)). `y` needs
  to be a logical expression; if not you will likely get a type error
  from your database.

- New translations for `x$y` and `x[["y"]]` to `x.y`, enabling you to
  index into nested fields in databases that provide them
  ([\#158](https://github.com/tidyverse/dbplyr/issues/158)).

- The `.data` and `.env` pronouns of tidy evaluation are correctly
  translated ([\#132](https://github.com/tidyverse/dbplyr/issues/132)).

- New translation for [`median()`](https://rdrr.io/r/stats/median.html)
  and [`quantile()`](https://rdrr.io/r/stats/quantile.html). Works for
  all ANSI compliant databases (SQL Server, Postgres, MariaDB, Teradata)
  and has custom translations for Hive. Thanks to
  [@edavidaja](https://github.com/edavidaja) for researching the SQL
  variants! ([\#169](https://github.com/tidyverse/dbplyr/issues/169))

- [`na_if()`](https://dplyr.tidyverse.org/reference/na_if.html) is
  correct translated to `NULLIF()` (rather than `NULL_IF`)
  ([\#211](https://github.com/tidyverse/dbplyr/issues/211)).

- [`n_distinct()`](https://dplyr.tidyverse.org/reference/n_distinct.html)
  translation throws an error when given more than one argument.
  ([\#101](https://github.com/tidyverse/dbplyr/issues/101),
  [\#133](https://github.com/tidyverse/dbplyr/issues/133)).

- New default translations for
  [`paste()`](https://rdrr.io/r/base/paste.html),
  [`paste0()`](https://rdrr.io/r/base/paste.html), and the hyperbolic
  functions (these previously were only available for ODBC databases).

- Corrected translations of
  [`pmin()`](https://rdrr.io/r/base/Extremes.html) and
  [`pmax()`](https://rdrr.io/r/base/Extremes.html) to `LEAST()` and
  `GREATEST()` for ANSI compliant databases
  ([\#118](https://github.com/tidyverse/dbplyr/issues/118)), to `MIN()`
  and `MAX()` for SQLite, and to an error for SQL server.

- New translation for [`switch()`](https://rdrr.io/r/base/switch.html)
  to the simple form of `CASE WHEN`
  ([\#192](https://github.com/tidyverse/dbplyr/issues/192)).

#### SQL simulation

SQL simulation makes it possible to see what dbplyr will translate SQL
to, without having an active database connection, and is used for
testing and generating reprexes.

- SQL simulation has been overhauled. It now works reliably, is better
  documented, and always uses ANSI escaping (i.e. `` ` `` for field
  names and `'` for strings).

- [`tbl_lazy()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
  now actually puts a
  [`dbplyr::src`](https://dplyr.tidyverse.org/reference/src.html) in the
  `$src` field. This shouldn’t affect any downstream code unless you
  were previously working around this weird difference between
  `tbl_lazy` and `tbl_sql` classes. It also includes the `src` class in
  its class, and when printed, shows the generated SQL
  ([\#111](https://github.com/tidyverse/dbplyr/issues/111)).

### Database specific improvements

- MySQL/MariaDB

  - Translations also applied to connections via the odbc package
    ([@colearendt](https://github.com/colearendt),
    [\#238](https://github.com/tidyverse/dbplyr/issues/238))

  - Basic support for regular expressions via `str_detect()` and  
    `str_replace_all()` ([@colearendt](https://github.com/colearendt),
    [\#168](https://github.com/tidyverse/dbplyr/issues/168)).

  - Improved translation for `as.logical(x)` to `IF(x, TRUE, FALSE)`.

- Oracle

  - New custom translation for
    [`paste()`](https://rdrr.io/r/base/paste.html) and
    [`paste0()`](https://rdrr.io/r/base/paste.html)
    ([@cderv](https://github.com/cderv),
    [\#221](https://github.com/tidyverse/dbplyr/issues/221))

- Postgres

  - Basic support for regular expressions via `str_detect()` and  
    `str_replace_all()` ([@colearendt](https://github.com/colearendt),
    [\#168](https://github.com/tidyverse/dbplyr/issues/168)).

- SQLite

  - [`explain()`](https://dplyr.tidyverse.org/reference/explain.html)
    translation now generates `EXPLAIN QUERY PLAN` which generates a
    higher-level, more human friendly explanation.

- SQL server

  - Improved translation for `as.logical(x)` to `CAST(x as BIT)`
    ([\#250](https://github.com/tidyverse/dbplyr/issues/250)).

  - Translates [`paste()`](https://rdrr.io/r/base/paste.html),
    [`paste0()`](https://rdrr.io/r/base/paste.html), and `str_c()` to
    `+`.

  - [`copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html)
    method applies temporary table name transformation earlier so that
    you can now overwrite temporary tables
    ([\#258](https://github.com/tidyverse/dbplyr/issues/258)).

  - [`db_write_table()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
    method uses correct argument name for passing along field types
    ([\#251](https://github.com/tidyverse/dbplyr/issues/251)).

### Minor improvements and bug fixes

- Aggregation functions only warn once per session about the use of
  `na.rm = TRUE`
  ([\#216](https://github.com/tidyverse/dbplyr/issues/216)).

- table names generated by `random_table_name()` have the prefix
  “dbplyr\_”, which makes it easier to find them programmatically
  ([@mattle24](https://github.com/mattle24),
  [\#111](https://github.com/tidyverse/dbplyr/issues/111))

- Functions that are only available in a windowed
  ([`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html))
  query now throw an error when called in a aggregate
  ([`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html))
  query ([\#129](https://github.com/tidyverse/dbplyr/issues/129))

- [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
  understands the `.by_group` argument, making it possible sort by
  groups if desired. The default is `FALSE`
  ([\#115](https://github.com/tidyverse/dbplyr/issues/115))

- [`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html)
  now handles computed variables like `distinct(df, y = x + y)`
  ([\#154](https://github.com/tidyverse/dbplyr/issues/154)).

- [`escape()`](https://dbplyr.tidyverse.org/dev/reference/escape.md),
  [`sql_expr()`](https://dbplyr.tidyverse.org/dev/reference/sql_expr.md)
  and
  [`build_sql()`](https://dbplyr.tidyverse.org/dev/reference/build_sql.md)
  no longer accept `con = NULL` as a shortcut for
  `con = simulate_dbi()`. This made it too easy to forget to pass `con`
  along, introducing extremely subtle escaping bugs.
  [`win_over()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md)
  gains a `con` argument for the same reason.

- New
  [`escape_ansi()`](https://dbplyr.tidyverse.org/dev/reference/escape.md)
  always uses ANSI SQL 92 standard escaping (for use in examples and
  documentation).

- `mutate(df, x = NULL)` drops `x` from the output, just like when
  working with local data frames
  ([\#194](https://github.com/tidyverse/dbplyr/issues/194)).

- [`partial_eval()`](https://dbplyr.tidyverse.org/dev/reference/partial_eval.md)
  processes inlined functions (including rlang lambda functions). This
  makes dbplyr work with more forms of scoped verbs like
  `df %>% summarise_all(~ mean(.))`, `df %>% summarise_all(list(mean))`
  ([\#134](https://github.com/tidyverse/dbplyr/issues/134)).

- [`sql_aggregate()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_agg.md)
  now takes an optional argument `f_r` for passing to `check_na_rm()`.
  This allows the warning to show the R function name rather than the
  SQL function name ([@sverchkov](https://github.com/sverchkov),
  [\#153](https://github.com/tidyverse/dbplyr/issues/153)).

- [`sql_infix()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_scalar.md)
  gains a `pad` argument for the rare operator that doesn’t need to be
  surrounded by spaces.

- [`sql_prefix()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_scalar.md)
  no longer turns SQL functions into uppercase, allowing for correct
  translation of case-sensitive SQL functions
  ([\#181](https://github.com/tidyverse/dbplyr/issues/181),
  [@mtoto](https://github.com/mtoto)).

- [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
  gives a clear error message if you refer to a variable created in that
  same
  [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
  ([\#114](https://github.com/tidyverse/dbplyr/issues/114)).

- New
  [`sql_call2()`](https://dbplyr.tidyverse.org/dev/reference/sql_expr.md)
  which is to
  [`rlang::call2()`](https://rlang.r-lib.org/reference/call2.html) as
  [`sql_expr()`](https://dbplyr.tidyverse.org/dev/reference/sql_expr.md)
  is to [`rlang::expr()`](https://rlang.r-lib.org/reference/expr.html).

- [`show_query()`](https://dbplyr.tidyverse.org/dev/reference/show_query.md)
  and [`explain()`](https://dplyr.tidyverse.org/reference/explain.html)
  use [`cat()`](https://rdrr.io/r/base/cat.html) rather than message.

- [`union()`](https://generics.r-lib.org/reference/setops.html),
  [`union_all()`](https://dplyr.tidyverse.org/reference/setops.html),
  [`setdiff()`](https://generics.r-lib.org/reference/setops.html) and
  [`intersect()`](https://generics.r-lib.org/reference/setops.html) do a
  better job of matching columns across backends
  ([\#183](https://github.com/tidyverse/dbplyr/issues/183)).

## dbplyr 1.3.0

CRAN release: 2019-01-09

- Now supports for dplyr 0.8.0
  ([\#190](https://github.com/tidyverse/dbplyr/issues/190)) and R 3.1.0

### API changes

- Calls of the form `dplyr::foo()` are now evaluated in the database,
  rather than locally
  ([\#197](https://github.com/tidyverse/dbplyr/issues/197)).

- `vars` argument to
  [`tbl_sql()`](https://dbplyr.tidyverse.org/dev/reference/tbl_sql.md)
  has been formally deprecated; it hasn’t actually done anything for a
  while ([\#3254](https://github.com/tidyverse/dbplyr/issues/3254)).

- `src` and `tbl` objects now include a class generated from the class
  of the underlying connection object. This makes it possible for dplyr
  backends to implement different behaviour at the dplyr level, when
  needed. ([\#2293](https://github.com/tidyverse/dbplyr/issues/2293))

### SQL translation

- `x %in% y` is now translated to `FALSE` if `y` is empty
  ([@mgirlich](https://github.com/mgirlich),
  [\#160](https://github.com/tidyverse/dbplyr/issues/160)).

- New `as.integer64(x)` translation to `CAST(x AS BIGINT)`
  ([\#3305](https://github.com/tidyverse/dbplyr/issues/3305))

- `case_when` now translates with a ELSE clause if a formula of the form
  `TRUE~<RHS>` is provided . ([@cderv](https://github.com/cderv),
  [\#112](https://github.com/tidyverse/dbplyr/issues/112))

- [`cummean()`](https://dplyr.tidyverse.org/reference/cumall.html) now
  generates `AVG()` not `MEAN()`
  ([\#157](https://github.com/tidyverse/dbplyr/issues/157))

- `str_detect()` now uses correct parameter order
  ([\#3397](https://github.com/tidyverse/dbplyr/issues/3397))

- MS SQL

  - Cumulative summary functions now work
    ([\#157](https://github.com/tidyverse/dbplyr/issues/157))
  - [`ifelse()`](https://rdrr.io/r/base/ifelse.html) uses `CASE WHEN`
    instead of `IIF`; this allows more complex operations, such as
    `%in%`, to work properly
    ([\#93](https://github.com/tidyverse/dbplyr/issues/93))

- Oracle

  - Custom
    [`db_drop_table()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
    now only drops tables if they exist
    ([\#3306](https://github.com/tidyverse/dbplyr/issues/3306))
  - Custom
    [`setdiff()`](https://generics.r-lib.org/reference/setops.html)
    translation
    ([\#3493](https://github.com/tidyverse/dbplyr/issues/3493))
  - Custom
    [`db_explain()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
    translation
    ([\#3471](https://github.com/tidyverse/dbplyr/issues/3471))

- SQLite

  - Correct translation for
    [`as.numeric()`](https://rdrr.io/r/base/numeric.html)/[`as.double()`](https://rdrr.io/r/base/double.html)
    ([@chris-park](https://github.com/chris-park),
    [\#171](https://github.com/tidyverse/dbplyr/issues/171)).

- Redshift

  - [`substr()`](https://rdrr.io/r/base/substr.html) translation
    improved ([\#3339](https://github.com/tidyverse/dbplyr/issues/3339))

### Minor improvements and bug fixes

- [`copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html) will
  only remove existing table when `overwrite = TRUE` and the table
  already exists, eliminating a confusing “NOTICE” from PostgreSQL
  ([\#3197](https://github.com/tidyverse/dbplyr/issues/3197)).

- [`partial_eval()`](https://dbplyr.tidyverse.org/dev/reference/partial_eval.md)
  handles unevaluated formulas
  ([\#184](https://github.com/tidyverse/dbplyr/issues/184)).

- [`pull.tbl_sql()`](https://dbplyr.tidyverse.org/dev/reference/pull.tbl_sql.md)
  now extracts correctly from grouped tables
  ([\#3562](https://github.com/tidyverse/dbplyr/issues/3562)).

- `sql_render.op()` now correctly forwards the `con` argument
  ([@kevinykuo](https://github.com/kevinykuo),
  [\#73](https://github.com/tidyverse/dbplyr/issues/73)).

## dbplyr 1.2.2

CRAN release: 2018-07-25

- R CMD check fixes

## dbplyr 1.2.1

CRAN release: 2018-02-19

- Forward compatibility fixes for rlang 0.2.0

## dbplyr 1.2.0

CRAN release: 2018-01-03

### New top-level translations

- New translations for

  - MS Access
    ([\#2946](https://github.com/tidyverse/dbplyr/issues/2946))
    ([@DavisVaughan](https://github.com/DavisVaughan))
  - Oracle, via odbc or ROracle
    ([\#2928](https://github.com/tidyverse/dbplyr/issues/2928),
    [\#2732](https://github.com/tidyverse/dbplyr/issues/2732),
    [@edgararuiz](https://github.com/edgararuiz))
  - Teradata.
  - Redshift.

- dbplyr now supplies appropriate translations for the RMariaDB and
  RPostgres packages
  ([\#3154](https://github.com/tidyverse/dbplyr/issues/3154)). We
  generally recommend using these packages in favour of the older RMySQL
  and RPostgreSQL packages as they are fully DBI compliant and tested
  with DBItest.

### New features

- [`copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html) can
  now “copy” tbl_sql in the same src, providing another way to cache a
  query into a temporary table
  ([\#3064](https://github.com/tidyverse/dbplyr/issues/3064)). You can
  also `copy_to` tbl_sqls from another source, and
  [`copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html) will
  automatically collect then copy.

- Initial support for stringr functions: `str_length()`,
  `str_to_upper()`, `str_to_lower()`, `str_replace_all()`,
  `str_detect()`, `str_trim()`. Regular expression support varies from
  database to database, but most simple regular expressions should be
  ok.

### Tools for developers

- [`db_compute()`](https://dbplyr.tidyverse.org/dev/reference/db-io.md)
  gains an `analyze` argument to match
  [`db_copy_to()`](https://dbplyr.tidyverse.org/dev/reference/db-io.md).

- New
  [`remote_name()`](https://dbplyr.tidyverse.org/dev/reference/remote_name.md),
  [`remote_con()`](https://dbplyr.tidyverse.org/dev/reference/remote_name.md),
  [`remote_src()`](https://dbplyr.tidyverse.org/dev/reference/remote_name.md),
  [`remote_query()`](https://dbplyr.tidyverse.org/dev/reference/remote_name.md)
  and
  [`remote_query_plan()`](https://dbplyr.tidyverse.org/dev/reference/remote_name.md)
  provide a standard API for get metadata about a remote tbl
  ([\#3130](https://github.com/tidyverse/dbplyr/issues/3130),
  [\#2923](https://github.com/tidyverse/dbplyr/issues/2923),
  [\#2824](https://github.com/tidyverse/dbplyr/issues/2824)).

- New
  [`sql_expr()`](https://dbplyr.tidyverse.org/dev/reference/sql_expr.md)
  is a more convenient building block for low-level SQL translation
  ([\#3169](https://github.com/tidyverse/dbplyr/issues/3169)).

- New
  [`sql_aggregate()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_agg.md)
  and
  [`win_aggregate()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md)
  for generating SQL and windowed SQL functions for aggregates. These
  take one argument, `x`, and warn if `na.rm` is not `TRUE`
  ([\#3155](https://github.com/tidyverse/dbplyr/issues/3155)).
  [`win_recycled()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md)
  is equivalent to
  [`win_aggregate()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md)
  and has been soft-deprecated.

- `db_write_table` now needs to return the table name

### Minor improvements and bug fixes

- Multiple [`head()`](https://rdrr.io/r/utils/head.html) calls in a row
  now collapse to a single call. This avoids a printing problem with MS
  SQL ([\#3084](https://github.com/tidyverse/dbplyr/issues/3084)).

- [`escape()`](https://dbplyr.tidyverse.org/dev/reference/escape.md) now
  works with integer64 values from the bit64 package
  ([\#3230](https://github.com/tidyverse/dbplyr/issues/3230))

- `if`, [`ifelse()`](https://rdrr.io/r/base/ifelse.html), and
  [`if_else()`](https://dplyr.tidyverse.org/reference/if_else.html) now
  correctly scope the false condition so that it only applies to
  non-NULL conditions
  ([\#3157](https://github.com/tidyverse/dbplyr/issues/3157))

- [`ident()`](https://dbplyr.tidyverse.org/dev/reference/ident.md) and
  [`ident_q()`](https://dbplyr.tidyverse.org/dev/reference/ident_q.md)
  handle 0-length inputs better, and should be easier to use with S3
  ([\#3212](https://github.com/tidyverse/dbplyr/issues/3212))

- [`in_schema()`](https://dbplyr.tidyverse.org/dev/reference/in_schema.md)
  should now work in more places, particularly in
  [`copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html)
  ([\#3013](https://github.com/tidyverse/dbplyr/issues/3013),
  [@baileych](https://github.com/baileych))

- SQL generation for joins no longer gets stuck in a endless loop if you
  request an empty suffix
  ([\#3220](https://github.com/tidyverse/dbplyr/issues/3220)).

- [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) has
  better logic for splitting a single mutate into multiple subqueries
  ([\#3095](https://github.com/tidyverse/dbplyr/issues/3095)).

- Improved [`paste()`](https://rdrr.io/r/base/paste.html) and
  [`paste0()`](https://rdrr.io/r/base/paste.html) support in MySQL,
  PostgreSQL
  ([\#3168](https://github.com/tidyverse/dbplyr/issues/3168)), and
  RSQLite ([\#3176](https://github.com/tidyverse/dbplyr/issues/3176)).
  MySQL and PostgreSQL gain support for `str_flatten()` which behaves
  like `paste(x, collapse = "-")` (but for technical reasons can’t be
  implemented as a straightforward translation of
  [`paste()`](https://rdrr.io/r/base/paste.html)).

- `same_src.tbl_sql()` now performs correct comparison instead of always
  returning `TRUE`. This means that `copy = TRUE` once again allows you
  to perform cross-database joins
  ([\#3002](https://github.com/tidyverse/dbplyr/issues/3002)).

- [`select()`](https://dplyr.tidyverse.org/reference/select.html)
  queries no longer alias column names unnecessarily
  ([\#2968](https://github.com/tidyverse/dbplyr/issues/2968),
  [@DavisVaughan](https://github.com/DavisVaughan)).

- [`select()`](https://dplyr.tidyverse.org/reference/select.html) and
  [`rename()`](https://dplyr.tidyverse.org/reference/rename.html) are
  now powered by tidyselect, fixing a few renaming bugs
  ([\#3132](https://github.com/tidyverse/dbplyr/issues/3132),
  [\#2943](https://github.com/tidyverse/dbplyr/issues/2943),
  [\#2860](https://github.com/tidyverse/dbplyr/issues/2860)).

- [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
  once again performs partial evaluation before database submission
  ([\#3148](https://github.com/tidyverse/dbplyr/issues/3148)).

- `test_src()` makes it easier to access a single test source.

### Database specific improvements

- MS SQL

  - Better support for temporary tables
    ([@Hong-Revo](https://github.com/Hong-Revo))

  - Different translations for filter/mutate contexts for: `NULL`
    evaluation ([`is.na()`](https://rdrr.io/r/base/NA.html),
    [`is.null()`](https://rdrr.io/r/base/NULL.html)), logical operators
    (`!`, `&`, `&&`, `|`, `||`), and comparison operators (`==`, `!=`,
    `<`, `>`, `>=`, `<=`)

- MySQL:
  [`copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html) (via
  [`db_write_table()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html))
  correctly translates logical variables to integers
  ([\#3151](https://github.com/tidyverse/dbplyr/issues/3151)).

- odbc: improved
  [`n()`](https://dplyr.tidyverse.org/reference/context.html)
  translation in windowed context.

- SQLite: improved `na_if` translation
  ([@cwarden](https://github.com/cwarden))

- PostgreSQL: translation for
  [`grepl()`](https://rdrr.io/r/base/grep.html) added
  ([@zozlak](https://github.com/zozlak))

- Oracle: changed VARVHAR to VARCHAR2 datatype
  ([@washcycle](https://github.com/washcycle),
  [\#66](https://github.com/tidyverse/dbplyr/issues/66))

## dbplyr 1.1.0

CRAN release: 2017-06-27

### New features

- [`full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  over non-overlapping columns `by = character()` translated to
  `CROSS JOIN`
  ([\#2924](https://github.com/tidyverse/dbplyr/issues/2924)).

- [`case_when()`](https://dplyr.tidyverse.org/reference/case_when.html)
  now translates to SQL “CASE WHEN”
  ([\#2894](https://github.com/tidyverse/dbplyr/issues/2894))

- `x %in% c(1)` now generates the same SQL as `x %in% 1`
  ([\#2898](https://github.com/tidyverse/dbplyr/issues/2898)).

- New
  [`window_order()`](https://dbplyr.tidyverse.org/dev/reference/window_order.md)
  and
  [`window_frame()`](https://dbplyr.tidyverse.org/dev/reference/window_order.md)
  give you finer control over the window functions that dplyr creates
  ([\#2874](https://github.com/tidyverse/dbplyr/issues/2874),
  [\#2593](https://github.com/tidyverse/dbplyr/issues/2593)).

- Added SQL translations for Oracle
  ([@edgararuiz](https://github.com/edgararuiz)).

### Minor improvements and bug fixes

- `x %in% c(1)` now generates the same SQL as `x %in% 1`
  ([\#2898](https://github.com/tidyverse/dbplyr/issues/2898)).

- `head(tbl, 0)` is now supported
  ([\#2863](https://github.com/tidyverse/dbplyr/issues/2863)).

- [`select()`](https://dplyr.tidyverse.org/reference/select.html)ing
  zero columns gives a more information error message
  ([\#2863](https://github.com/tidyverse/dbplyr/issues/2863)).

- Variables created in a join are now disambiguated against other
  variables in the same table, not just variables in the other table
  ([\#2823](https://github.com/tidyverse/dbplyr/issues/2823)).

- PostgreSQL gains a better translation for
  [`round()`](https://rdrr.io/r/base/Round.html)
  ([\#60](https://github.com/tidyverse/dbplyr/issues/60)).

- Added custom `db_analyze_table()` for MS SQL, Oracle, Hive and Impala
  ([@edgararuiz](https://github.com/edgararuiz))

- Added support for [`sd()`](https://rdrr.io/r/stats/sd.html) for
  aggregate and window functions
  ([\#2887](https://github.com/tidyverse/dbplyr/issues/2887))
  ([@edgararuiz](https://github.com/edgararuiz))

- You can now use the magrittr pipe within expressions,
  e.g. `mutate(mtcars, cyl %>% as.character())`.

- If a translation was supplied for a summarise function, but not for
  the equivalent windowed variant, the expression would be translated to
  `NULL` with a warning. Now
  [`sql_variant()`](https://dbplyr.tidyverse.org/dev/reference/sql_variant.md)
  checks that all aggregate functions have matching window functions so
  that correct translations or clean errors will be generated
  ([\#2887](https://github.com/tidyverse/dbplyr/issues/2887))

## dbplyr 1.0.0

CRAN release: 2017-06-09

### New features

- [`tbl()`](https://dplyr.tidyverse.org/reference/tbl.html) and
  [`copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html) now
  work directly with DBI connections
  ([\#2423](https://github.com/tidyverse/dbplyr/issues/2423),
  [\#2576](https://github.com/tidyverse/dbplyr/issues/2576)), so there
  is no longer a need to generate a dplyr src.

  ``` r
  library(dplyr)

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  copy_to(con, mtcars)

  mtcars2 <- tbl(con, "mtcars")
  mtcars2
  ```

- [`glimpse()`](https://pillar.r-lib.org/reference/glimpse.html) now
  works with remote tables
  ([\#2665](https://github.com/tidyverse/dbplyr/issues/2665))

- dplyr has gained a basic SQL optimiser, which collapses certain nested
  SELECT queries into a single query
  ([\#1979](https://github.com/tidyverse/dbplyr/issues/1979)). This will
  improve query execution performance for databases with less
  sophisticated query optimisers, and fixes certain problems with
  ordering and limits in subqueries
  ([\#1979](https://github.com/tidyverse/dbplyr/issues/1979)). A big
  thanks goes to [@hhoeflin](https://github.com/hhoeflin) for figuring
  out this optimisation.

- [`compute()`](https://dplyr.tidyverse.org/reference/compute.html) and
  [`collapse()`](https://dplyr.tidyverse.org/reference/compute.html) now
  preserve the “ordering” of rows. This only affects the computation of
  window functions, as the rest of SQL does not care about row order
  ([\#2281](https://github.com/tidyverse/dbplyr/issues/2281)).

- [`copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html)
  gains an `overwrite` argument which allows you to overwrite an
  existing table. Use with care!
  ([\#2296](https://github.com/tidyverse/dbplyr/issues/2296))

- New
  [`in_schema()`](https://dbplyr.tidyverse.org/dev/reference/in_schema.md)
  function makes it easy to refer to tables in schema:
  `in_schema("my_schema_name", "my_table_name")`.

### Deprecated and defunct

- `query()` is no longer exported. It hasn’t been useful for a while so
  this shouldn’t break any code.

### Verb-level SQL generation

- Partial evaluation occurs immediately when you execute a verb (like
  [`filter()`](https://dplyr.tidyverse.org/reference/filter.html) or
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html))
  rather than happening when the query is executed
  ([\#2370](https://github.com/tidyverse/dbplyr/issues/2370)).

- `mutate.tbl_sql()` will now generate as many subqueries as necessary
  so that you can refer to variables that you just created (like in
  mutate with regular dataframes)
  ([\#2481](https://github.com/tidyverse/dbplyr/issues/2481),
  [\#2483](https://github.com/tidyverse/dbplyr/issues/2483)).

- SQL joins have been improved:

  - SQL joins always use the `ON ...` syntax, avoiding `USING ...` even
    for natural joins. Improved handling of tables with columns of the
    same name
    ([\#1997](https://github.com/tidyverse/dbplyr/issues/1997),
    [@javierluraschi](https://github.com/javierluraschi)). They now
    generate SQL more similar to what you’d write by hand, eliminating a
    layer or two of subqueries
    ([\#2333](https://github.com/tidyverse/dbplyr/issues/2333))

  - \[API\] They now follow the same rules for including duplicated key
    variables that the data frame methods do, namely that key variables
    are only kept from `x`, and never from `y`
    ([\#2410](https://github.com/tidyverse/dbplyr/issues/2410))

  - \[API\] The
    [`sql_join()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
    generic now gains a `vars` argument which lists the variables taken
    from the left and right sides of the join. If you have a custom
    [`sql_join()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
    method, you’ll need to update how your code generates joins,
    following the template in `sql_join.generic()`.

  - [`full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
    throws a clear error when you attempt to use it with a MySQL backend
    ([\#2045](https://github.com/tidyverse/dbplyr/issues/2045))

  - [`right_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
    and
    [`full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
    now return results consistent with local data frame sources when
    there are records in the right table with no match in the left
    table.
    [`right_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
    returns values of `by` columns from the right table.
    [`full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
    returns coalesced values of `by` columns from the left and right
    tables ([\#2578](https://github.com/tidyverse/dbplyr/issues/2578),
    [@ianmcook](https://github.com/ianmcook))

- [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  can now perform an inline mutate for database backends
  ([\#2422](https://github.com/tidyverse/dbplyr/issues/2422)).

- The SQL generation set operations
  ([`intersect()`](https://generics.r-lib.org/reference/setops.html),
  [`setdiff()`](https://generics.r-lib.org/reference/setops.html),
  [`union()`](https://generics.r-lib.org/reference/setops.html), and
  [`union_all()`](https://dplyr.tidyverse.org/reference/setops.html))
  have been considerably improved.

  By default, the component SELECT are surrounded with parentheses,
  except on SQLite. The SQLite backend will now throw an error if you
  attempt a set operation on a query that contains a LIMIT, as that is
  not supported in SQLite
  ([\#2270](https://github.com/tidyverse/dbplyr/issues/2270)).

  All set operations match column names across inputs, filling in
  non-matching variables with NULL
  ([\#2556](https://github.com/tidyverse/dbplyr/issues/2556)).

- [`rename()`](https://dplyr.tidyverse.org/reference/rename.html) and
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  now combine correctly
  ([\#1962](https://github.com/tidyverse/dbplyr/issues/1962))

- [`tbl_lazy()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
  and `lazy_tbl()` have been exported. These help you test generated SQL
  with out an active database connection.

- [`ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html)
  correctly resets grouping variables
  ([\#2704](https://github.com/tidyverse/dbplyr/issues/2704)).

### Vector-level SQL generation

- New [`as.sql()`](https://dbplyr.tidyverse.org/dev/reference/sql.md)
  safely coerces an input to SQL.

- More translators for
  [`as.character()`](https://rdrr.io/r/base/character.html),
  [`as.integer()`](https://rdrr.io/r/base/integer.html) and
  [`as.double()`](https://rdrr.io/r/base/double.html)
  ([\#2775](https://github.com/tidyverse/dbplyr/issues/2775)).

- New
  [`ident_q()`](https://dbplyr.tidyverse.org/dev/reference/ident_q.md)
  makes it possible to specifier identifiers that do not need to be
  quoted.

- Translation of inline scalars:

  - Logical values are now translated differently depending on the
    backend. The default is to use “true” and “false” which is the
    SQL-99 standard, but not widely support. SQLite translates to “0”
    and “1” ([\#2052](https://github.com/tidyverse/dbplyr/issues/2052)).

  - `Inf` and `-Inf` are correctly escaped

  - Better test for whether or not a double is similar to an integer and
    hence needs a trailing 0.0 added
    ([\#2004](https://github.com/tidyverse/dbplyr/issues/2004)).

  - Quoting defaults to `DBI::dbEscapeString()` and
    [`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html)
    respectively.

- `::` and `:::` are handled correctly
  ([\#2321](https://github.com/tidyverse/dbplyr/issues/2321))

- `x %in% 1` is now correctly translated to `x IN (1)`
  ([\#511](https://github.com/tidyverse/dbplyr/issues/511)).

- [`ifelse()`](https://rdrr.io/r/base/ifelse.html) and
  [`if_else()`](https://dplyr.tidyverse.org/reference/if_else.html) use
  correct argument names in SQL translation
  ([\#2225](https://github.com/tidyverse/dbplyr/issues/2225)).

- [`ident()`](https://dbplyr.tidyverse.org/dev/reference/ident.md) now
  returns an object with class `c("ident", "character")`. It no longer
  contains “sql” to indicate that this is not already escaped.

- [`is.na()`](https://rdrr.io/r/base/NA.html) and
  [`is.null()`](https://rdrr.io/r/base/NULL.html) gain extra parens in
  SQL translation to preserve correct precedence
  ([\#2302](https://github.com/tidyverse/dbplyr/issues/2302)).

- \[API\] `log(x, b)` is now correctly translated to the SQL `log(b, x)`
  ([\#2288](https://github.com/tidyverse/dbplyr/issues/2288)). SQLite
  does not support the 2-argument log function so it is translated to
  `log(x) / log(b)`.

- `nth(x, i)` is now correctly translated to `nth_value(x, i)`.

- [`n_distinct()`](https://dplyr.tidyverse.org/reference/n_distinct.html)
  now accepts multiple variables
  ([\#2148](https://github.com/tidyverse/dbplyr/issues/2148)).

- \[API\] [`substr()`](https://rdrr.io/r/base/substr.html) is now
  translated to SQL, correcting for the difference in the third
  argument. In R, it’s the position of the last character, in SQL it’s
  the length of the string
  ([\#2536](https://github.com/tidyverse/dbplyr/issues/2536)).

- [`win_over()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md)
  escapes expression using current database rules.

### Backends

- [`copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html) now
  uses
  [`db_write_table()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  instead of
  [`db_create_table()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  and
  [`db_insert_into()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html).
  `db_write_table.DBIConnection()` uses
  [`dbWriteTable()`](https://dbi.r-dbi.org/reference/dbWriteTable.html).

- New
  [`db_copy_to()`](https://dbplyr.tidyverse.org/dev/reference/db-io.md),
  [`db_compute()`](https://dbplyr.tidyverse.org/dev/reference/db-io.md)
  and
  [`db_collect()`](https://dbplyr.tidyverse.org/dev/reference/db-io.md)
  allow backends to override the entire database process behind
  [`copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html),
  [`compute()`](https://dplyr.tidyverse.org/reference/compute.html) and
  [`collect()`](https://dplyr.tidyverse.org/reference/compute.html).
  [`db_sql_render()`](https://dbplyr.tidyverse.org/dev/reference/db-misc.md)
  allow additional control over the SQL rendering process.

- All generics whose behaviour can vary from database to database now
  provide a DBIConnection method. That means that you can easily scan
  the NAMESPACE to see the extension points.

- [`sql_escape_logical()`](https://dbplyr.tidyverse.org/dev/reference/db-quote.md)
  allows you to control the translation of literal logicals
  ([\#2614](https://github.com/tidyverse/dbplyr/issues/2614)).

- `src_desc()` has been replaced by
  [`db_desc()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  and now dispatches on the connection, eliminating the last method that
  required dispatch on the class of the src.

- [`win_over()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md),
  [`win_rank()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md),
  [`win_recycled()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md),
  [`win_cumulative()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md),
  [`win_current_group()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md)
  and
  [`win_current_order()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md)
  are now exported. This should make it easier to provide customised SQL
  for window functions
  ([\#2051](https://github.com/tidyverse/dbplyr/issues/2051),
  [\#2126](https://github.com/tidyverse/dbplyr/issues/2126)).

- SQL translation for Microsoft SQL Server
  ([@edgararuiz](https://github.com/edgararuiz))

- SQL translation for Apache Hive
  ([@edgararuiz](https://github.com/edgararuiz))

- SQL translation for Apache Impala
  ([@edgararuiz](https://github.com/edgararuiz))

### Minor bug fixes and improvements

- [`collect()`](https://dplyr.tidyverse.org/reference/compute.html) once
  again defaults to return all rows in the data
  ([\#1968](https://github.com/tidyverse/dbplyr/issues/1968)). This
  makes it behave the same as
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) and
  [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html).

- [`collect()`](https://dplyr.tidyverse.org/reference/compute.html) only
  regroups by variables present in the data
  ([\#2156](https://github.com/tidyverse/dbplyr/issues/2156))

- [`collect()`](https://dplyr.tidyverse.org/reference/compute.html) will
  automatically LIMIT the result to the `n`, the number of rows
  requested. This will provide the query planner with more information
  that it may be able to use to improve execution time
  ([\#2083](https://github.com/tidyverse/dbplyr/issues/2083)).

- [`common_by()`](https://dplyr.tidyverse.org/reference/common_by.html)
  gets a better error message for unexpected inputs
  ([\#2091](https://github.com/tidyverse/dbplyr/issues/2091))

- [`copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html) no
  longer checks that the table doesn’t exist before creation, instead
  preferring to fall back on the database for error messages. This
  should reduce both false positives and false negative
  ([\#1470](https://github.com/tidyverse/dbplyr/issues/1470))

- [`copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html) now
  succeeds for MySQL if a character column contains `NA`
  ([\#1975](https://github.com/tidyverse/dbplyr/issues/1975),
  [\#2256](https://github.com/tidyverse/dbplyr/issues/2256),
  [\#2263](https://github.com/tidyverse/dbplyr/issues/2263),
  [\#2381](https://github.com/tidyverse/dbplyr/issues/2381),
  [@demorenoc](https://github.com/demorenoc),
  [@eduardgrebe](https://github.com/eduardgrebe)).

- [`copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html) now
  returns its output invisibly (since you’re often just calling for the
  side-effect).

- [`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html)
  reports improved variable information for SQL backends. This means
  that it is more likely to work in the middle of a pipeline
  ([\#2359](https://github.com/tidyverse/dbplyr/issues/2359)).

- Ungrouped [`do()`](https://dplyr.tidyverse.org/reference/do.html) on
  database backends now collects all data locally first
  ([\#2392](https://github.com/tidyverse/dbplyr/issues/2392)).

- Call [`dbFetch()`](https://dbi.r-dbi.org/reference/dbFetch.html)
  instead of the deprecated
  [`fetch()`](https://dbi.r-dbi.org/reference/dbFetch.html)
  ([\#2134](https://github.com/tidyverse/dbplyr/issues/2134)). Use
  [`DBI::dbExecute()`](https://dbi.r-dbi.org/reference/dbExecute.html)
  for non-query SQL commands
  ([\#1912](https://github.com/tidyverse/dbplyr/issues/1912))

- [`explain()`](https://dplyr.tidyverse.org/reference/explain.html) and
  [`show_query()`](https://dbplyr.tidyverse.org/dev/reference/show_query.md)
  now invisibly return the first argument, making them easier to use
  inside a pipeline.

- `print.tbl_sql()` displays ordering
  ([\#2287](https://github.com/tidyverse/dbplyr/issues/2287)) and prints
  table name, if known.

- `print(df, n = Inf)` and `head(df, n = Inf)` now work with remote
  tables ([\#2580](https://github.com/tidyverse/dbplyr/issues/2580)).

- [`db_desc()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  and
  [`sql_translate_env()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  get defaults for DBIConnection.

- Formatting now works by overriding the `tbl_sum()` generic instead of
  [`print()`](https://rdrr.io/r/base/print.html). This means that the
  output is more consistent with tibble, and that
  [`format()`](https://rdrr.io/r/base/format.html) is now supported also
  for SQL sources (tidyverse/dbplyr#14).

### Lazy ops

- \[API\] The signature of `op_base` has changed to
  `op_base(x, vars, class)`

- \[API\]
  [`translate_sql()`](https://dbplyr.tidyverse.org/dev/reference/translate_sql.md)
  and
  [`partial_eval()`](https://dbplyr.tidyverse.org/dev/reference/partial_eval.md)
  have been refined:

  - [`translate_sql()`](https://dbplyr.tidyverse.org/dev/reference/translate_sql.md)
    no longer takes a vars argument; instead call
    [`partial_eval()`](https://dbplyr.tidyverse.org/dev/reference/partial_eval.md)
    yourself.

  - Because it no longer needs the environment `translate_sql()_` now
    works with a list of dots, rather than a `lazy_dots`.

  - [`partial_eval()`](https://dbplyr.tidyverse.org/dev/reference/partial_eval.md)
    now takes a character vector of variable names rather than a tbl.

  - This leads to a simplification of the `op` data structure: dots is
    now a list of expressions rather than a `lazy_dots`.

- \[API\]
  [`op_vars()`](https://dbplyr.tidyverse.org/dev/reference/lazy_ops.md)
  now returns a list of quoted expressions. This enables escaping to
  happen at the correct time (i.e. when the connection is known).
