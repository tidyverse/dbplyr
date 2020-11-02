# chunked

<details>

* Version: 0.5.0
* GitHub: https://github.com/edwindj/chunked
* Source code: https://github.com/cran/chunked
* Date/Publication: 2020-03-24 08:20:02 UTC
* Number of recursive dependencies: 52

Run `cloud_details(, "chunked")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       1. ├─chunked::write_chunkwise(tbl_iris, db, "iris") test-write.R:26:2
       2. └─chunked:::write_chunkwise.chunkwise(tbl_iris, db, "iris")
       3.   └─chunked::insert_chunkwise_into(x, dest, table, ...)
       4.     └─dplyr::db_begin(con)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      ERROR (test-insert-into.R:9:5): insert_chunkwise_into: can insert into a db
      ERROR (test-insert-into.R:18:5): insert_chunkwise_into: can insert modified table into a db
      ERROR (test-write.R:26:3): write_chunkwise to db works
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 42 ]
      Error: Test failures
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

# duckdb

<details>

* Version: 0.2.1-2
* GitHub: https://github.com/cwida/duckdb
* Source code: https://github.com/cran/duckdb
* Date/Publication: 2020-09-30 08:00:07 UTC
* Number of recursive dependencies: 47

Run `cloud_details(, "duckdb")` for more info

</details>

## Newly broken

*   checking whether package ‘duckdb’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/duckdb/new/duckdb.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is 179.6Mb
      sub-directories of 1Mb or more:
        libs  179.3Mb
    ```

## Installation

### Devel

```
* installing *source* package ‘duckdb’ ...
** package ‘duckdb’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdbr.cpp -o duckdbr.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/extension/parquet/parquet-extension.cpp -o duckdb/extension/parquet/parquet-extension.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/extension/parquet/parquet_reader.cpp -o duckdb/extension/parquet/parquet_reader.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/extension/parquet/parquet_timestamp.cpp -o duckdb/extension/parquet/parquet_timestamp.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/extension/parquet/parquet_writer.cpp -o duckdb/extension/parquet/parquet_writer.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/amalgamation-1.cpp -o duckdb/amalgamation-1.o
In file included from duckdb/amalgamation-1.cpp:153:
duckdb/src/execution/expression_executor/execute_constant.cpp:19:1: fatal error: error writing to /tmp/ccsRqZd0.s: No space left on device
   19 | } // namespace duckdb
      | ^
compilation terminated.
make: *** [/usr/lib/R/etc/Makeconf:177: duckdb/amalgamation-1.o] Error 1
ERROR: compilation failed for package ‘duckdb’
* removing ‘/tmp/workdir/duckdb/new/duckdb.Rcheck/duckdb’

```
### CRAN

```
* installing *source* package ‘duckdb’ ...
** package ‘duckdb’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdbr.cpp -o duckdbr.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/extension/parquet/parquet-extension.cpp -o duckdb/extension/parquet/parquet-extension.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/extension/parquet/parquet_reader.cpp -o duckdb/extension/parquet/parquet_reader.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/extension/parquet/parquet_timestamp.cpp -o duckdb/extension/parquet/parquet_timestamp.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/extension/parquet/parquet_writer.cpp -o duckdb/extension/parquet/parquet_writer.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/amalgamation-1.cpp -o duckdb/amalgamation-1.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/amalgamation-2.cpp -o duckdb/amalgamation-2.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/amalgamation-3.cpp -o duckdb/amalgamation-3.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/amalgamation-4.cpp -o duckdb/amalgamation-4.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/amalgamation-5.cpp -o duckdb/amalgamation-5.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/amalgamation-6.cpp -o duckdb/amalgamation-6.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/amalgamation-7.cpp -o duckdb/amalgamation-7.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/fmt/format.cc -o duckdb/third_party/fmt/format.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/libpg_query/pg_functions.cpp -o duckdb/third_party/libpg_query/pg_functions.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/libpg_query/postgres_parser.cpp -o duckdb/third_party/libpg_query/postgres_parser.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/libpg_query/src_backend_nodes_list.cpp -o duckdb/third_party/libpg_query/src_backend_nodes_list.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/libpg_query/src_backend_nodes_makefuncs.cpp -o duckdb/third_party/libpg_query/src_backend_nodes_makefuncs.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/libpg_query/src_backend_nodes_value.cpp -o duckdb/third_party/libpg_query/src_backend_nodes_value.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/libpg_query/src_backend_parser_gram.cpp -o duckdb/third_party/libpg_query/src_backend_parser_gram.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/libpg_query/src_backend_parser_parser.cpp -o duckdb/third_party/libpg_query/src_backend_parser_parser.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/libpg_query/src_backend_parser_scan.cpp -o duckdb/third_party/libpg_query/src_backend_parser_scan.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/libpg_query/src_backend_parser_scansup.cpp -o duckdb/third_party/libpg_query/src_backend_parser_scansup.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/libpg_query/src_common_keywords.cpp -o duckdb/third_party/libpg_query/src_common_keywords.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/miniz/miniz.cpp -o duckdb/third_party/miniz/miniz.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/parquet/parquet_constants.cpp -o duckdb/third_party/parquet/parquet_constants.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/parquet/parquet_types.cpp -o duckdb/third_party/parquet/parquet_types.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/re2/re2/bitstate.cc -o duckdb/third_party/re2/re2/bitstate.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/re2/re2/compile.cc -o duckdb/third_party/re2/re2/compile.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/re2/re2/dfa.cc -o duckdb/third_party/re2/re2/dfa.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/re2/re2/filtered_re2.cc -o duckdb/third_party/re2/re2/filtered_re2.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/re2/re2/mimics_pcre.cc -o duckdb/third_party/re2/re2/mimics_pcre.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/re2/re2/nfa.cc -o duckdb/third_party/re2/re2/nfa.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/re2/re2/onepass.cc -o duckdb/third_party/re2/re2/onepass.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/re2/re2/parse.cc -o duckdb/third_party/re2/re2/parse.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/re2/re2/perl_groups.cc -o duckdb/third_party/re2/re2/perl_groups.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/re2/re2/prefilter.cc -o duckdb/third_party/re2/re2/prefilter.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/re2/re2/prefilter_tree.cc -o duckdb/third_party/re2/re2/prefilter_tree.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/re2/re2/prog.cc -o duckdb/third_party/re2/re2/prog.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/re2/re2/re2.cc -o duckdb/third_party/re2/re2/re2.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/re2/re2/regexp.cc -o duckdb/third_party/re2/re2/regexp.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/re2/re2/set.cc -o duckdb/third_party/re2/re2/set.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/re2/re2/simplify.cc -o duckdb/third_party/re2/re2/simplify.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/re2/re2/stringpiece.cc -o duckdb/third_party/re2/re2/stringpiece.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/re2/re2/tostring.cc -o duckdb/third_party/re2/re2/tostring.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/re2/re2/unicode_casefold.cc -o duckdb/third_party/re2/re2/unicode_casefold.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/re2/re2/unicode_groups.cc -o duckdb/third_party/re2/re2/unicode_groups.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/re2/util/rune.cc -o duckdb/third_party/re2/util/rune.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/re2/util/strutil.cc -o duckdb/third_party/re2/util/strutil.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/snappy/snappy-sinksource.cc -o duckdb/third_party/snappy/snappy-sinksource.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/snappy/snappy.cc -o duckdb/third_party/snappy/snappy.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/thrift/thrift/protocol/TProtocol.cpp -o duckdb/third_party/thrift/thrift/protocol/TProtocol.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/thrift/thrift/transport/TBufferTransports.cpp -o duckdb/third_party/thrift/thrift/transport/TBufferTransports.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/thrift/thrift/transport/TTransportException.cpp -o duckdb/third_party/thrift/thrift/transport/TTransportException.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/utf8proc/utf8proc.cpp -o duckdb/third_party/utf8proc/utf8proc.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/third_party/utf8proc/utf8proc_wrapper.cpp -o duckdb/third_party/utf8proc/utf8proc_wrapper.o
g++ -std=gnu++11 -I"/usr/share/R/include" -DNDEBUG -I. -DDUCKDB_DISABLE_PRINT -Iduckdb/src/include -Iduckdb/third_party/fmt/include -Iduckdb/third_party/re2 -Iduckdb/third_party/miniz -Iduckdb/third_party/utf8proc/include -Iduckdb/third_party/utf8proc -Iduckdb/third_party/libpg_query/include -Iduckdb/third_party/libpg_query -Iduckdb/third_party/concurrentqueue -Iduckdb/extension/parquet/include -Iduckdb/third_party/parquet -Iduckdb/third_party/snappy -Iduckdb/third_party/thrift -Iduckdb    -fpic  -g -O2 -fdebug-prefix-map=/build/r-base-jbaK_j/r-base-3.6.3=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c duckdb/amalgamation-8.cpp -o duckdb/amalgamation-8.o
g++ -std=gnu++11 -shared -L/usr/lib/R/lib -Wl,-Bsymbolic-functions -Wl,-z,relro -o duckdb.so duckdbr.o duckdb/extension/parquet/parquet-extension.o duckdb/extension/parquet/parquet_reader.o duckdb/extension/parquet/parquet_timestamp.o duckdb/extension/parquet/parquet_writer.o duckdb/amalgamation-1.o duckdb/amalgamation-2.o duckdb/amalgamation-3.o duckdb/amalgamation-4.o duckdb/amalgamation-5.o duckdb/amalgamation-6.o duckdb/amalgamation-7.o duckdb/third_party/fmt/format.o duckdb/third_party/libpg_query/pg_functions.o duckdb/third_party/libpg_query/postgres_parser.o duckdb/third_party/libpg_query/src_backend_nodes_list.o duckdb/third_party/libpg_query/src_backend_nodes_makefuncs.o duckdb/third_party/libpg_query/src_backend_nodes_value.o duckdb/third_party/libpg_query/src_backend_parser_gram.o duckdb/third_party/libpg_query/src_backend_parser_parser.o duckdb/third_party/libpg_query/src_backend_parser_scan.o duckdb/third_party/libpg_query/src_backend_parser_scansup.o duckdb/third_party/libpg_query/src_common_keywords.o duckdb/third_party/miniz/miniz.o duckdb/third_party/parquet/parquet_constants.o duckdb/third_party/parquet/parquet_types.o duckdb/third_party/re2/re2/bitstate.o duckdb/third_party/re2/re2/compile.o duckdb/third_party/re2/re2/dfa.o duckdb/third_party/re2/re2/filtered_re2.o duckdb/third_party/re2/re2/mimics_pcre.o duckdb/third_party/re2/re2/nfa.o duckdb/third_party/re2/re2/onepass.o duckdb/third_party/re2/re2/parse.o duckdb/third_party/re2/re2/perl_groups.o duckdb/third_party/re2/re2/prefilter.o duckdb/third_party/re2/re2/prefilter_tree.o duckdb/third_party/re2/re2/prog.o duckdb/third_party/re2/re2/re2.o duckdb/third_party/re2/re2/regexp.o duckdb/third_party/re2/re2/set.o duckdb/third_party/re2/re2/simplify.o duckdb/third_party/re2/re2/stringpiece.o duckdb/third_party/re2/re2/tostring.o duckdb/third_party/re2/re2/unicode_casefold.o duckdb/third_party/re2/re2/unicode_groups.o duckdb/third_party/re2/util/rune.o duckdb/third_party/re2/util/strutil.o duckdb/third_party/snappy/snappy-sinksource.o duckdb/third_party/snappy/snappy.o duckdb/third_party/thrift/thrift/protocol/TProtocol.o duckdb/third_party/thrift/thrift/transport/TBufferTransports.o duckdb/third_party/thrift/thrift/transport/TTransportException.o duckdb/third_party/utf8proc/utf8proc.o duckdb/third_party/utf8proc/utf8proc_wrapper.o duckdb/amalgamation-8.o -L/usr/lib/R/lib -lR
installing to /tmp/workdir/duckdb/old/duckdb.Rcheck/00LOCK-duckdb/00new/duckdb/libs
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (duckdb)

```
# parsemsf

<details>

* Version: 0.1.1
* GitHub: https://github.com/benjaminjack/parsemsf
* Source code: https://github.com/cran/parsemsf
* Date/Publication: 2017-12-09 22:00:10 UTC
* Number of recursive dependencies: 79

Run `cloud_details(, "parsemsf")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘parsemsf-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: make_area_table
    > ### Title: Make a table of peptide areas
    > ### Aliases: make_area_table
    > 
    > ### ** Examples
    > 
    > make_area_table(parsemsf_example("test_db.msf"))
    Warning: `select_()` is deprecated as of dplyr 0.7.0.
    Please use `select()` instead.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Error in UseMethod("select_") : 
      no applicable method for 'select_' applied to an object of class "c('tbl_SQLiteConnection', 'tbl_dbi', 'tbl_sql', 'tbl_lazy', 'tbl')"
    Calls: make_area_table ... _fseq -> freduce -> withVisible -> <Anonymous> -> select_
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        7.           └─magrittr::freduce(value, `_function_list`)
        8.             ├─base::withVisible(function_list[[k]](value))
        9.             └─function_list[[k]](value)
       10.               └─dplyr::select_(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      Warning (test_make_area_table.R:16:3): make_area_table creates a data frame with the correct column names
      ERROR (test_make_area_table.R:16:3): make_area_table creates a data frame with the correct column names
      ERROR (test_make_pep_table.R:13:3): make_pep_table creates a data frame with the correct column names
      ERROR (test_map_peptides.R:16:3): map_peptides creates a data frame with the correct column names
      ERROR (???): (code run outside of `test_that()`)
      
      [ FAIL 4 | WARN 1 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

# pool

<details>

* Version: 0.1.4.3
* GitHub: https://github.com/rstudio/pool
* Source code: https://github.com/cran/pool
* Date/Publication: 2019-10-03 11:30:02 UTC
* Number of recursive dependencies: 51

Run `cloud_details(, "pool")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +   poolClose(pool)
    + 
    + } else {
    +   message("Please install the 'RSQLite' package to run this example")
    + }
    
    Attaching package: ‘dplyr’
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    Error in UseMethod("db_list_tables") : 
      no applicable method for 'db_list_tables' applied to an object of class "c('SQLiteConnection', 'DBIConnection', 'DBIObject')"
    Calls: db_list_tables -> db_list_tables.Pool -> db_list_tables
    Execution halted
    Warning in (function (e)  : You have a leaked pooled object.
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          █
       1. ├─testthat::expect_true(db_has_table(pool, "flights")) test-dplyr.R:39:6
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─dplyr::db_has_table(pool, "flights")
       5. └─pool:::db_has_table.Pool(pool, "flights")
       6.   └─dplyr::db_has_table(db_con, table = table)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      ERROR (test-dplyr.R:39:7): pool package: can use dplyr syntax to copy table to DB
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 234 ]
      Error: Test failures
      Execution halted
    ```

# RPresto

<details>

* Version: 1.3.4
* GitHub: https://github.com/prestodb/RPresto
* Source code: https://github.com/cran/RPresto
* Date/Publication: 2019-10-18 17:40:03 UTC
* Number of recursive dependencies: 50

Run `cloud_details(, "RPresto")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Reason: Cannot set locale to tr_TR.iso8859-9it is set at: C.UTF-8
      
      ── Skip (???): `[[` works for dynamic indices ──────────────────────────────────
      Reason: Cannot set locale to tr_TR.iso8859-9it is set at: C.UTF-8
      
      ── Skipped tests  ──────────────────────────────────────────────────────────────
      ● Cannot set locale to tr_TR.iso8859-9it is set at: C.UTF-8 (14)
      ● On CRAN (26)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      ERROR (???): db_query_fields works with mock
      
      [ FAIL 1 | WARN 0 | SKIP 40 | PASS 229 ]
      Error: Test failures
      Execution halted
    ```

