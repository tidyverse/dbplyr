# bigrquery

Version: 1.1.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      [31m──[39m [31m3. Failure: %||% translates to IFNULL (@test-dplyr.R#131) [39m [31m────────────────────────────────[39m
      sql$select[[2]] not equal to "IFNULL(`x`, 2) AS `y`".
      1/1 mismatches
      x[1]: "IFNULL(`x`, 2)"
      y[1]: "IFNULL(`x`, 2) AS `y`"
      
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════
      OK: 242 SKIPPED: 0 FAILED: 3
      1. Failure: casting uses bigquery types (@test-dplyr.R#120) 
      2. Failure: casting uses bigquery types (@test-dplyr.R#121) 
      3. Failure: %||% translates to IFNULL (@test-dplyr.R#131) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# BiocOncoTK

Version: 1.2.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Loading required package: BiocParallel
      
      Attaching package: 'DelayedArray'
      
      The following objects are masked from 'package:matrixStats':
      
          colMaxs, colMins, colRanges, rowMaxs, rowMins, rowRanges
      
      The following objects are masked from 'package:base':
      
          aperm, apply
      
      Error: package or namespace load failed for 'restfulSE' in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
       there is no package called 'GO.db'
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 18-30 (BiocOncoTK.Rmd) 
    Error: processing vignette 'BiocOncoTK.Rmd' failed with diagnostics:
    there is no package called 'org.Hs.eg.db'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘org.Hs.eg.db’ ‘TxDb.Hsapiens.UCSC.hg19.knownGene’
      ‘TxDb.Hsapiens.UCSC.hg18.knownGene’ ‘FDb.InfiniumMethylation.hg19’
      ‘EnsDb.Hsapiens.v75’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        data   4.0Mb
        doc    1.9Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Package listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘DBI’
    A package should be listed in only one of these fields.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/mc3utils.R:118)
    ggFeatureSegs: no visible binding for global variable ‘symbol’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/mc3utils.R:124-126)
    ggMutDens : <anonymous>: no visible binding for global variable
      ‘Consequence’
    ggMutDens: no visible binding for global variable ‘project_short_name’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/mc3utils.R:67-68)
    ggMutDens: no visible binding for global variable ‘project_short_name’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/mc3utils.R:70)
    ggMutDens: no visible binding for global variable ‘project_short_name’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/mc3utils.R:71-73)
    mc3toGR : <anonymous>: no visible binding for global variable
      ‘Consequence’
    rainfall: no visible global function definition for ‘genome’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/rainfall3.R:152)
    tumNorSet : <anonymous>: no visible global function definition for
      ‘pancan_SE’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/pancan.R:132-134)
    Undefined global functions or variables:
      BiocFileCache Consequence genes genome pancan_SE project_short_name
      seqlengths symbol tfstart
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 46 marked UTF-8 strings
    ```

# childesr

Version: 0.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 42-44 (access_childes_db.Rmd) 
    Error: processing vignette 'access_childes_db.Rmd' failed with diagnostics:
    Failed to connect to database: Error: Can't connect to MySQL server on 'ec2-54-68-171-132.us-west-2.compute.amazonaws.com' (36)
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

# civis

Version: 1.6.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        R      3.2Mb
        help   2.5Mb
    ```

# cytominer

Version: 0.1.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      15: db_collect.DBIConnection(x$src$con, sql, n = n, warn_incomplete = warn_incomplete) at /private/tmp/RtmpDJWeoM/R.INSTALL150ff7d3d7092/dbplyr/R/verb-compute.R:114
      16: dbSendQuery(con, sql) at /private/tmp/RtmpDJWeoM/R.INSTALL150ff7d3d7092/dbplyr/R/verb-compute.R:119
      17: dbSendQuery(con, sql)
      18: .local(conn, statement, ...)
      19: new("SQLiteResult", sql = statement, ptr = result_create(conn@ptr, statement), conn = conn, bigint = conn@bigint)
      20: initialize(value, ...)
      21: initialize(value, ...)
      22: result_create(conn@ptr, statement)
      
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════
      OK: 41 SKIPPED: 0 FAILED: 1
      1. Error: `aggregate` aggregates data (@test-aggregate.R#15) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# dlookr

Version: 0.3.8

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        doc   4.1Mb
    ```

# dplyr

Version: 0.8.0.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        R      2.0Mb
        libs   3.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# grasp2db

Version: 1.1.0

## In both

*   R CMD check timed out
    

*   checking for missing documentation entries ... WARNING
    ```
    Undocumented code objects:
      ‘GRASP2’ ‘checkAnti’ ‘getJoinCompatible’
    Undocumented data sets:
      ‘mml10p_nox’ ‘uniqueGexNames2.0’ ‘uniquePPDnames2.0’
    All user-level objects in a package should have documentation entries.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking data for non-ASCII characters ... WARNING
    ```
      Warning: found non-ASCII string
      'Beh<e7>et's disease' in object 'uniquePPDnames2.0'
    ```

*   checking data for ASCII and uncompressed saves ... WARNING
    ```
      
      Note: significantly better compression could be obtained
            by using R CMD build --resave-data
                            old_size new_size compress
      mml10p_nox.rda           7.1Mb    2.8Mb       xz
      uniquePPDnames2.0.rda     17Kb     15Kb    bzip2
    ```

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘AnnotationHubData’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        data   7.1Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    License components with restrictions not permitted:
      Artistic-2.0 + file LICENSE
    ```

*   checking R code for possible problems ... NOTE
    ```
    .grasp2ToAnnotationHub: no visible global function definition for
      ‘outputFile’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/grasp2db/new/grasp2db.Rcheck/00_pkg_src/grasp2db/R/db_AnnotationHub.R:39)
    .grasp2ToAnnotationHub: no visible global function definition for
      ‘outputFile’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/grasp2db/new/grasp2db.Rcheck/00_pkg_src/grasp2db/R/db_AnnotationHub.R:40)
    checkAnti: no visible binding for global variable ‘chr_hg19’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/grasp2db/new/grasp2db.Rcheck/00_pkg_src/grasp2db/R/doanti.R:19-20)
    getJoinCompatible: no visible binding for global variable ‘gwrngs19’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/grasp2db/new/grasp2db.Rcheck/00_pkg_src/grasp2db/R/doanti.R:7)
    Undefined global functions or variables:
      chr_hg19 gwrngs19 outputFile
    ```

# hydrolinks

Version: 0.10.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

# implyr

Version: 0.2.4

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      The following objects are masked from 'package:base':
      
          intersect, setdiff, setequal, union
      
      > library(RJDBC)
      Loading required package: rJava
      Error: package or namespace load failed for 'rJava':
       .onLoad failed in loadNamespace() for 'rJava', details:
        call: dyn.load(file, DLLpath = DLLpath, ...)
        error: unable to load shared object '/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/implyr/rJava/libs/rJava.so':
        dlopen(/Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/implyr/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
        Referenced from: /Users/hadley/Documents/dplyr/dbplyr/revdep/library.noindex/implyr/rJava/libs/rJava.so
        Reason: image not found
      Error: package 'rJava' could not be loaded
      Execution halted
    ```

# infuser

Version: 0.2.8

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 157-158 (getting_started.Rmd) 
    Error: processing vignette 'getting_started.Rmd' failed with diagnostics:
    `con` must not be NULL
    Execution halted
    ```

# ipumsr

Version: 0.4.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘R6’
      All declared Imports should be used.
    ```

# mdsr

Version: 0.1.6

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘mdsr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: src_scidb
    > ### Title: src_scidb
    > ### Aliases: src_scidb dbConnect_scidb mysql_scidb
    > 
    > ### ** Examples
    > 
    > 
    > dbAir <- src_scidb("airlines")
    Error in .local(drv, ...) : 
      Failed to connect to database: Error: Can't connect to MySQL server on 'mdsr.cdc7tgkkqd0n.us-east-1.rds.amazonaws.com' (36)
    Calls: src_scidb ... dbConnect_scidb -> <Anonymous> -> <Anonymous> -> .local -> .Call
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      2: dbplyr::src_dbi(dbConnect_scidb(dbname, ...)) at /Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/mdsr/new/mdsr.Rcheck/00_pkg_src/mdsr/R/scidb.R:24
      3: paste0("src_", class(con)[[1]]) at /private/tmp/Rtmp3b7dcu/R.INSTALL5d923c9e52e5/dbplyr/R/src_dbi.R:102
      4: dbConnect_scidb(dbname, ...) at /Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/mdsr/new/mdsr.Rcheck/00_pkg_src/mdsr/R/scidb.R:24
      5: DBI::dbConnect(RMySQL::MySQL(), dbname = dbname, host = "mdsr.cdc7tgkkqd0n.us-east-1.rds.amazonaws.com", 
             user = "mdsr_public", password = "ImhsmflMDSwR") at /Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/mdsr/new/mdsr.Rcheck/00_pkg_src/mdsr/R/scidb.R:42
      6: DBI::dbConnect(RMySQL::MySQL(), dbname = dbname, host = "mdsr.cdc7tgkkqd0n.us-east-1.rds.amazonaws.com", 
             user = "mdsr_public", password = "ImhsmflMDSwR")
      7: .local(drv, ...)
      
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════
      OK: 0 SKIPPED: 0 FAILED: 1
      1. Error: scidb works (@tests.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        data   5.5Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2694 marked UTF-8 strings
    ```

# metagenomeFeatures

Version: 2.2.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.1Mb
      sub-directories of 1Mb or more:
        doc       2.9Mb
        extdata   3.5Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Package in Depends field not imported from: ‘Biobase’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    .select.taxa: no visible binding for global variable ‘Keys’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/metagenomeFeatures/new/metagenomeFeatures.Rcheck/00_pkg_src/metagenomeFeatures/R/mgDb_method_select.R:43)
    Undefined global functions or variables:
      Keys
    ```

# mlbgameday

Version: 0.1.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘doParallel’ ‘iterators’ ‘parallel’
      All declared Imports should be used.
    ```

# MonetDBLite

Version: 0.6.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      8: dbplyr::sql(dbplyr::build_sql("SELECT * FROM (", dbplyr::sql_render(x, x$src$con), ") AS s SAMPLE ", 
             as.integer(size))) at /Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/MonetDBLite/new/MonetDBLite.Rcheck/00_pkg_src/MonetDBLite/R/dplyr.R:48
      9: c_character(...) at /private/tmp/RtmpDJWeoM/R.INSTALL150ff7d3d7092/dbplyr/R/sql.R:11
      10: dbplyr::build_sql("SELECT * FROM (", dbplyr::sql_render(x, x$src$con), ") AS s SAMPLE ", as.integer(size)) at /Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/MonetDBLite/new/MonetDBLite.Rcheck/00_pkg_src/MonetDBLite/R/dplyr.R:48
      11: stop("`con` must not be NULL", call. = FALSE) at /private/tmp/RtmpDJWeoM/R.INSTALL150ff7d3d7092/dbplyr/R/build-sql.R:34
      
      [1] "E"
      [1] "H"
      [1] "M"
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════
      OK: 308 SKIPPED: 13 FAILED: 1
      1. Error: sample works (@test_04_dplyr.R#41) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        libs   5.4Mb
    ```

# mudata2

Version: 1.0.5

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      37: purrr::map_chr(enexprs(...), escape_expr, con = con) at /private/tmp/RtmpDJWeoM/R.INSTALL150ff7d3d7092/dbplyr/R/build-sql.R:48
      38: .f(.x[[i]], ...)
      39: escape(val, con = con) at /private/tmp/RtmpDJWeoM/R.INSTALL150ff7d3d7092/dbplyr/R/build-sql.R:45
      
      OGR: Unsupported geometry type
      OGR: Unsupported geometry type
      OGR: Unsupported geometry type
      OGR: Unsupported geometry type
      OGR: Unsupported geometry type
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════
      OK: 967 SKIPPED: 0 FAILED: 1
      1. Error: long_pairs works with sqlite sources (@test_mudata_remote.R#141) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# Organism.dplyr

Version: 1.10.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [32] 95456 - 95461 == -5
      [33] 95456 - 95461 == -5
      [34] 95456 - 95461 == -5
      [35] 95456 - 95461 == -5
      [47] 95461 - 95456 ==  5
      ...
      
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════
      OK: 200 SKIPPED: 0 FAILED: 1
      1. Failure: select (@test-src_organism-select.R#44) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘org.Hs.eg.db’ ‘org.Mm.eg.db’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘AnnotationDbi:::smartKeys’ ‘GenomicFeatures:::.exons_with_3utr’
      ‘GenomicFeatures:::.exons_with_5utr’
      ‘GenomicFeatures:::get_TxDb_seqinfo0’
      ‘S4Vectors:::extract_data_frame_rows’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    .toGRanges: no visible binding for global variable ‘.’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/extractors.R:236)
    intronsByTranscript,src_organism: no visible binding for global
      variable ‘.’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/extractor-methods.R:254-255)
    intronsByTranscript,src_organism: no visible binding for global
      variable ‘.’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/extractor-methods.R:264-265)
    orgPackageName,src_organism: no visible binding for global variable
      ‘name’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/src.R:434-435)
    orgPackageName,src_organism: no visible binding for global variable
      ‘organism’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/src.R:436)
    orgPackageName,src_organism: no visible binding for global variable
      ‘OrgDb’
      (/Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/Organism.dplyr/new/Organism.dplyr.Rcheck/00_pkg_src/Organism.dplyr/R/src.R:436)
    Undefined global functions or variables:
      . OrgDb name organism
    ```

# parsemsf

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

# pivot

Version: 18.4.17

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    > # establish db as a database connection
    > ## Don't show: 
    >    con <- simulate_mssql()
    >    src <- src_dbi(con)
    >    base <- list( x = ident('##iris')
    +                , vars  = tbl_vars(iris)
    +                ) %>% structure(class=c('op_base_remote', 'op_base', 'op'))
    >    db_iris <- structure( list( src = src
    +                              , ops = base
    +                              )
    +        , class = c('tbl_dbi', 'tbl_sql', 'tbl_lazy', 'tbl'))
    > ## End(Don't show)
    > ## Not run: 
    > ##D db_iris <- copy_to(db, iris)
    > ## End(Not run)
    > result <- pivot( db_iris, Species, mean(Petal.Length, na.rm=TRUE)
    +                , setosa, versicolor, virginica)
    > sql_render(result)
    Error: `con` must not be NULL
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      25: .abort(text)
      
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════
      OK: 65 SKIPPED: 1 FAILED: 8
      1. Error: PIVOT construction (@test-pivot.R#44) 
      2. Error: PIVOT warnings and errors (@test-pivot.R#71) 
      3. Error: spread.tbl_lazy (@test-tidyr.R#8) 
      4. Failure: UNPIVOT construction (@test-unpivot.R#45) 
      5. Failure: order_by (@test-unpivot.R#81) 
      6. Failure: find_connection (@test-utils.R#11) 
      7. Failure: find_connection (@test-utils.R#12) 
      8. Error: get_pivot_levels (@test-utils.R#29) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘odbc’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘colorspace’ ‘lubridate’
      All declared Imports should be used.
    ```

# pool

Version: 0.1.4.2

## In both

*   checking examples ... ERROR
    ```
    ...
    +     username = "guest",
    +     password = "guest"
    +   )
    + 
    +   dbGetQuery(pool, "SELECT * from City LIMIT 5;")
    +   #>   ID           Name CountryCode      District Population
    +   #> 1  1          Kabul         AFG         Kabol    1780000
    +   #> 2  2       Qandahar         AFG      Qandahar     237500
    +   #> 3  3          Herat         AFG         Herat     186800
    +   #> 4  4 Mazar-e-Sharif         AFG         Balkh     127800
    +   #> 5  5      Amsterdam         NLD Noord-Holland     731200
    + 
    +   poolClose(pool)
    + 
    + } else {
    +   message("Please install the 'RMySQL' package to run this example")
    + }
    Error in .local(drv, ...) : 
      Failed to connect to database: Error: Can't connect to MySQL server on 'shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com' (36)
    Calls: dbPool ... do.call -> <Anonymous> -> <Anonymous> -> .local -> .Call
    Execution halted
    ```

# RPresto

Version: 1.3.2

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      7: stop("`con` must not be NULL", call. = FALSE) at /private/tmp/RtmpDJWeoM/R.INSTALL150ff7d3d7092/dbplyr/R/build-sql.R:34
      
      [31m--[39m [31m2. Failure: as() works (@test-translate_sql.R#26) [39m [31m----------------------------------------[39m
      translate_sql(pmax(x), con = s[["con"]]) not equal to dplyr::sql("GREATEST(\"x\")").
      1/1 mismatches
      x[1]: "greatest(\"x\")"
      y[1]: "GREATEST(\"x\")"
      
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════
      OK: 278 SKIPPED: 28 FAILED: 2
      1. Error: db_explain works with mock (@test-db_explain.R#26) 
      2. Failure: as() works (@test-translate_sql.R#26) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# sparklyr

Version: 1.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        R      4.1Mb
        java   1.5Mb
    ```

# sqlscore

Version: 0.1.3

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sqlscore-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: create_statement
    > ### Title: Generate a CREATE TABLE statement from a model
    > ### Aliases: create_statement
    > 
    > ### ** Examples
    > 
    > # Basic create statements
    > mod <- glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
    +            data=datasets::iris)
    > create_statement(mod, src_table="tbl_name", dest_table="target_tbl")
    Error: `con` must not be NULL
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
             }
             pieces <- purrr::map_chr(enexprs(...), escape_expr, con = con)
             sql(paste0(pieces, collapse = ""))
         })("\"quux\"", ".", "\"baz\"", ".", "\"foo bar\"")
      4: stop("`con` must not be NULL", call. = FALSE) at /private/tmp/RtmpDJWeoM/R.INSTALL150ff7d3d7092/dbplyr/R/build-sql.R:34
      
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════
      OK: 80 SKIPPED: 0 FAILED: 4
      1. Error: (unknown) (@test-create_statement.R#9) 
      2. Error: Basic table names are escaped (@test-fqtn.R#4) 
      3. Error: More complicated names are escaped (@test-fqtn.R#10) 
      4. Error: (unknown) (@test-select_statement.R#9) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tidyverse

Version: 1.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dbplyr’ ‘reprex’ ‘rlang’
      All declared Imports should be used.
    ```

# wordbankr

Version: 0.3.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 31-33 (wordbankr.Rmd) 
    Error: processing vignette 'wordbankr.Rmd' failed with diagnostics:
    Failed to connect to database: Error: Can't connect to MySQL server on 'server.wordbank.stanford.edu' (36)
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

