# grasp2db

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/grasp2db
* Number of recursive dependencies: 184

Run `revdep_details(,"grasp2db")` for more info

</details>

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

# MonetDBLite

<details>

* Version: 0.6.0
* Source code: https://github.com/cran/MonetDBLite
* URL: https://github.com/hannesmuehleisen/MonetDBLite-R
* BugReports: https://github.com/hannesmuehleisen/MonetDBLite-R/issues
* Date/Publication: 2018-07-27 09:40:03 UTC
* Number of recursive dependencies: 75

Run `revdep_details(,"MonetDBLite")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

## Newly fixed

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      8: dbplyr::sql(dbplyr::build_sql("SELECT * FROM (", dbplyr::sql_render(x, x$src$con), ") AS s SAMPLE ", 
             as.integer(size))) at /Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/MonetDBLite/old/MonetDBLite.Rcheck/00_pkg_src/MonetDBLite/R/dplyr.R:48
      9: c_character(...)
      10: dbplyr::build_sql("SELECT * FROM (", dbplyr::sql_render(x, x$src$con), ") AS s SAMPLE ", as.integer(size)) at /Users/hadley/Documents/dplyr/dbplyr/revdep/checks.noindex/MonetDBLite/old/MonetDBLite.Rcheck/00_pkg_src/MonetDBLite/R/dplyr.R:48
      11: stop("`con` must not be NULL", call. = FALSE)
      
      [1] "E"
      [1] "H"
      [1] "M"
      ══ testthat results  ════════════════════════════════════════════════════════════════════════
      OK: 297 SKIPPED: 13 WARNINGS: 11 FAILED: 1
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

