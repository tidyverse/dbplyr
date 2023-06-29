# BiocOncoTK

<details>

* Version: 1.18.0
* GitHub: NA
* Source code: https://github.com/cran/BiocOncoTK
* Date/Publication: 2022-11-01
* Number of recursive dependencies: 214

Run `revdep_details(, "BiocOncoTK")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      file.exists("BLCA_CD8A.csv") is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      ── Error ('test_dockstore_scripts.R:11'): exprByMSI_csv produces expected CSV ──
      Error in `file(file, "rt")`: cannot open the connection
      Backtrace:
          ▆
       1. └─utils::read.csv("BLCA_CD8A.csv") at test_dockstore_scripts.R:11:1
       2.   └─utils::read.table(...)
       3.     └─base::file(file, "rt")
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 2 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking whether package ‘BiocOncoTK’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘GenomicRanges::subtract’ by ‘magrittr::subtract’ when loading ‘BiocOncoTK’
    See ‘/Users/mgirlich/GitHub/dbplyr/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘FDb.InfiniumMethylation.hg19’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 11.6Mb
      sub-directories of 1Mb or more:
        data        3.1Mb
        doc         3.6Mb
        pamphlets   4.5Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ggscat_av: warning in png(file = tempfile()): partial argument match of
      'file' to 'filename'
    .flexbi: no visible binding for global variable ‘v1’
    .flexbi: no visible binding for global variable ‘v2’
    .rainfall.bq.df: no visible global function definition for ‘seqlengths’
    .rainfall.maeGRL.df: no visible global function definition for ‘genome’
    .rainfall.maeGRL.df: no visible global function definition for
      ‘seqlengths’
    acronym_to_system: no visible binding for global variable
      ‘map_tcga_ncit’
    ...
    tumNorSet : <anonymous>: no visible global function definition for
      ‘pancan_SE’
    Undefined global functions or variables:
      BiocFileCache Consequence acronym genome log2ex log2exa mapIds
      map_tcga_ncit msicode msival new pancan_SE project_short_name
      right_join seqlengths symbol tfstart tmsi v1 v2
    Consider adding
      importFrom("methods", "new")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Error loading dataset 'brcaMAE':
       Error in get_Nindex_lengths(x@index, dim(x@seed)) : 
        length(Nindex) == length(dim) is not TRUE
      
      Note: found 46 marked UTF-8 strings
    ```

