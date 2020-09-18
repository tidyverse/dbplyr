# parsemsf

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/parsemsf
* URL: https://github.com/benjaminjack/parsemsf/
* BugReports: https://github.com/benjaminjack/parsemsf/issues
* Date/Publication: 2017-12-09 22:00:10 UTC
* Number of recursive dependencies: 73

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
        1. parsemsf::make_area_table(parsemsf_example("test_db.msf"))
        2. dplyr::tbl(my_db, "Events")
       10. dplyr::select_(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 0 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 4 ]
      1. Error: make_area_table creates a data frame with the correct column names (@test_make_area_table.R#16) 
      2. Error: make_pep_table creates a data frame with the correct column names (@test_make_pep_table.R#13) 
      3. Error: map_peptides creates a data frame with the correct column names (@test_map_peptides.R#16) 
      4. Error: (unknown) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

