# msPurity

<details>

* Version: 1.22.0
* GitHub: https://github.com/computational-metabolomics/msPurity
* Source code: https://github.com/cran/msPurity
* Date/Publication: 2022-04-26
* Number of recursive dependencies: 160

Run `revdep_details(, "msPurity")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘msPurity-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: purityX
    > ### Title: Assessing anticipated purity of XCMS features from an LC-MS run
    > ### Aliases: purityX
    > 
    > ### ** Examples
    > 
    > msPths <- list.files(system.file("extdata", "lcms", "mzML",
    ...
    > xset <- xcms::retcor(xset)
    Performing retention time correction using 763 peak groups.
    > xset <- xcms::group(xset)
    Processing 3179 mz slices ... OK
    > ppLCMS <- purityX(xset, cores = 1, xgroups = c(1, 2))
    Sizes of mz and intensity arrays don't match.
    Error in object@backend$getPeakList(scans) : 
      dims [product 4516] do not match the length of object [2258]
    Calls: purityX ... <Anonymous> -> .local -> .peaks -> <Anonymous> -> .External
    Execution halted
    ```

## In both

*   R CMD check timed out
    

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 23.0Mb
      sub-directories of 1Mb or more:
        doc       3.5Mb
        extdata  18.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Package in Depends field not imported from: ‘Rcpp’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    package 'methods' is used but not declared
    ```

*   checking R code for possible problems ... NOTE
    ```
    addGenericMS1LookupResults: no visible global function definition for
      ‘count.fields’
    addMetFragResults: no visible global function definition for
      ‘count.fields’
    addSiriusResults: no visible global function definition for
      ‘count.fields’
    assessPuritySingle: no visible binding for global variable ‘parallel’
    combineAnnotations: no visible binding for global variable
      ‘compoundDbname’
    createDatabase: no visible global function definition for
    ...
      PeakDensityParam accession alli chromPeaks chromPeaks<-
      compoundDbname count.fields featureValues grpid i idx inPurity
      instrument instrument_type l_speakmetaFiltered
      library_spectra_meta_id lower match_factor mtch mtchi name.y parallel
      pass_flag phenoData pid polarity precursor_mz purity ra
      retention_time sampclass<- spectraType spectrum_type topn type
      variable
    Consider adding
      importFrom("utils", "count.fields")
    to your NAMESPACE file.
    ```

